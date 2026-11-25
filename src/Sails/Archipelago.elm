module Sails.Archipelago exposing
    ( Archipelago
    , Route
    , courseFor
    , defaultCourse
    , defaultMap
    , generate
    , harbourExemption
    , legClearance
    , legs
    , lengthRange
    , maxAttempts
    , maxTurn
    , minIslandGap
    , routeCount
    , routePoints
    , seatCourses
    , turns
    )

{-| The archipelago of a map: the islands, the harbours, the routes round
them, the treasure, the targets, the towns and the decoration on the water,
all made from the map's seed (`generate`), so one map code gives the same
archipelago on every device and in every seat of a room.

The layout: the start at the origin; the `Hub` island 90-110 m ahead,
with the fort or lighthouse and the final harbour on its shore facing the
start; four `HarbourIsle`s in a ring round it, one ahead, one either side
and one behind the start; a small `TreasureIsle` beside a leg; and a few
`Skerry` rocks out of the way.

A route calls at the four harbour islands round the ring, clockwise or
anticlockwise, calls at the hub once on the way (before the first, second
or third of them), and ends at the final harbour. Up to three routes are
kept, and seat `i` of a room sails route `i` modulo their number (see
`courseFor`). A route is kept only when:

  - every leg passes at least `legClearance` (8 m) off the shore of every
    island, except for the `harbourExemption` (12 m) into and out of a
    harbour on its own island's shore;
  - the turn at every harbour is at most `maxTurn` (150 degrees), so the
    boat never swings back onto the rocks;
  - it is 850 to 1050 m long (`lengthRange`);
  - three legs between harbours have room for a pirate patrol (see
    `Sails.Pirates.patrolRoutes`).

When no route passes, the generator repairs the layout: it turns the docks
15 degrees either way, then shrinks the island that blocked the most legs,
and then starts again with the next attempt's seed, at most `maxAttempts`
times. The archipelago tests sweep hundreds of maps to check that this is
always enough.

-}

import Game.MapParams as MapParams exposing (MapParams)
import Math.Vector3 exposing (vec3)
import Random
import Sails.Types
    exposing
        ( Course
        , Decor
        , DecorKind(..)
        , Faction(..)
        , HarbourKind(..)
        , Island
        , IslandRole(..)
        , Town
        , Waypoint
        , Wind
        )
import Sails.World as World



-- TYPES


{-| One order of the harbours: the islands they dock on, with the compass
bearing of each dock from its island's centre, before the final harbour on
the hub (`finalBearing`); the heading the boat starts on (towards the
first harbour); the treasure island and where on its shore the chest lies;
the floating targets beside the legs; and the route's length in metres.
-}
type alias Route =
    { harbours : List ( Island, Float )
    , finalBearing : Float
    , startHeading : Float
    , treasureBearing : Float
    , treasure : Island
    , targets : List ( Float, Float )
    , lengthM : Float
    }


{-| A generated archipelago. `attempts` is how many layouts were tried (1
when the first one worked); `valid` is False only if every attempt failed
and the fallback layout was used, which the seed sweep tests rule out.
-}
type alias Archipelago =
    { map : MapParams
    , islands : List Island
    , hub : Island
    , harbourIsles : List Island
    , routes : List Route
    , towns : List Town
    , decor : List Decor
    , wind : Wind
    , arena : { x : Float, z : Float, radius : Float }
    , attempts : Int
    , valid : Bool
    }



-- THE FIXED MAP


{-| A fixed map for tests: the desert coast in a summer storm, seed 1, the
closest map to the old fixed course. The game itself starts from a link's
map, the map sailed last, or every pick left to chance (see `Main`).
-}
defaultMap : MapParams
defaultMap =
    { biome = MapParams.Desert, season = MapParams.Summer, weather = MapParams.Storm, seed = 1 }


{-| The first seat's course of `defaultMap`.
-}
defaultCourse : Course
defaultCourse =
    courseFor (generate defaultMap) 0



-- RULES


{-| How far every leg passes off every island's shore, in metres.
-}
legClearance : Float
legClearance =
    8


{-| The length of a leg, at either end, that may pass closer to the island
the harbour at that end docks on.
-}
harbourExemption : Float
harbourExemption =
    12


{-| The sharpest turn at a harbour, in degrees.
-}
maxTurn : Float
maxTurn =
    150


{-| The shortest and longest route, in metres.
-}
lengthRange : ( Float, Float )
lengthRange =
    ( 850, 1050 )


{-| The smallest gap between two islands' shores, in metres.
-}
minIslandGap : Float
minIslandGap =
    30


{-| How many layouts `generate` tries before it gives up.
-}
maxAttempts : Int
maxAttempts =
    12


{-| How many seat routes an archipelago has (1 to 3).
-}
routeCount : Archipelago -> Int
routeCount arch =
    List.length arch.routes



-- GEOMETRY


type alias Point =
    ( Float, Float )


{-| A point on an island's shore line at a compass bearing, `out` metres
off the water's edge.
-}
offShore : Island -> Float -> Float -> Point
offShore island bearing out =
    ( island.x + sin (degrees bearing) * (island.radius + out)
    , island.z + cos (degrees bearing) * (island.radius + out)
    )


{-| Where a harbour lies: 3 m off its island's shore (as
`Sails.Physics.harbour`).
-}
dockPoint : Island -> Float -> Point
dockPoint island bearing =
    offShore island bearing 3


bearingTo : Point -> Point -> Float
bearingTo ( ax, az ) ( bx, bz ) =
    180 / pi * atan2 (bx - ax) (bz - az)


dist : Point -> Point -> Float
dist ( ax, az ) ( bx, bz ) =
    sqrt ((bx - ax) ^ 2 + (bz - az) ^ 2)


angleDiff : Float -> Float -> Float
angleDiff a b =
    let
        d =
            a - b - 360 * toFloat (floor ((a - b) / 360))
    in
    if d > 180 then
        d - 360

    else
        d


{-| The mean of two compass bearings (the one between them on the short
side).
-}
meanBearing : Float -> Float -> Float
meanBearing a b =
    b + angleDiff a b / 2


centreOf : Island -> Point
centreOf island =
    ( island.x, island.z )


{-| The nearest distance from `p` to the part of segment a-b that runs from
`s0` to `s1` metres along it.
-}
segmentDistance : Point -> Point -> Float -> Float -> Point -> Float
segmentDistance ( ax, az ) ( bx, bz ) s0 s1 ( px, pz ) =
    let
        ex =
            bx - ax

        ez =
            bz - az

        len =
            sqrt (ex * ex + ez * ez)
    in
    if len < 1.0e-6 then
        dist ( ax, az ) ( px, pz )

    else
        let
            t =
                clamp (s0 / len) (s1 / len) (((px - ax) * ex + (pz - az) * ez) / (len * len))
        in
        dist ( ax + ex * t, az + ez * t ) ( px, pz )



-- STOPS AND LEGS


{-| A point of a route: the start, or a harbour docked on an island.
-}
type alias Stop =
    { point : Point
    , island : Maybe Island
    }


stopsOf : Route -> List Stop
stopsOf route =
    { point = ( 0, 0 ), island = Nothing }
        :: List.map (\( isl, b ) -> { point = dockPoint isl b, island = Just isl }) route.harbours


{-| The start and every harbour of a route in order, the final harbour
last, as (x, z).
-}
routePoints : Archipelago -> Route -> List Point
routePoints arch route =
    List.map .point (fullStops arch.hub route)


fullStops : Island -> Route -> List Stop
fullStops hub route =
    stopsOf route ++ [ { point = dockPoint hub route.finalBearing, island = Just hub } ]


pairs : List a -> List ( a, a )
pairs xs =
    List.map2 Tuple.pair xs (List.drop 1 xs)


{-| The legs of a route, as pairs of points.
-}
legs : Archipelago -> Route -> List ( Point, Point )
legs arch route =
    pairs (routePoints arch route)


{-| The turn at each harbour of a route bar the last, in degrees (0 is
straight on).
-}
turns : Archipelago -> Route -> List Float
turns arch route =
    turnsOf (List.map .point (fullStops arch.hub route))


turnsOf : List Point -> List Float
turnsOf points =
    List.map3 (\a b c -> abs (angleDiff (bearingTo b c) (bearingTo a b))) points (List.drop 1 points) (List.drop 2 points)


lengthOf : List Point -> Float
lengthOf points =
    List.sum (List.map (\( a, b ) -> dist a b) (pairs points))


{-| How far a leg between two stops passes off an island's shore, leaving
out the 12 m at an end that docks on that island.
-}
legGap : Stop -> Stop -> Island -> Float
legGap a b island =
    let
        len =
            dist a.point b.point

        own s =
            s.island == Just island

        s0 =
            if own a then
                harbourExemption

            else
                0

        s1 =
            if own b then
                len - harbourExemption

            else
                len
    in
    if s1 <= s0 then
        1.0e9

    else
        segmentDistance a.point b.point s0 s1 (centreOf island) - island.radius



-- RANDOM HELPERS


type alias Gen a =
    Random.Generator a


range : ( Float, Float ) -> Gen Float
range ( lo, hi ) =
    Random.float lo hi



-- LAYOUT


{-| The fixed part of an attempt: the hub, the ring and the docks.
-}
type alias Layout =
    { hub : Island
    , ring : List Island
    , ringDocks : List Float
    }


ringGen : MapParams -> Gen Layout
ringGen map =
    let
        scale =
            World.islandScale map.biome
    in
    Random.map4
        (\theta0 d0 hubR hubH ->
            { theta0 = theta0, d0 = d0, hubR = hubR, hubH = hubH }
        )
        (Random.float 0 360)
        (range ( 90, 110 ))
        (range ( 20, 26 ))
        (range ( 6, 8 ))
        |> Random.andThen
            (\{ theta0, d0, hubR, hubH } ->
                let
                    hub =
                        { x = d0 * sin (degrees theta0)
                        , z = d0 * cos (degrees theta0)
                        , radius = hubR
                        , height = hubH * scale.height
                        , role = Hub
                        }

                    isleGen k =
                        Random.map4
                            (\jitter r h far ->
                                let
                                    radius =
                                        r * scale.radius

                                    -- The island behind the start lies past it.
                                    reach =
                                        if k == 2 then
                                            max far (d0 + radius + 30)

                                        else
                                            far
                                in
                                { angle = theta0 + 90 * toFloat k + jitter, distance = reach, radius = radius, height = h * scale.height }
                            )
                            (range ( -25, 25 ))
                            (range ( 35, 50 ))
                            (range ( 7, 12 ))
                            (range ( 150, 220 ))

                    dockJitter =
                        Random.list 4 (range ( -10, 10 ))
                in
                Random.map2
                    (\isles jitters ->
                        let
                            ring =
                                spaceRing hub isles
                        in
                        { hub = hub, ring = ring, ringDocks = jitters }
                    )
                    (sequence (List.map isleGen [ 0, 1, 2, 3 ]))
                    dockJitter
            )


sequence : List (Gen a) -> Gen (List a)
sequence gens =
    List.foldr (Random.map2 (::)) (Random.constant []) gens


type alias RingIsle =
    { angle : Float, distance : Float, radius : Float, height : Float }


ringIsland : Island -> RingIsle -> Island
ringIsland hub r =
    { x = hub.x + r.distance * sin (degrees r.angle)
    , z = hub.z + r.distance * cos (degrees r.angle)
    , radius = r.radius
    , height = r.height
    , role = HarbourIsle
    }


{-| Push ring islands outward along their own ring rays, 10 m at a time and
at most 5 times, until every gap between the hub and the ring islands is
`minIslandGap` and the start lies well clear of them.
-}
spaceRing : Island -> List RingIsle -> List Island
spaceRing hub isles =
    let
        crowded all k r =
            let
                me =
                    ringIsland hub r

                others =
                    hub
                        :: List.filterMap
                            (\( j, o ) ->
                                if j == k then
                                    Nothing

                                else
                                    Just (ringIsland hub o)
                            )
                            (List.indexedMap Tuple.pair all)
            in
            List.any (\o -> dist (centreOf me) (centreOf o) - me.radius - o.radius < minIslandGap) others
                || dist (centreOf me) ( 0, 0 )
                < me.radius
                + 25

        round_ n all =
            if n == 0 then
                all

            else
                let
                    pushed =
                        List.indexedMap
                            (\k r ->
                                if crowded all k r then
                                    { r | distance = r.distance + 10 }

                                else
                                    r
                            )
                            all
                in
                if pushed == all then
                    all

                else
                    round_ (n - 1) pushed
    in
    List.map (ringIsland hub) (round_ 5 isles)



-- ROUTES


{-| The ring orders to try, best first: clockwise or anticlockwise from
the side the seed picks, with the hub call before the first, second or
third harbour island (index -1 stands for the hub).
-}
candidateOrders : Bool -> List (List Int)
candidateOrders clockwiseFirst =
    let
        cw =
            [ 1, 0, 3, 2 ]

        ccw =
            [ 3, 0, 1, 2 ]

        withHub p order =
            List.take p order ++ (-1 :: List.drop p order)

        ( first, second ) =
            if clockwiseFirst then
                ( cw, ccw )

            else
                ( ccw, cw )
    in
    List.concatMap (\p -> [ withHub p first, withHub p second ]) [ 0, 1, 2 ]


type alias Candidate =
    { harbours : List ( Island, Float )
    , finalBearing : Float
    , stops : List Stop
    , blockers : List Island
    , valid : Bool
    , lengthM : Float
    }


{-| Build the route of one ring order and check it.
-}
candidate : Layout -> List Int -> Candidate
candidate layout order =
    let
        hub =
            layout.hub

        finalBearing =
            bearingTo (centreOf hub) ( 0, 0 )

        ringIsle k =
            case ( List.drop k layout.ring, List.drop k layout.ringDocks ) of
                ( isl :: _, j :: _ ) ->
                    Just ( isl, j )

                _ ->
                    Nothing

        calls =
            List.filterMap
                (\k ->
                    if k < 0 then
                        Just ( hub, 0 )

                    else
                        ringIsle k
                )
                order

        finalPoint =
            dockPoint hub finalBearing

        -- Each harbour docks on the shore between the way in and the way
        -- out (the bisector, turned by the island's small seeded offset).
        -- The first pass aims at the neighbouring islands' centres, the
        -- second at the docks the first pass found. The hub call keeps at
        -- least 50 degrees round the shore from the final harbour.
        dockAll approx =
            let
                prevs =
                    ( 0, 0 ) :: approx

                nexts =
                    List.drop 1 approx ++ [ finalPoint ]
            in
            List.map3
                (\( isl, j ) prev next ->
                    let
                        mid =
                            meanBearing (bearingTo (centreOf isl) prev) (bearingTo (centreOf isl) next) + j

                        off =
                            angleDiff mid finalBearing
                    in
                    if isl.role == Hub && abs off < 50 then
                        ( isl
                        , finalBearing
                            + (if off >= 0 then
                                50

                               else
                                -50
                              )
                        )

                    else
                        ( isl, mid )
                )
                calls
                prevs
                nexts

        firstPass =
            dockAll (List.map (\( isl, _ ) -> centreOf isl) calls)

        harbours =
            dockAll (List.map (\( isl, b ) -> dockPoint isl b) firstPass)

        route =
            { harbours = harbours, finalBearing = finalBearing, startHeading = 0, treasureBearing = 0, treasure = hub, targets = [], lengthM = 0 }

        stops =
            fullStops hub route

        points =
            List.map .point stops

        islands =
            hub :: layout.ring

        blockers =
            List.concatMap
                (\( a, b ) -> List.filter (\isl -> legGap a b isl < legClearance) islands)
                (pairs stops)

        harbourPoints =
            List.drop 1 points

        apart =
            List.all
                (\( i, p ) -> List.all (\( j, q ) -> i >= j || dist p q >= 25) (List.indexedMap Tuple.pair harbourPoints))
                (List.indexedMap Tuple.pair harbourPoints)

        len =
            lengthOf points

        ( lo, hi ) =
            lengthRange
    in
    { harbours = harbours
    , finalBearing = finalBearing
    , stops = stops
    , blockers = blockers
    , valid =
        List.isEmpty blockers
            && List.all (\t -> t <= maxTurn) (turnsOf points)
            && len
            >= lo
            && len
            <= hi
            && apart
            && patrolLegs islands stops
            >= 3
    , lengthM = len
    }


indexOf : a -> List a -> Int
indexOf x xs =
    xs
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, y ) -> y == x)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


{-| How many legs between harbours have a clear pirate patrol circle at
their middle (as `Sails.Pirates.isClear`, with a margin): 9 m round,
clear of every harbour's mooring circle and the start by 8 m and of every
island by 4 m.
-}
patrolLegs : List Island -> List Stop -> Int
patrolLegs islands stops =
    let
        harbours =
            List.map .point (List.drop 1 stops)

        clear ( a, b ) =
            let
                mid =
                    ( (Tuple.first a + Tuple.first b) / 2, (Tuple.second a + Tuple.second b) / 2 )
            in
            List.all (\h -> dist mid h - 9 >= 10 + 8 + 1) harbours
                && (dist mid ( 0, 0 ) - 9 >= 10 + 8 + 1)
                && List.all (\isl -> dist mid (centreOf isl) - 9 >= isl.radius + 6) islands
    in
    List.length (List.filter clear (pairs harbours))


{-| The valid routes of a layout, up to three, in order of preference.
-}
validRoutes : Bool -> Layout -> ( List Candidate, List Candidate )
validRoutes clockwiseFirst layout =
    let
        all =
            List.map (candidate layout) (candidateOrders clockwiseFirst)
    in
    ( List.take 3 (List.filter .valid all), all )


{-| Try the layout as it is, with its docks turned 15 degrees either way,
then with the island that blocked the most legs shrunk by 10 % (to 30 m at
the least), up to three times.
-}
repair : Bool -> Layout -> Maybe ( Layout, List Candidate )
repair clockwiseFirst layout0 =
    let
        turned d l =
            { l | ringDocks = List.map ((+) d) l.ringDocks }

        tryDocks l =
            [ l, turned 15 l, turned -15 l ]
                |> List.foldl
                    (\candidateLayout found ->
                        case found of
                            Just _ ->
                                found

                            Nothing ->
                                case Tuple.first (validRoutes clockwiseFirst candidateLayout) of
                                    [] ->
                                        Nothing

                                    routes ->
                                        Just ( candidateLayout, routes )
                    )
                    Nothing

        shrink l =
            let
                blockers =
                    Tuple.second (validRoutes clockwiseFirst l)
                        |> List.concatMap .blockers

                count isl =
                    List.length (List.filter ((==) isl) blockers)

                worst =
                    (l.hub :: l.ring)
                        |> List.filter (\isl -> isl.radius > 30 && count isl > 0)
                        |> List.sortBy (\isl -> negate (count isl))
                        |> List.head

                smaller isl =
                    { isl | radius = max 30 (isl.radius * 0.9) }
            in
            case worst of
                Just w ->
                    if w == l.hub then
                        Just { l | hub = smaller w }

                    else
                        Just
                            { l
                                | ring =
                                    List.map
                                        (\isl ->
                                            if isl == w then
                                                smaller isl

                                            else
                                                isl
                                        )
                                        l.ring
                            }

                Nothing ->
                    Nothing

        go n l =
            case tryDocks l of
                Just found ->
                    Just found

                Nothing ->
                    if n == 0 then
                        Nothing

                    else
                        case shrink l of
                            Just smaller ->
                                go (n - 1) smaller

                            Nothing ->
                                Nothing
    in
    go 3 layout0



-- GENERATE


{-| The archipelago of a map.
-}
generate : MapParams -> Archipelago
generate map =
    let
        try attempt =
            let
                seed0 =
                    Random.initialSeed (MapParams.streamSeed ("archipelago/" ++ String.fromInt attempt) map)

                ( layout, seed1 ) =
                    Random.step (ringGen map) seed0

                ( clockwiseFirst, seed2 ) =
                    Random.step (Random.uniform True [ False ]) seed1
            in
            case repair clockwiseFirst layout of
                Just ( fixed, routes ) ->
                    furnish map attempt fixed routes seed2

                Nothing ->
                    Nothing

        go attempt =
            if attempt >= maxAttempts then
                fallback map

            else
                case try attempt of
                    Just arch ->
                        arch

                    Nothing ->
                        go (attempt + 1)
    in
    go 0


{-| The layout used if every attempt fails: the first attempt's ring with
its first route, whatever it breaks. The seed sweep tests make sure no map
gets here.
-}
fallback : MapParams -> Archipelago
fallback map =
    let
        seed0 =
            Random.initialSeed (MapParams.streamSeed "archipelago/0" map)

        ( layout, seed1 ) =
            Random.step (ringGen map) seed0

        routes =
            List.take 1 (Tuple.second (validRoutes True layout))
    in
    case furnishWith True map maxAttempts layout routes seed1 of
        Just arch ->
            { arch | valid = False }

        Nothing ->
            { map = map
            , islands = layout.hub :: layout.ring
            , hub = layout.hub
            , harbourIsles = layout.ring
            , routes = []
            , towns = []
            , decor = []
            , wind = { speed = World.windSpeed map, direction = 270 }
            , arena = { x = layout.hub.x, z = layout.hub.z, radius = 300 }
            , attempts = maxAttempts
            , valid = False
            }


furnish : MapParams -> Int -> Layout -> List Candidate -> Random.Seed -> Maybe Archipelago
furnish =
    furnishWith False


{-| Add the treasure, the targets, the towns, the skerries, the decoration,
the arena and the wind to a layout with its routes. The treasure island
must lie within reach of every route kept (see `placeTreasure`); if it
cannot, the last routes are dropped until it does. `Nothing` if it fits
nowhere even for the first route alone (`force` places it anyway).
-}
furnishWith : Bool -> MapParams -> Int -> Layout -> List Candidate -> Random.Seed -> Maybe Archipelago
furnishWith force map attempt layout offered seed0 =
    let
        hub =
            layout.hub

        mainIslands =
            hub :: layout.ring

        ( treasureDraw, seed1 ) =
            Random.step (Random.map3 (\r o s -> ( r, o, s )) (range ( 8, 12 )) (range ( 25, 40 )) (Random.uniform 1 [ -1 ])) seed0

        keep n =
            let
                kept =
                    List.take n offered
            in
            case kept of
                first :: _ ->
                    placeTreasure treasureDraw mainIslands (List.map .stops kept) first.stops
                        |> Maybe.map (\t -> ( kept, t ))

                [] ->
                    Nothing

        found =
            firstJust keep (List.reverse (List.range 1 (List.length offered)))
    in
    case ( found, force ) of
        ( Nothing, False ) ->
            Nothing

        _ ->
            let
                ( candidates, ( treasure, treasureBearing ) ) =
                    Maybe.withDefault ( List.take 1 offered, ( { x = hub.x, z = hub.z, radius = 1, height = 1, role = TreasureIsle }, 0 ) ) found

                allStops =
                    List.map .stops candidates

                allLegs =
                    List.concatMap pairs allStops

                islandsSoFar =
                    mainIslands ++ [ treasure ]

                points =
                    List.concatMap (List.map .point) allStops

                n =
                    toFloat (max 1 (List.length points))

                ( ax, az ) =
                    ( List.sum (List.map Tuple.first points) / n, List.sum (List.map Tuple.second points) / n )

                arenaRadius =
                    (List.maximum (List.map (dist ( ax, az )) points) |> Maybe.withDefault 200) + 60

                arena =
                    { x = ax, z = az, radius = arenaRadius }

                ( targetSeeds, seed2 ) =
                    Random.step (Random.list (List.length candidates) (Random.int 1 2147483646)) seed1

                ( skerries, seed3 ) =
                    placeSkerries map arena islandsSoFar allLegs seed2

                islands =
                    islandsSoFar ++ skerries

                ( decor, seed4 ) =
                    placeDecor map arena islands allLegs seed3

                ( windDir, seed5 ) =
                    Random.step (Random.float 0 360) seed4

                ( townDraws, _ ) =
                    Random.step (Random.list 4 (Random.map2 (\a s -> a * s) (range ( 40, 70 )) (Random.uniform 1 [ -1 ]))) seed5

                routes =
                    List.map2
                        (\c tSeed ->
                            { harbours = c.harbours
                            , finalBearing = c.finalBearing
                            , startHeading =
                                case List.drop 1 c.stops of
                                    s :: _ ->
                                        bearingTo ( 0, 0 ) s.point

                                    [] ->
                                        0
                            , treasureBearing = treasureBearing
                            , treasure = treasure
                            , targets = placeTargets islands c.stops tSeed
                            , lengthM = c.lengthM
                            }
                        )
                        candidates
                        targetSeeds

                wind =
                    { speed = World.windSpeed map, direction = fairWind routes windDir }
            in
            Just
                { map = map
                , islands = islands
                , hub = hub
                , harbourIsles = layout.ring
                , routes = routes
                , towns = placeTowns map layout routes townDraws
                , decor = decor
                , wind = wind
                , arena = arena
                , attempts = attempt + 1
                , valid = True
                }


{-| Turn the wind 15 degrees at a time until no route starts within 30
degrees of dead upwind.
-}
fairWind : List Route -> Float -> Float
fairWind routes dir0 =
    let
        ok dir =
            List.all (\r -> abs (angleDiff r.startHeading (dir + 180)) >= 30) routes

        go k =
            let
                dir =
                    dir0 + 15 * toFloat k
            in
            if ok dir || k >= 24 then
                dir - 360 * toFloat (floor (dir / 360))

            else
                go (k + 1)
    in
    go 0


{-| The treasure island beside the middle leg of the first route (or the
nearest leg to it that has room), off to one side, clear of every leg of
every route and of the other islands, and with the chest within 40 m of
every route (inside `Sails.Physics.courseFreeRange`, so no current pushes a
boat back on the detour). Returns the island and the bearing of the chest
on its shore, facing the leg.
-}
placeTreasure : ( Float, Float, Float ) -> List Island -> List (List Stop) -> List Stop -> Maybe ( Island, Float )
placeTreasure ( radius, offset, side ) islands routeStops stops =
    let
        allLegs =
            List.concatMap pairs routeStops

        -- Each route as `Sails.Physics.nearestCoursePoint` sees it, with
        -- the leg back from the finish to the start.
        withReturn rs =
            case rs of
                first :: _ ->
                    pairs (rs ++ [ first ])

                [] ->
                    []

        nearRoute p rs =
            withReturn rs
                |> List.map (\( a, b ) -> segmentDistance a.point b.point 0 (dist a.point b.point) p)
                |> List.minimum
                |> Maybe.withDefault 1.0e9

        routeLegs =
            pairs stops

        middle =
            List.length routeLegs // 2

        legOrder =
            List.range 0 (List.length routeLegs - 1)
                |> List.sortBy
                    (\i ->
                        abs (i - middle)
                            * 2
                            + (if i < middle then
                                1

                               else
                                0
                              )
                    )

        options =
            List.concatMap
                (\i ->
                    List.concatMap
                        (\s ->
                            List.concatMap
                                (\f -> List.map (\o -> ( i, ( s, f, o ) )) [ offset, 32, 40, 25 ])
                                [ 0.5, 0.4, 0.6 ]
                        )
                        [ side, negate side ]
                )
                legOrder

        mids =
            List.map
                (\( a, b ) -> ( (Tuple.first a.point + Tuple.first b.point) / 2, (Tuple.second a.point + Tuple.second b.point) / 2 ))
                allLegs

        at ( i, ( s, f, o ) ) =
            case List.drop i routeLegs of
                ( a, b ) :: _ ->
                    let
                        ( ax, az ) =
                            a.point

                        ( bx, bz ) =
                            b.point

                        len =
                            max 1 (dist a.point b.point)

                        ( nx, nz ) =
                            ( (bz - az) / len * s, -(bx - ax) / len * s )

                        ( px, pz ) =
                            ( ax + (bx - ax) * f + nx * o, az + (bz - az) * f + nz * o )

                        isl =
                            { x = px, z = pz, radius = radius, height = 5, role = TreasureIsle }

                        fits =
                            List.all (\( p, q ) -> legGap p q isl >= legClearance) allLegs
                                && List.all (\other -> dist ( px, pz ) (centreOf other) - other.radius - radius >= minIslandGap) islands
                                && List.all (\m -> dist ( px, pz ) m - 9 >= radius + 6) mids
                                && dist ( px, pz ) ( 0, 0 )
                                >= radius
                                + 20
                                && List.all (\rs -> nearRoute chest rs <= 40) routeStops

                        chestBearing =
                            bearingTo ( px, pz ) ( ax + (bx - ax) * f, az + (bz - az) * f )

                        chest =
                            ( px + sin (degrees chestBearing) * radius * 0.86, pz + cos (degrees chestBearing) * radius * 0.86 )
                    in
                    if fits then
                        Just ( isl, chestBearing )

                    else
                        Nothing

                [] ->
                    Nothing
    in
    firstJust at options


firstJust : (a -> Maybe b) -> List a -> Maybe b
firstJust f xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            case f x of
                Just y ->
                    Just y

                Nothing ->
                    firstJust f rest


{-| Seven floating targets beside a route's legs, 35 to 65 % of the way
along, 12 to 20 m off to the side away from the nearest island, at least
6 m off every island's shore and off the pirates' patrol circles.
-}
placeTargets : List Island -> List Stop -> Int -> List ( Float, Float )
placeTargets islands stops seedInt =
    let
        routeLegs =
            pairs stops

        nLegs =
            List.length routeLegs

        ( draws, _ ) =
            Random.step (Random.list 7 (Random.map2 Tuple.pair (range ( 0.35, 0.65 )) (range ( 12, 20 )))) (Random.initialSeed seedInt)

        harbourLegs =
            pairs (List.drop 1 stops)

        mids =
            List.map (\( a, b ) -> ( (Tuple.first a.point + Tuple.first b.point) / 2, (Tuple.second a.point + Tuple.second b.point) / 2 )) harbourLegs

        target i ( f0, o ) =
            case List.drop (modBy (max 1 nLegs) i) routeLegs of
                ( a, b ) :: _ ->
                    let
                        f =
                            if i >= nLegs then
                                1 - f0

                            else
                                f0

                        ( ax, az ) =
                            a.point

                        ( bx, bz ) =
                            b.point

                        len =
                            max 1 (dist a.point b.point)

                        ( nx, nz ) =
                            ( (bz - az) / len, -(bx - ax) / len )

                        base =
                            ( ax + (bx - ax) * f, az + (bz - az) * f )

                        nearest =
                            islands
                                |> List.sortBy (\isl -> dist base (centreOf isl) - isl.radius)
                                |> List.head

                        awaySide =
                            case nearest of
                                Just isl ->
                                    if (isl.x - Tuple.first base) * nx + (isl.z - Tuple.second base) * nz > 0 then
                                        -1

                                    else
                                        1

                                Nothing ->
                                    1

                        spot s off =
                            ( Tuple.first base + nx * s * off, Tuple.second base + nz * s * off )

                        fits p =
                            List.all (\isl -> dist p (centreOf isl) >= isl.radius + 6) islands
                                && List.all (\m -> abs (dist p m - 9) >= 5) mids
                                && List.all (\s -> dist p s.point >= 12) stops
                    in
                    [ spot awaySide o, spot awaySide 12, spot (negate awaySide) o, spot (negate awaySide) 12 ]
                        |> List.filter fits
                        |> List.head

                [] ->
                    Nothing
    in
    List.filterMap identity (List.indexedMap target draws)


{-| A town on each harbour island, 40 to 70 degrees round the shore from its
dock, and one on the hub, as far round its shore from the hub's harbours as
it can be. Three of the harbour islands are the factions' citadels: the
Red Coats beside the first, the Pirates on the far one and the Blue Coats
on the third; the one behind the start has no flag.
-}
placeTowns : MapParams -> Layout -> List Route -> List Float -> List Town
placeTowns map layout routes draws =
    let
        names =
            World.townNames map.biome

        nameAt i =
            List.drop i names |> List.head |> Maybe.withDefault "Harbour"

        factionOf k =
            case k of
                1 ->
                    Just RedCoats

                0 ->
                    Just Pirates

                3 ->
                    Just BlueCoats

                _ ->
                    Nothing

        docksOn isl =
            List.concatMap
                (\r ->
                    List.filterMap
                        (\( i, b ) ->
                            if i == isl then
                                Just b

                            else
                                Nothing
                        )
                        r.harbours
                )
                routes

        -- As far round the shore from every dock on the island as the
        -- preferred 40 to 70 degrees off the first route's dock allows.
        townBearing isl d =
            let
                docks =
                    docksOn isl

                first =
                    List.head docks |> Maybe.withDefault 0

                gapAt b =
                    List.minimum (List.map (\x -> abs (angleDiff b x)) docks) |> Maybe.withDefault 180
            in
            (first + d)
                :: List.map (\i -> toFloat i * 10) (List.range 0 35)
                |> List.filter (\b -> gapAt b >= 40)
                |> List.head
                |> Maybe.withDefault (first + d)

        ringTowns =
            List.map2
                (\( k, isl ) d ->
                    { name = nameAt (k + 1), island = isl, bearing = townBearing isl d, faction = factionOf k }
                )
                (List.indexedMap Tuple.pair layout.ring)
                draws

        hubDocks =
            List.concatMap
                (\r ->
                    r.finalBearing
                        :: List.filterMap
                            (\( isl, b ) ->
                                if isl.role == Hub then
                                    Just b

                                else
                                    Nothing
                            )
                            r.harbours
                )
                routes

        hubBearing =
            List.range 0 35
                |> List.map (\i -> toFloat i * 10)
                |> List.sortBy (\b -> negate (List.minimum (List.map (\d -> abs (angleDiff b d)) hubDocks) |> Maybe.withDefault 180))
                |> List.head
                |> Maybe.withDefault 100
    in
    { name = nameAt 0, island = layout.hub, bearing = hubBearing, faction = Nothing } :: ringTowns


{-| Whether a point (with a radius) lies well out of the way: inside the
arena, 55 m (`courseFreeRange` and 10 m) off every leg, and clear of the
islands and the start.
-}
outOfTheWay : { x : Float, z : Float, radius : Float } -> List Island -> List ( Stop, Stop ) -> Float -> Point -> Float -> Bool
outOfTheWay arena islands allLegs gap p r =
    dist p ( arena.x, arena.z )
        + r
        <= arena.radius
        - 10
        && List.all (\( a, b ) -> segmentDistance a.point b.point 0 (dist a.point b.point) p - r >= 55) allLegs
        && List.all (\isl -> dist p (centreOf isl) - isl.radius - r >= gap) islands
        && dist p ( 0, 0 )
        - r
        >= 40


{-| The skerries of a map: small rocks out of the way of the routes.
-}
placeSkerries : MapParams -> { x : Float, z : Float, radius : Float } -> List Island -> List ( Stop, Stop ) -> Random.Seed -> ( List Island, Random.Seed )
placeSkerries map arena islands allLegs seed0 =
    let
        scale =
            World.islandScale map.biome

        ( count, seed1 ) =
            Random.step (Random.int (Tuple.first scale.skerries) (Tuple.second scale.skerries)) seed0

        spotGen =
            Random.map4 (\a d r h -> ( a, d, ( r, h ) )) (Random.float 0 360) (Random.float 0 1) (range ( 6, 14 )) (range ( 3, 5 ))

        ( spots, seed2 ) =
            Random.step (Random.list 60 spotGen) seed1

        add ( a, d, ( r, h ) ) placed =
            if List.length placed >= count then
                placed

            else
                let
                    reach =
                        sqrt d * (arena.radius - 10 - r)

                    p =
                        ( arena.x + reach * sin (degrees a), arena.z + reach * cos (degrees a) )
                in
                if outOfTheWay arena (islands ++ placed) allLegs minIslandGap p r then
                    placed ++ [ { x = Tuple.first p, z = Tuple.second p, radius = r, height = h * scale.height, role = Skerry } ]

                else
                    placed
    in
    ( List.foldl add [] spots, seed2 )


{-| Coral reefs (2 to 4) and, where the biome has them, a wreck and rock sea
arches, out of the way of the routes like the skerries.
-}
placeDecor : MapParams -> { x : Float, z : Float, radius : Float } -> List Island -> List ( Stop, Stop ) -> Random.Seed -> ( List Decor, Random.Seed )
placeDecor map arena islands allLegs seed0 =
    let
        look =
            World.dressing map

        ( ( reefs, wrecks, arches ), seed1 ) =
            Random.step
                (Random.map3 (\a b c -> ( a, b, c ))
                    (Random.int 2 4)
                    (if look.wreck then
                        Random.int 0 1

                     else
                        Random.constant 0
                    )
                    (if look.arches then
                        Random.int 0 2

                     else
                        Random.constant 0
                    )
                )
                seed0

        wanted =
            List.repeat reefs Reef ++ List.repeat wrecks Wreck ++ List.repeat arches SeaArch

        sizeOf kind =
            case kind of
                Reef ->
                    6

                Wreck ->
                    6

                SeaArch ->
                    8

        spotGen =
            Random.map3 (\a d y -> ( a, d, y )) (Random.float 0 360) (Random.float 0 1) (Random.float 0 (2 * pi))

        ( spots, seed2 ) =
            Random.step (Random.list 80 spotGen) seed1

        add ( a, d, yaw ) ( todo, placed ) =
            case todo of
                [] ->
                    ( todo, placed )

                kind :: rest ->
                    let
                        r =
                            sizeOf kind

                        reach =
                            sqrt d * (arena.radius - 10 - r)

                        p =
                            ( arena.x + reach * sin (degrees a), arena.z + reach * cos (degrees a) )

                        clearOfDecor =
                            List.all (\o -> dist p ( o.x, o.z ) >= 25) placed
                    in
                    if clearOfDecor && outOfTheWay arena islands allLegs 10 p r then
                        ( rest, placed ++ [ { kind = kind, x = Tuple.first p, z = Tuple.second p, yaw = yaw } ] )

                    else
                        ( todo, placed )
    in
    ( Tuple.second (List.foldl add ( wanted, [] ) spots), seed2 )



-- COURSE


{-| The course of a seat (0 for the first seat and for a boat sailing
alone): the archipelago's islands, towns and decoration, with the harbour
order, targets and start heading of route `seat` modulo the number of
routes.
-}
courseFor : Archipelago -> Int -> Course
courseFor arch seat =
    let
        route =
            case List.drop (modBy (max 1 (routeCount arch)) seat) arch.routes of
                r :: _ ->
                    r

                [] ->
                    { harbours = [], finalBearing = 0, startHeading = 0, treasureBearing = 0, treasure = arch.hub, targets = [], lengthM = 950 }

        harbour i kind island bearing =
            let
                ( x, z ) =
                    dockPoint island bearing
            in
            { index = i, position = vec3 x 0 z, radius = 10, kind = kind }

        waypoints : List Waypoint
        waypoints =
            List.indexedMap (\i ( island, bearing ) -> harbour i Harbour island bearing) route.harbours
                ++ [ harbour (List.length route.harbours) FinalHarbour arch.hub route.finalBearing ]

        chest =
            let
                t =
                    route.treasure

                share =
                    0.86
            in
            vec3 (t.x + sin (degrees route.treasureBearing) * t.radius * share)
                (0.22 * t.height - 0.6)
                (t.z + cos (degrees route.treasureBearing) * t.radius * share)
    in
    { id = MapParams.leaderboardKey arch.map
    , map = arch.map
    , title = MapParams.toCode arch.map
    , description = "Dock at each harbour in order, round the islands and back to the final harbour. Pirate treasure lies on a small island beside the route."
    , waypoints = waypoints
    , targets = List.indexedMap (\i ( x, z ) -> { id = i, position = vec3 x 0 z, sunk = False, sinkAge = 0 }) route.targets
    , islands = arch.islands
    , towns = arch.towns
    , treasures =
        if route.treasure.role == TreasureIsle then
            [ { id = 0, position = chest, bearing = route.treasureBearing, bonusSeconds = 6 } ]

        else
            []
    , decor = arch.decor
    , wind = arch.wind
    , arena = arch.arena
    , startPosition = vec3 0 0 0
    , startHeading = route.startHeading
    , targetSeconds = toFloat (round (route.lengthM / 8.6))
    }


{-| The course of every route, one per seat up to the number of routes.
-}
seatCourses : Archipelago -> List Course
seatCourses arch =
    List.map (courseFor arch) (List.range 0 (routeCount arch - 1))
