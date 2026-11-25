module ArchipelagoTest exposing (suite)

{-| Tests of the generated archipelagos (`Sails.Archipelago`) and of the map
tables (`Sails.World`, the weather profiles of `Sails.Weather`).

The seed sweep checks the layout rules on hundreds of maps from the course
the game plays (`Archipelago.seatCourses`), with geometry worked out here
rather than taken from the generator: every leg passes 8 m off every
island's shore (bar the 12 m at either end that docks on that island),
islands keep 30 m apart, harbour turns stay under 150 degrees, routes run
850 to 1050 m, and so on. The autopilot then sails every route of twelve
maps (three per biome) to the finish.

-}

import Expect
import Game.MapParams as MapParams exposing (Biome(..), MapParams, Pick(..), Season(..), WeatherKind(..))
import Math.Vector3 as Vec3 exposing (Vec3)
import Sails.Archipelago as Archipelago exposing (Archipelago)
import Sails.Boats as Boats
import Sails.Physics as Physics
import Sails.Types exposing (Course, Island, IslandRole(..))
import Sails.Weather as Weather exposing (Spell(..))
import Sails.World as World
import SailsTest
import Test exposing (Test, describe, test)



-- MAPS


{-| 100 maps of each biome, with the season and the weather picked from the
seed as on the main screen with every pick left at Random.
-}
sweepMaps : List MapParams
sweepMaps =
    MapParams.allBiomes
        |> List.concatMap
            (\b ->
                List.map (\seed -> MapParams.resolve { biome = Exactly b, season = Any, weather = Any, seed = seed * 97 }) (List.range 1 100)
            )


sweep : List ( MapParams, Archipelago )
sweep =
    List.map (\m -> ( m, Archipelago.generate m )) sweepMaps


{-| Three maps per biome for the autopilot.
-}
sailedMaps : List MapParams
sailedMaps =
    MapParams.allBiomes
        |> List.concatMap
            (\b ->
                [ MapParams.resolve { biome = Exactly b, season = Exactly Summer, weather = Exactly Clear, seed = 7 }
                , MapParams.resolve { biome = Exactly b, season = Exactly Winter, weather = Exactly Snow, seed = 4242 }
                , MapParams.resolve { biome = Exactly b, season = Exactly Autumn, weather = Exactly Storm, seed = 999999 }
                ]
            )


{-| Run a check on every map of the sweep and list the codes it fails on,
so a failure names the maps.
-}
everyMap : (Archipelago -> List Course -> Bool) -> Expect.Expectation
everyMap check =
    sweep
        |> List.filter (\( _, arch ) -> not (check arch (Archipelago.seatCourses arch)))
        |> List.map (Tuple.first >> MapParams.toCode)
        |> Expect.equal []



-- GEOMETRY


type alias P =
    ( Float, Float )


flat : Vec3 -> P
flat v =
    ( Vec3.getX v, Vec3.getZ v )


dist : P -> P -> Float
dist ( ax, az ) ( bx, bz ) =
    sqrt ((bx - ax) ^ 2 + (bz - az) ^ 2)


{-| Nearest distance from `p` to the part of segment a-b from `s0` to `s1`
metres along it.
-}
partDistance : P -> P -> Float -> Float -> P -> Float
partDistance ( ax, az ) ( bx, bz ) s0 s1 ( px, pz ) =
    let
        len =
            dist ( ax, az ) ( bx, bz )

        t =
            if len < 1.0e-6 then
                0

            else
                clamp (s0 / len) (s1 / len) (((px - ax) * (bx - ax) + (pz - az) * (bz - az)) / (len * len))
    in
    dist ( ax + (bx - ax) * t, az + (bz - az) * t ) ( px, pz )


{-| The island a harbour docks on: the one whose shore is 3 m away.
-}
dockedOn : Course -> P -> Maybe Island
dockedOn course p =
    course.islands
        |> List.filter (\isl -> abs (dist p ( isl.x, isl.z ) - isl.radius - 3) < 0.01)
        |> List.head


routeOf : Course -> List P
routeOf course =
    flat course.startPosition :: List.map (.position >> flat) course.waypoints


legsOf : Course -> List ( P, P )
legsOf course =
    let
        pts =
            routeOf course
    in
    List.map2 Tuple.pair pts (List.drop 1 pts)


{-| The leg passes 8 m off every island, leaving out the 12 m at an end
that docks on that island.
-}
legsClear : Course -> Bool
legsClear course =
    List.all
        (\( a, b ) ->
            List.all
                (\isl ->
                    let
                        len =
                            dist a b

                        s0 =
                            if dockedOn course a == Just isl then
                                12

                            else
                                0

                        s1 =
                            if dockedOn course b == Just isl then
                                len - 12

                            else
                                len
                    in
                    s1 <= s0 || partDistance a b s0 s1 ( isl.x, isl.z ) >= isl.radius + 8 - 1.0e-6
                )
                course.islands
        )
        (legsOf course)


bearing : P -> P -> Float
bearing ( ax, az ) ( bx, bz ) =
    atan2 (bx - ax) (bz - az) * 180 / pi


turnsOf : Course -> List Float
turnsOf course =
    let
        pts =
            routeOf course
    in
    List.map3 (\a b c -> abs (Physics.angleDiff (bearing b c) (bearing a b))) pts (List.drop 1 pts) (List.drop 2 pts)


lengthOf : Course -> Float
lengthOf course =
    List.sum (List.map (\( a, b ) -> dist a b) (legsOf course))


pairsOf : List a -> List ( a, a )
pairsOf xs =
    case xs of
        [] ->
            []

        x :: rest ->
            List.map (Tuple.pair x) rest ++ pairsOf rest



-- SUITE


suite : Test
suite =
    describe "Archipelago"
        [ describe "generation"
            [ test "the same map gives the same archipelago" <|
                \_ ->
                    List.map Archipelago.generate (List.take 20 sweepMaps)
                        |> Expect.equal (List.map Archipelago.generate (List.take 20 sweepMaps))
            , test "different seeds give different archipelagos" <|
                \_ ->
                    let
                        m =
                            Archipelago.defaultMap
                    in
                    Expect.notEqual (Archipelago.generate m).islands (Archipelago.generate { m | seed = 2 }).islands
            , test "golden maps keep their layout (bump MapParams.generatorVersion if this changes on purpose)" <|
                \_ ->
                    [ "desert-summer-storm-1", "boreal-winter-snow-48213", "farmland-autumn-fog-777", "fell-spring-clear-31337" ]
                        |> List.filterMap MapParams.fromCode
                        |> List.map
                            (\m ->
                                let
                                    arch =
                                        Archipelago.generate m
                                in
                                ( List.length arch.islands, List.map (.lengthM >> round) arch.routes )
                            )
                        |> Expect.equal golden
            , test "a course's id is the map's leaderboard key and its map is the map" <|
                \_ ->
                    Expect.equal ( "g1:desert-summer-storm-1", Archipelago.defaultMap )
                        ( Archipelago.defaultCourse.id, Archipelago.defaultCourse.map )
            ]
        , describe "seed sweep (100 maps per biome)"
            [ test "no map needs the fallback layout" <|
                \_ -> everyMap (\arch _ -> arch.valid && arch.attempts <= Archipelago.maxAttempts)
            , test "every map has one to three routes, and seats wrap round them" <|
                \_ ->
                    everyMap
                        (\arch courses ->
                            List.length courses
                                >= 1
                                && List.length courses
                                <= 3
                                && (Archipelago.courseFor arch (List.length courses)).waypoints
                                == (Archipelago.courseFor arch 0).waypoints
                        )
            , test "a hub, four harbour islands and a treasure island" <|
                \_ ->
                    everyMap
                        (\arch _ ->
                            let
                                count role =
                                    List.length (List.filter (\i -> i.role == role) arch.islands)
                            in
                            count Hub == 1 && count HarbourIsle == 4 && count TreasureIsle == 1
                        )
            , test "every leg passes 8 m off every island (bar 12 m at its own harbours)" <|
                \_ -> everyMap (\_ courses -> List.all legsClear courses)
            , test "islands keep 30 m apart" <|
                \_ ->
                    everyMap
                        (\arch _ ->
                            List.all (\( a, b ) -> dist ( a.x, a.z ) ( b.x, b.z ) - a.radius - b.radius >= 30 - 1.0e-6) (pairsOf arch.islands)
                        )
            , test "no harbour turn is sharper than 150 degrees" <|
                \_ -> everyMap (\_ courses -> List.all (\c -> List.all (\t -> t <= 150) (turnsOf c)) courses)
            , test "every route is 850 to 1050 m long, and the target time follows its length" <|
                \_ ->
                    everyMap
                        (\_ courses ->
                            List.all
                                (\c ->
                                    let
                                        len =
                                            lengthOf c
                                    in
                                    len >= 850 && len <= 1050 && c.targetSeconds == toFloat (round (len / 8.6))
                                )
                                courses
                        )
            , test "every seat calls at the same harbour islands, each harbour on an island's shore, ending at the hub" <|
                \_ ->
                    everyMap
                        (\_ courses ->
                            let
                                docks c =
                                    List.map (.position >> flat >> dockedOn c) c.waypoints

                                islandsOf c =
                                    docks c |> List.filterMap identity |> List.map (\i -> ( i.x, i.z )) |> List.sort
                            in
                            List.all
                                (\c ->
                                    List.all ((/=) Nothing) (docks c)
                                        && (List.reverse (docks c) |> List.head |> Maybe.andThen identity |> Maybe.map .role)
                                        == Just Hub
                                        && List.length c.waypoints
                                        == 6
                                )
                                courses
                                && List.all (\c -> islandsOf c == (List.head courses |> Maybe.map islandsOf |> Maybe.withDefault [])) courses
                        )
            , test "the start is in open water and the first leg is not dead upwind" <|
                \_ ->
                    everyMap
                        (\_ courses ->
                            List.all
                                (\c ->
                                    List.all (\i -> dist ( 0, 0 ) ( i.x, i.z ) >= i.radius + 8) c.islands
                                        && abs (Physics.angleDiff c.startHeading (c.wind.direction + 180))
                                        >= 30
                                        - 1.0e-6
                                )
                                courses
                        )
            , test "every harbour lies inside the arena with 40 m to spare" <|
                \_ ->
                    everyMap
                        (\_ courses ->
                            List.all
                                (\c -> List.all (\p -> dist p ( c.arena.x, c.arena.z ) <= c.arena.radius - 40) (routeOf c))
                                courses
                        )
            , test "the treasure lies on its island's beach, within reach of every route" <|
                \_ ->
                    everyMap
                        (\_ courses ->
                            List.all
                                (\c ->
                                    List.length c.treasures
                                        == 1
                                        && List.all (\t -> Physics.courseCurrent (Physics.courseRoute c) t.position == ( 0, 0 )) c.treasures
                                )
                                courses
                        )
            , test "targets float off every island's shore" <|
                \_ ->
                    everyMap
                        (\_ courses ->
                            List.all
                                (\c ->
                                    List.length c.targets
                                        >= 4
                                        && List.all (\t -> List.all (\i -> dist (flat t.position) ( i.x, i.z ) >= i.radius + 6 - 1.0e-6) c.islands) c.targets
                                )
                                courses
                        )
            , test "skerries, reefs, wrecks and arches lie well off every route" <|
                \_ ->
                    everyMap
                        (\_ courses ->
                            List.all
                                (\c ->
                                    List.all
                                        (\( a, b ) ->
                                            List.all (\i -> i.role /= Skerry || partDistance a b 0 (dist a b) ( i.x, i.z ) - i.radius >= Physics.courseFreeRange) c.islands
                                                && List.all (\d -> partDistance a b 0 (dist a b) ( d.x, d.z ) >= Physics.courseFreeRange) c.decor
                                        )
                                        (legsOf c)
                                )
                                courses
                        )
            , test "towns stand on the hub and on every harbour island, with the three citadels" <|
                \_ ->
                    everyMap
                        (\arch _ ->
                            List.length arch.towns
                                == 5
                                && List.all (\t -> List.member t.island arch.islands) arch.towns
                                && List.length (List.filterMap .faction arch.towns)
                                == 3
                        )
            ]
        , describe "sailing"
            [ test "the autopilot finishes every route of twelve maps within twice the target time" <|
                \_ ->
                    sailedMaps
                        |> List.concatMap
                            (\m ->
                                Archipelago.seatCourses (Archipelago.generate m)
                                    |> List.indexedMap
                                        (\seat c ->
                                            let
                                                run =
                                                    SailsTest.autopilot c Boats.default (2 * c.targetSeconds)
                                            in
                                            if run.finished && run.elapsedSeconds <= 2 * c.targetSeconds then
                                                Nothing

                                            else
                                                Just ( MapParams.toCode m, seat, run.currentWaypointIndex )
                                        )
                                    |> List.filterMap identity
                            )
                        |> Expect.equal []
            ]
        , describe "map tables"
            [ test "the course wind stays between a light breeze and a fresh breeze" <|
                \_ ->
                    sweepMaps
                        |> List.map World.windSpeed
                        |> List.filter (\w -> w < 3.5 || w > 11)
                        |> Expect.equal []
            , test "every island palette a map uses has a prop model" <|
                \_ ->
                    sweepMaps
                        |> List.map (World.islandPalette >> .key)
                        |> List.filter (\k -> not (List.any (\( name, _ ) -> name == k) World.islandModels))
                        |> Expect.equal []
            ]
        , describe "weather profiles"
            [ test "clear and fog weather never leave fair" <|
                \_ ->
                    [ Clear, Fog ]
                        |> List.map (\w -> spells (World.weatherProfile { biome = Fell, season = Summer, weather = w, seed = 1 }) 600)
                        |> List.map (List.filter ((/=) Fair))
                        |> Expect.equal [ [], [] ]
            , test "storm weather is the tuned storm profile and still brings every kind of storm" <|
                \_ ->
                    let
                        profile =
                            World.weatherProfile Archipelago.defaultMap

                        seen =
                            spells profile 600
                    in
                    Expect.all
                        [ \_ -> Expect.equal Weather.stormProfile profile
                        , \_ -> Expect.equal True (List.member Squall seen)
                        , \_ -> Expect.equal True (List.member Thunderstorm seen)
                        ]
                        ()
            , test "snow falls as snow, rain as rain" <|
                \_ ->
                    let
                        precipOf w =
                            (Weather.conditions { speed = 7, direction = 0 } (Weather.init (World.weatherProfile { biome = Boreal, season = Winter, weather = w, seed = 1 }) 3)).precip
                    in
                    Expect.equal [ Weather.SnowFall, Weather.RainFall ] [ precipOf Snow, precipOf Storm ]
            , test "fair weather of the clear profile is the course wind and the normal sea" <|
                \_ ->
                    let
                        wind =
                            { speed = 6.3, direction = 123 }
                    in
                    Expect.equal (Weather.fair wind) (Weather.conditions wind (Weather.init Weather.clearProfile 9))
            , test "fog pulls the fog in and overcast greys the sky" <|
                \_ ->
                    let
                        at w =
                            Weather.conditions { speed = 7, direction = 0 } (Weather.init (World.weatherProfile { biome = Farmland, season = Autumn, weather = w, seed = 1 }) 3)
                    in
                    Expect.all
                        [ \_ -> Expect.lessThan 0.5 (at Fog).fog
                        , \_ -> Expect.greaterThan 0.2 (at Overcast).gloom
                        , \_ -> Expect.greaterThan 0.3 (at Rain).rain
                        ]
                        ()
            ]
        ]


{-| The spells of `seconds` of weather in a profile, one sample a second.
-}
spells : Weather.Profile -> Float -> List Spell
spells profile seconds =
    List.foldl
        (\_ ( w, acc ) ->
            let
                stepped =
                    List.foldl (\_ x -> Tuple.first (Weather.step 0.1 x)) w (List.range 1 10)
            in
            ( stepped, stepped.spell :: acc )
        )
        ( Weather.init profile 11, [] )
        (List.range 1 (round seconds))
        |> Tuple.second


{-| Island count and route lengths (m) of the golden maps.
-}
golden : List ( Int, List Int )
golden =
    [ ( 8, [ 869, 869 ] ), ( 10, [ 855, 865, 946 ] ), ( 8, [ 999 ] ), ( 10, [ 1011 ] ) ]
