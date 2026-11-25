module GameplayTest exposing (suite)

{-| Tests of the sea, the sails, the weather and the islands.
-}

import Dict
import Expect
import LDraw.Document as Document
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartStatus(..))
import Math.Vector3 as Vec3
import Sails.Boats as Boats
import Sails.Physics as Physics
import Sails.Scenery as Scenery
import Sails.ShipMesh as ShipMesh
import Sails.Types exposing (CourseProgress, SailingState, Wind)
import Sails.Weather as Weather exposing (Spell(..), Weather)
import Test exposing (Test, describe, test)


course : Sails.Types.Course
course =
    Physics.defaultCourse


{-| The highest swell seen over a grid of points and a minute and a half.
-}
peakWave : Float -> Float
peakWave sea =
    List.range 0 90
        |> List.concatMap
            (\i ->
                List.range 0 8
                    |> List.map (\j -> abs (Physics.seaElevation sea (toFloat j * 17) (toFloat j * 11) (toFloat i)))
            )
        |> List.maximum
        |> Maybe.withDefault 0


{-| Sail straight on a heading (the helm holds it) for `seconds`.
-}
holdCourse : Float -> Float -> Float -> SailingState
holdCourse sea heading seconds =
    sailWith { sea = sea, sail = 1, wind = course.wind, steer = Nothing } heading seconds


{-| Sail from the start on a heading for `seconds` with a sail area, sea
and wind; `steer` holds the helm over instead of keeping the heading.
-}
sailWith : { sea : Float, sail : Float, wind : Wind, steer : Maybe Float } -> Float -> Float -> SailingState
sailWith cfg heading seconds =
    let
        start =
            Physics.initSailingState course
    in
    List.foldl
        (\i s ->
            Physics.stepBoat
                { dt = 0.02
                , time = toFloat i * 0.02
                , steer = Maybe.withDefault (clamp -1 1 (-(Physics.angleDiff heading s.heading) / 10)) cfg.steer
                , throttle = 1
                , sea = cfg.sea
                , sail = cfg.sail
                }
                cfg.wind
                Boats.default
                []
                s
        )
        { start | heading = heading, sail = cfg.sail }
        (List.range 1 (round (seconds / 0.02)))


{-| A storm wind: twice the course wind.
-}
gale : Wind
gale =
    { speed = 14, direction = course.wind.direction }


{-| Heading across the course wind (the fastest point of sail).
-}
reach : Float
reach =
    course.wind.direction + 180 + 90


speedWith : Float -> Wind -> Float
speedWith area wind =
    (sailWith { sea = 1, sail = area, wind = wind, steer = Nothing } reach 10).speed


{-| Degrees turned in two seconds hard over.
-}
turnedWith : Float -> Wind -> Float
turnedWith area wind =
    let
        moving =
            sailWith { sea = 1, sail = area, wind = wind, steer = Nothing } reach 8

        after =
            List.foldl
                (\i s -> Physics.stepBoat { dt = 0.02, time = 8 + toFloat i * 0.02, steer = 1, throttle = 1, sea = 1, sail = area } wind Boats.default [] s)
                moving
                (List.range 1 100)
    in
    abs (Physics.angleDiff after.heading moving.heading)


{-| Autopilot round the course in a weather that keeps going, with a sail
area. Returns the progress when the course is done or `limit` runs out.
-}
raceIn : Weather -> Float -> Float -> CourseProgress
raceIn weather0 area limit =
    let
        go weather state progress =
            if progress.finished || progress.elapsedSeconds > limit then
                progress

            else
                let
                    goal =
                        List.drop progress.currentWaypointIndex course.waypoints
                            |> List.head
                            |> Maybe.map (.position >> Vec3.toRecord)
                            |> Maybe.withDefault { x = 0, y = 0, z = 0 }

                    p =
                        Vec3.toRecord state.position

                    bearing =
                        atan2 (goal.x - p.x) (goal.z - p.z) * 180 / pi

                    ( nextWeather, _ ) =
                        Weather.step 0.02 weather

                    now =
                        Weather.conditions course.wind nextWeather

                    next =
                        Physics.stepBoat
                            { dt = 0.02
                            , time = progress.elapsedSeconds
                            , steer = clamp -1 1 (-(Physics.angleDiff bearing state.heading) / 25)
                            , throttle = Physics.harbourThrottle course.waypoints progress state
                            , sea = now.sea
                            , sail = area
                            }
                            now.wind
                            Boats.default
                            course.islands
                            state

                    ( nextProgress, _ ) =
                        Physics.stepProgress 0.02 next course.waypoints progress
                in
                go nextWeather next nextProgress
    in
    go weather0 (Physics.initSailingState course) (Physics.initProgress course)


{-| The spells a seed's weather goes through in `seconds`, with how long
each lasted.
-}
spells : Int -> Float -> List ( Spell, Float )
spells seed seconds =
    List.foldl
        (\_ ( w, acc ) ->
            let
                ( next, _ ) =
                    Weather.step 0.1 w
            in
            case acc of
                ( spell, length ) :: rest ->
                    if next.spell == spell then
                        ( next, ( spell, length + 0.1 ) :: rest )

                    else
                        ( next, ( next.spell, 0.1 ) :: ( spell, length ) :: rest )

                [] ->
                    ( next, [ ( next.spell, 0.1 ) ] )
        )
        ( Weather.init seed, [] )
        (List.range 1 (round (seconds / 0.1)))
        |> Tuple.second
        |> List.reverse


{-| Shortest distance from a point to the segment a-b, on the water.
-}
segmentDistance : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float
segmentDistance ( px, pz ) ( ax, az ) ( bx, bz ) =
    let
        dx =
            bx - ax

        dz =
            bz - az

        t =
            clamp 0 1 (((px - ax) * dx + (pz - az) * dz) / max 1.0e-6 (dx * dx + dz * dz))
    in
    sqrt ((px - (ax + t * dx)) ^ 2 + (pz - (az + t * dz)) ^ 2)


{-| The buoy route as legs, from the start line through every buoy.
-}
legs : List ( ( Float, Float ), ( Float, Float ) )
legs =
    let
        pts =
            (course.startPosition :: List.map .position course.waypoints)
                |> List.map (\v -> ( Vec3.getX v, Vec3.getZ v ))
    in
    List.map2 Tuple.pair pts (List.drop 1 pts)


{-| Autopilot that detours to the treasure after harbour 4 and then finishes.
-}
treasureHunt : Float -> ( CourseProgress, List Int )
treasureHunt limit =
    let
        go state progress found =
            if progress.finished || progress.elapsedSeconds > limit then
                ( progress, found )

            else
                let
                    chest =
                        List.head course.treasures

                    goal =
                        case chest of
                            Just t ->
                                if progress.currentWaypointIndex == 4 && not (List.member t.id found) then
                                    Vec3.toRecord t.position

                                else
                                    buoyGoal

                            Nothing ->
                                buoyGoal

                    buoyGoal =
                        List.drop progress.currentWaypointIndex course.waypoints
                            |> List.head
                            |> Maybe.map (.position >> Vec3.toRecord)
                            |> Maybe.withDefault { x = 0, y = 0, z = 0 }

                    p =
                        Vec3.toRecord state.position

                    bearing =
                        atan2 (goal.x - p.x) (goal.z - p.z) * 180 / pi

                    next =
                        Physics.stepBoat
                            { dt = 0.02, time = progress.elapsedSeconds, steer = clamp -1 1 (-(Physics.angleDiff bearing state.heading) / 25), throttle = Physics.harbourThrottle course.waypoints progress state, sea = 1, sail = 1 }
                            course.wind
                            Boats.default
                            course.islands
                            state

                    ( nextProgress, _ ) =
                        Physics.stepProgress 0.02 next course.waypoints progress
                in
                go next nextProgress (found ++ List.map .id (Physics.reachedTreasure next course.treasures found))
    in
    go (Physics.initSailingState course) (Physics.initProgress course) []


suite : Test
suite =
    describe "Gameplay"
        [ describe "Sea"
            [ test "normal weather keeps the original swell between wave sets" <|
                \_ ->
                    let
                        original px pz pt =
                            0.22 * sin (2 * pi / 36 * (pz * 0.9 + px * 0.4) - 2 * pi / 36 * 3 * pt) + 0.1 * sin (2 * pi / 22 * (px * 0.8 - pz * 0.5) - 2 * pi / 22 * 2.4 * pt)

                        -- Out of a set: the set envelope is 1 there.
                        x =
                            0

                        z =
                            -50

                        t =
                            0
                    in
                    Expect.within (Expect.Absolute 0.001) (original x z t) (Physics.waterElevation x z t)
            , test "wave sets roll through: some waves are higher than the plain swell" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.greaterThan 0.34 (peakWave 1)
                        , \_ -> Expect.atMost 0.5 (peakWave 1)
                        ]
                        ()
            , test "a storm sea is much bigger, a calm one smaller" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.greaterThan (2 * peakWave 1) (peakWave 2.4)
                        , \_ -> Expect.lessThan (peakWave 1) (peakWave 0.6)
                        ]
                        ()
            , test "big head seas slow the boat; normal seas barely do" <|
                \_ ->
                    let
                        into =
                            Physics.swellHeading + 180

                        calm =
                            (holdCourse 1 into 12).speed

                        rough =
                            (holdCourse 2.4 into 12).speed
                    in
                    Expect.all
                        [ \_ -> Expect.lessThan (calm * 0.92) rough
                        , \_ -> Expect.greaterThan (calm * 0.6) rough
                        ]
                        ()
            , test "the hull pitches and rolls more in a storm sea" <|
                \_ ->
                    let
                        motion sea =
                            List.range 1 40
                                |> List.map (\i -> holdCourse sea 90 (toFloat i * 0.25))
                                |> List.map (\s -> abs s.heel + abs s.pitch)
                                |> List.maximum
                                |> Maybe.withDefault 0
                    in
                    Expect.greaterThan (motion 1 * 1.5) (motion 2.4)
            , test "the boat rides the water" <|
                \_ ->
                    let
                        s =
                            holdCourse 2.4 90 7

                        p =
                            Vec3.toRecord s.position
                    in
                    Expect.within (Expect.Absolute 0.001) (Physics.seaElevation 2.4 p.x p.z 7) p.y
            ]
        , describe "Sails"
            [ test "full sail in the course wind is the tuned sailing" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-9) 1 (Physics.sailDrive 1 course.wind)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 (Physics.overpower 1 course.wind)
                        , \_ -> Expect.equal ( "Full", 1.0 ) (Physics.sailLevel Physics.defaultSailLevel)
                        ]
                        ()
            , test "raising the sail is faster, lowering it slower" <|
                \_ ->
                    let
                        speeds =
                            List.map (\( _, area ) -> speedWith area course.wind) Physics.sailLevels
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.sort speeds) speeds
                        , \_ -> Expect.greaterThan (speedWith 1 course.wind * 1.05) (speedWith 1.2 course.wind)
                        , \_ -> Expect.lessThan (speedWith 1 course.wind * 0.85) (speedWith 0.65 course.wind)
                        , \_ -> Expect.greaterThan 0.5 (speedWith 0 course.wind)
                        ]
                        ()
            , test "less sail steers more easily, extra sail turns wider" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.greaterThan (turnedWith 1 course.wind) (turnedWith 0.35 course.wind)
                        , \_ -> Expect.lessThan (turnedWith 1 course.wind) (turnedWith 1.2 course.wind)
                        ]
                        ()
            , test "in a gale full sail is overpowered: it heels hard and reefing is faster" <|
                \_ ->
                    let
                        heelWith area =
                            abs (sailWith { sea = 1, sail = area, wind = gale, steer = Nothing } reach 10).heel
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 1 (Physics.overpower 1 gale)
                        , \_ -> Expect.greaterThan (speedWith 1 gale) (speedWith 0.65 gale)
                        , \_ -> Expect.greaterThan (heelWith 0.65 + 3) (heelWith 1)
                        , \_ -> Expect.greaterThan (turnedWith 1 gale) (turnedWith 0.65 gale)
                        ]
                        ()
            , test "the sail eases to a new setting over a second or two" <|
                \_ ->
                    let
                        start =
                            Physics.initSailingState course

                        stepped n =
                            List.foldl
                                (\i s -> Physics.stepBoat { dt = 0.02, time = toFloat i * 0.02, steer = 0, throttle = 1, sea = 1, sail = 0.35 } course.wind Boats.default [] s)
                                start
                                (List.range 1 n)
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 0.6 (stepped 10).sail
                        , \_ -> Expect.within (Expect.Absolute 0.05) 0.35 (stepped 200).sail
                        ]
                        ()
            ]
        , describe "Storms"
            [ test "fair weather is the course wind and the normal sea" <|
                \_ ->
                    let
                        now =
                            Weather.conditions course.wind (Weather.init 42)
                    in
                    Expect.equal ( now.wind, now.sea, now.rain ) ( course.wind, 1, 0 )
            , test "storms raise the wind and the waves" <|
                \_ ->
                    let
                        windOf spell =
                            (Weather.conditions course.wind (Weather.developed spell)).wind.speed

                        seaOf spell =
                            (Weather.conditions course.wind (Weather.developed spell)).sea
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan (course.wind.speed * 1.5) (windOf Thunderstorm)
                        , \_ -> Expect.greaterThan (course.wind.speed * 1.3) (windOf Squall)
                        , \_ -> Expect.lessThan course.wind.speed (windOf Lull)
                        , \_ -> Expect.atLeast 2.2 (seaOf Thunderstorm)
                        , \_ -> Expect.greaterThan 1.4 (seaOf Squall)
                        , \_ -> Expect.lessThan 1 (seaOf Lull)
                        , \_ -> Expect.greaterThan (2 * peakWave 1) (peakWave (seaOf Thunderstorm))
                        ]
                        ()
            , test "storms start at random and end after a while" <|
                \_ ->
                    let
                        runs =
                            List.map (\seed -> spells seed 600) [ 1, 2, 3, 4, 5, 6, 7, 8 ]

                        storms =
                            List.concat runs |> List.filter (\( s, _ ) -> s /= Fair)

                        -- The last spell of a run may be cut short by the end of the run.
                        finished =
                            List.concatMap (List.reverse >> List.drop 1) runs
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (List.all (\r -> List.any (\( s, _ ) -> s /= Fair) r) runs)
                        , \_ -> Expect.equal True (List.all (\( s, l ) -> s == Fair || l <= 26.2) finished)
                        , \_ -> Expect.equal True (List.all (\r -> List.head r |> Maybe.map (\( s, l ) -> s == Fair && l >= 9.9) |> Maybe.withDefault False) runs)
                        , \_ -> Expect.equal True (List.member Squall (List.map Tuple.first storms) && List.member Thunderstorm (List.map Tuple.first storms) && List.member Lull (List.map Tuple.first storms))
                        , \_ -> Expect.notEqual (spells 1 300) (spells 2 300)
                        ]
                        ()
            , test "a calm before the storm is always followed by a thunderstorm" <|
                \_ ->
                    let
                        pairs r =
                            List.map2 Tuple.pair r (List.drop 1 r)
                    in
                    List.range 1 12
                        |> List.concatMap (\seed -> pairs (List.map Tuple.first (spells seed 600)))
                        |> List.filter (\( a, _ ) -> a == Lull)
                        |> List.all (\( _, b ) -> b == Thunderstorm)
                        |> Expect.equal True
            , test "the same seed always gives the same weather" <|
                \_ ->
                    Expect.equal (spells 99 400) (spells 99 400)
            , test "lightning strikes in a thunderstorm, never in a squall" <|
                \_ ->
                    let
                        bolts w =
                            List.foldl
                                (\_ ( s, n ) ->
                                    let
                                        ( next, struck ) =
                                            Weather.step 0.02 s
                                    in
                                    ( next
                                    , if struck then
                                        n + 1

                                      else
                                        n
                                    )
                                )
                                ( w, 0 )
                                (List.range 1 1000)
                                |> Tuple.second
                    in
                    Expect.all
                        [ \_ -> Expect.atLeast 2 (bolts (Weather.developed Thunderstorm))
                        , \_ -> Expect.equal 0 (bolts (Weather.developed Squall))
                        ]
                        ()
            , test "the course stays finishable in a thunderstorm that never ends" <|
                \_ ->
                    let
                        reefed =
                            raceIn (Weather.developed Thunderstorm) 0.65 400

                        full =
                            raceIn (Weather.developed Thunderstorm) 1 400

                        fairRace =
                            raceIn (Weather.init 1) 1 400
                    in
                    Expect.all
                        [ \_ -> Expect.equal True reefed.finished
                        , \_ -> Expect.equal True full.finished
                        , \_ -> Expect.atMost (course.targetSeconds * 3) reefed.elapsedSeconds
                        , \_ -> Expect.greaterThan fairRace.elapsedSeconds reefed.elapsedSeconds
                        ]
                        ()
            ]
        , describe "Islands, towns and treasure"
            [ test "every island is clear of the route, bar the islets the legs' harbours lie off" <|
                \_ ->
                    let
                        -- An island a leg's end docks at: its shore within 6 m.
                        docksAt ( px, pz ) isl =
                            sqrt ((px - isl.x) ^ 2 + (pz - isl.z) ^ 2) <= isl.radius + 6

                        -- 8 m clear of every other island, and outside the
                        -- rock the boat bumps into (2 m off the shore) of an
                        -- islet it docks at.
                        tooClose ( a, b ) isl =
                            if docksAt a isl || docksAt b isl then
                                segmentDistance ( isl.x, isl.z ) a b <= isl.radius + 2

                            else
                                segmentDistance ( isl.x, isl.z ) a b < isl.radius + 8
                    in
                    course.islands
                        |> List.filter (\isl -> List.any (\leg -> tooClose leg isl) legs)
                        |> Expect.equal []
            , test "the start is in open water and every harbour lies just off an island's shore" <|
                \_ ->
                    let
                        distance v isl =
                            sqrt ((Vec3.getX v - isl.x) ^ 2 + (Vec3.getZ v - isl.z) ^ 2)

                        offShore v =
                            List.any (\isl -> distance v isl > isl.radius + 2 && distance v isl <= isl.radius + 6) course.islands
                    in
                    Expect.all
                        [ \_ ->
                            course.islands
                                |> List.filter (\isl -> distance course.startPosition isl < isl.radius + 10)
                                |> Expect.equal []
                        , \_ ->
                            course.waypoints
                                |> List.filter (\wp -> not (offShore wp.position))
                                |> List.map .index
                                |> Expect.equal []
                        , \_ ->
                            course.waypoints
                                |> List.filter (\wp -> List.any (\isl -> distance wp.position isl <= isl.radius + 2) course.islands)
                                |> List.map .index
                                |> Expect.equal []
                        ]
                        ()
            , test "towns stand on islands of the course and a treasure island has a chest" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.atLeast 1 (List.length course.towns)
                        , \_ -> Expect.equal True (List.all (\t -> List.member t.island course.islands) course.towns)
                        , \_ -> Expect.atLeast 1 (List.length course.treasures)
                        , \_ ->
                            course.treasures
                                |> List.all
                                    (\t ->
                                        List.any
                                            (\isl ->
                                                let
                                                    d =
                                                        sqrt ((Vec3.getX t.position - isl.x) ^ 2 + (Vec3.getZ t.position - isl.z) ^ 2)
                                                in
                                                d < isl.radius && d + Physics.treasureReach > isl.radius + 2
                                            )
                                            course.islands
                                    )
                                |> Expect.equal True
                        ]
                        ()
            , test "the treasure can be reached from the route and the course still finished" <|
                \_ ->
                    let
                        ( progress, found ) =
                            treasureHunt 200

                        withTreasure =
                            { progress | treasureFound = found, treasureBonus = List.sum (List.map .bonusSeconds course.treasures) }
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.map .id course.treasures) found
                        , \_ -> Expect.equal True progress.finished
                        , \_ -> Expect.within (Expect.Absolute 1.0e-6) (Physics.finalTime progress - 6) (Physics.finalTime withTreasure)
                        ]
                        ()
            , test "the scenery is built from catalog bricks and stands on the ground" <|
                \_ ->
                    let
                        catalogParts =
                            [ "3001.dat", "3004.dat", "3005.dat", "3020.dat", "3024.dat", "3034.dat", "3037.dat", "3062b.dat", "3460.dat", "3957a.dat", "2335.dat", "6141.dat" ]

                        box =
                            Parser.parseFile "4 16 -10 0 -10 10 0 -10 10 24 10 -10 24 10"

                        cache =
                            catalogParts |> List.map (\n -> ( n, Loaded box )) |> Dict.fromList

                        parts piece =
                            Document.flattenedPlacements (Document.parse piece.ldraw) |> List.map .part

                        mesh piece =
                            ShipMesh.buildScenery Scenery.metersPerLdu (Document.parse piece.ldraw) cache
                    in
                    Expect.all
                        [ \_ -> Expect.equal [] (List.concatMap parts Scenery.all |> List.filter (\p -> not (List.member p catalogParts)))
                        , \_ -> Expect.equal True (List.all (\piece -> List.length (parts piece) >= 10) Scenery.all)
                        , \_ -> Expect.equal True (List.all (\piece -> (mesh piece).heightMeters > 2 && (mesh piece).lengthMeters < 20) Scenery.all)
                        ]
                        ()
            ]
        ]
