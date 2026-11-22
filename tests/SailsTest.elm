module SailsTest exposing (suite)

import Expect
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Analyze as Analyze exposing (Seaworthiness(..))
import Sails.Boats as Boats
import Sails.CameraRig as CameraRig
import Sails.Ingest as Ingest
import Sails.Physics as Physics
import Sails.Types exposing (Boat, CourseProgress, SailingState)
import Test exposing (Test, describe, test)


sampleShipPlacements : List ( String, Vec3.Vec3 )
sampleShipPlacements =
    [ ( "3660b.dat", vec3 40.0 -24.0 10.0 )
    , ( "3660b.dat", vec3 40.0 -24.0 70.0 )
    , ( "3003.dat", vec3 40.0 -24.0 40.0 )
    , ( "3068b.dat", vec3 40.0 -32.0 0.0 )
    , ( "3068b.dat", vec3 40.0 -32.0 80.0 )
    , ( "2335.dat", vec3 40.0 -124.0 40.0 )
    , ( "3957a.dat", vec3 40.0 -40.0 40.0 )
    , ( "87580.dat", vec3 40.0 -32.0 40.0 )
    ]


course : Sails.Types.Course
course =
    Physics.defaultCourse


{-| Run the boat for `seconds` with a fixed steering input.
-}
sail : Boat -> Float -> Float -> SailingState -> SailingState
sail boat steer seconds state0 =
    let
        frames =
            round (seconds / 0.02)
    in
    List.foldl
        (\i s ->
            Physics.stepBoat { dt = 0.02, time = toFloat i * 0.02, steer = steer, throttle = 1 } course.wind boat course.islands s
        )
        state0
        (List.range 1 frames)


{-| Simple autopilot: steer at the next buoy until the course is finished
or the time limit runs out. Returns the progress.
-}
autopilot : Boat -> Float -> CourseProgress
autopilot boat limit =
    let
        go state progress =
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

                    steer =
                        clamp -1 1 (-(Physics.angleDiff bearing state.heading) / 25)

                    next =
                        Physics.stepBoat { dt = 0.02, time = progress.elapsedSeconds, steer = steer, throttle = 1 } course.wind boat course.islands state

                    ( nextProgress, _ ) =
                        Physics.stepProgress 0.02 next course.waypoints progress
                in
                go next nextProgress
    in
    go (Physics.initSailingState course) (Physics.initProgress course)


suite : Test
suite =
    describe "Brick Sails Test Suite"
        [ describe "Ship Ingestion & Geometry"
            [ test "ingests micro sloop parts correctly" <|
                \_ ->
                    let
                        spec =
                            Ingest.ingestFromPlacements "Micro Sloop" sampleShipPlacements
                    in
                    Expect.all
                        [ \s -> Expect.equal 8 s.totalPartCount
                        , \s -> Expect.greaterThan 0 s.hullPartCount
                        , \s -> Expect.equal 1 s.mastPartCount
                        , \s -> Expect.equal 1 s.sailPartCount
                        , \s -> Expect.greaterThan 0.5 s.metacentricHeight
                        , \s -> Expect.greaterThan 2.0 s.lengthMeters
                        ]
                        spec
            ]
        , describe "Seaworthiness & Architecture Analysis"
            [ test "analyzes micro sloop as seaworthy" <|
                \_ ->
                    Ingest.ingestFromPlacements "Micro Sloop" sampleShipPlacements
                        |> Analyze.analyzeShip
                        |> .seaworthiness
                        |> Expect.equal Seaworthy
            , test "detects missing hull as unseaworthy" <|
                \_ ->
                    let
                        report =
                            Ingest.ingestFromPlacements "Ghost Rig"
                                [ ( "3957a.dat", vec3 0.0 -40.0 0.0 )
                                , ( "2335.dat", vec3 0.0 -120.0 0.0 )
                                ]
                                |> Analyze.analyzeShip
                    in
                    case report.seaworthiness of
                        Unseaworthy _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected unseaworthy rating for vessel without hull"
            ]
        , describe "Water"
            [ test "waterElevation stays a gentle swell" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.5) 0.0 (Physics.waterElevation 0.0 0.0 0.0)
                        , \_ -> Expect.within (Expect.Absolute 0.5) 0.0 (Physics.waterElevation 10.0 20.0 1.5)
                        ]
                        ()
            ]
        , describe "Arcade sailing"
            [ test "wind factor is forgiving: never below 0.6, best across the wind" <|
                \_ ->
                    let
                        factors =
                            List.range 0 71 |> List.map (\i -> Physics.windFactor course.wind (toFloat i * 5))

                        from =
                            course.wind.direction + 180

                        acrossWind =
                            Physics.windFactor course.wind (from + 90)

                        intoWind =
                            Physics.windFactor course.wind from
                    in
                    Expect.all
                        [ \_ -> Expect.atLeast 0.6 (List.minimum factors |> Maybe.withDefault 0)
                        , \_ -> Expect.greaterThan intoWind acrossWind
                        ]
                        ()
            , test "boat keeps moving even when pointed straight into the wind" <|
                \_ ->
                    let
                        start =
                            Physics.initSailingState course

                        intoWind =
                            { start | heading = course.wind.direction + 180 - 360, position = vec3 0 0 60 }

                        after =
                            sail Boats.default 0 4 intoWind
                    in
                    Expect.greaterThan (Boats.default.topSpeed * 0.5) after.speed
            , test "holding right turns the boat to the right (heading goes down)" <|
                \_ ->
                    let
                        start =
                            Physics.initSailingState course

                        moving =
                            { start | heading = 90, speed = 8, position = vec3 0 0 60 }

                        after =
                            sail Boats.default 1 0.5 moving
                    in
                    Expect.lessThan 90 after.heading
            , test "islands push the boat out instead of letting it sail through" <|
                \_ ->
                    let
                        island =
                            List.head course.islands |> Maybe.withDefault { x = 0, z = 0, radius = 10, height = 1 }

                        start =
                            Physics.initSailingState course

                        -- Aim straight at the island centre from the south.
                        aimed =
                            { start | heading = 0, speed = 9, position = vec3 island.x 0 (island.z - island.radius - 10) }

                        after =
                            sail Boats.default 0 5 aimed

                        p =
                            Vec3.toRecord after.position

                        dist =
                            sqrt ((p.x - island.x) ^ 2 + (p.z - island.z) ^ 2)
                    in
                    Expect.atLeast island.radius dist
            , test "every boat can finish the course on autopilot within the 2-star time" <|
                \_ ->
                    Boats.all
                        |> List.map (\b -> autopilot b 200)
                        |> Expect.all
                            (List.indexedMap
                                (\i _ list ->
                                    case List.drop i list |> List.head of
                                        Just p ->
                                            Expect.all
                                                [ \pr -> Expect.equal True pr.finished
                                                , \pr -> Expect.atMost (course.targetSeconds * 1.3) pr.elapsedSeconds
                                                ]
                                                p

                                        Nothing ->
                                            Expect.fail "missing run"
                                )
                                Boats.all
                            )
            , test "a clean autopilot run without hitting targets is not already 3 stars" <|
                \_ ->
                    Boats.all
                        |> List.map (\b -> (autopilot b 200).elapsedSeconds)
                        |> List.minimum
                        |> Maybe.withDefault 0
                        |> Expect.greaterThan course.targetSeconds
            ]
        , describe "Buoys and scoring"
            [ test "passing within a buoy's radius ticks it off; the last one finishes" <|
                \_ ->
                    let
                        start =
                            Physics.initSailingState course

                        progress0 =
                            Physics.initProgress course

                        last =
                            List.length course.waypoints - 1

                        atLast =
                            List.drop last course.waypoints |> List.head |> Maybe.map .position |> Maybe.withDefault (vec3 0 0 0)

                        ( p1, passed ) =
                            Physics.stepProgress 0.1 { start | position = atLast } course.waypoints { progress0 | currentWaypointIndex = last }

                        ( p2, notPassed ) =
                            Physics.stepProgress 0.1 { start | position = vec3 500 0 500 } course.waypoints progress0
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just last) passed
                        , \_ -> Expect.equal True p1.finished
                        , \_ -> Expect.equal Nothing notPassed
                        , \_ -> Expect.equal 0 p2.currentWaypointIndex
                        ]
                        ()
            , test "each target hit takes time off and stars follow the target time" <|
                \_ ->
                    let
                        progress0 =
                            Physics.initProgress course

                        run =
                            { progress0 | elapsedSeconds = course.targetSeconds + 4, targetsHit = 2, finished = True }
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) (course.targetSeconds + 4 - 2 * Physics.bonusPerHit) (Physics.finalTime run)
                        , \_ -> Expect.equal 3 (Physics.starRating course.targetSeconds (Physics.finalTime run))
                        , \_ -> Expect.equal 2 (Physics.starRating course.targetSeconds (course.targetSeconds * 1.2))
                        , \_ -> Expect.equal 1 (Physics.starRating course.targetSeconds (course.targetSeconds * 2))
                        ]
                        ()
            ]
        , describe "Cannons"
            [ test "firing near a target lobs a ball that sinks it" <|
                \_ ->
                    let
                        target =
                            { id = 1, position = vec3 15 0 5, sunk = False, sinkAge = 0 }

                        start =
                            Physics.initSailingState course

                        balls =
                            Physics.fireCannons 1 { start | position = vec3 0 0 0 } [ target.position ] 1

                        fly i ( bs, ts, hits ) =
                            let
                                ( flying, _ ) =
                                    Physics.stepProjectiles 0.02 0 bs

                                ( left, _, ( ts2, n ) ) =
                                    Physics.checkTargetHits flying ts
                            in
                            ( left, ts2, hits + n + 0 * i )

                        ( _, finalTargets, totalHits ) =
                            List.foldl fly ( balls, [ target ], 0 ) (List.range 1 150)
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (List.length balls)
                        , \_ -> Expect.equal 1 totalHits
                        , \_ -> Expect.equal [ True ] (List.map .sunk finalTargets)
                        ]
                        ()
            , test "with nothing in range a broadside goes out both sides" <|
                \_ ->
                    Physics.fireCannons 1 (Physics.initSailingState course) [ vec3 500 0 500 ] 1
                        |> List.length
                        |> Expect.equal 2
            ]
        , describe "Follow camera"
            [ test "camera sits behind and above the boat" <|
                \_ ->
                    let
                        boat =
                            vec3 10 0 20

                        follow =
                            CameraRig.initFollow boat 90

                        view =
                            CameraRig.followView follow

                        eye =
                            Vec3.toRecord view.eye
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 10 eye.y
                        , \_ -> Expect.lessThan 10 eye.x
                        ]
                        ()
            , test "camera turns towards the boat's heading over time" <|
                \_ ->
                    let
                        boat =
                            vec3 0 0 0

                        stepped =
                            List.foldl (\_ f -> CameraRig.stepFollow 0.05 1.0 boat 120 Nothing f)
                                (CameraRig.initFollow boat 0)
                                (List.range 1 60)
                    in
                    Expect.within (Expect.Absolute 2) 120 stepped.yaw
            ]
        ]
