module SailsTest exposing (suite)

{-| Unit tests of Brick Sails: loading and analysing builds, the built-in
boats, handling, water, sailing, buoys and scoring, cannons and the follow
camera.
-}

import Dict
import Expect
import LDraw.Document as Document
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Analyze as Analyze exposing (Seaworthiness(..))
import Sails.Boats as Boats
import Sails.CameraRig as CameraRig
import Sails.Handling as Handling
import Sails.Ingest as Ingest
import Sails.Physics as Physics
import Sails.ShipMesh as ShipMesh
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


{-| A small ship built in the brick building app and shared as a binary
build link: inverted-slope hulls, a 2 x 2 brick, tiles, an antenna mast,
a flag and a jumper plate (8 parts).
-}
playBuildHash : String
playBuildHash =
    "#b/FYixDYMwEADvwR-lI0XK7JBXXjaO6ChooaKHKRiHkg1YD1PcnXT9IDzUU7I1uJmrW8pr-LlH9X9sF81tzBZmPkDDs5jp-waBrQBdVdKMt9mlruCUMdRwCFrmiws"


{-| Every part file a built-in boat may use.
-}
knownParts : List String
knownParts =
    [ "3001.dat", "3003.dat", "3004.dat", "3005.dat", "3008.dat", "3009.dat", "3010.dat", "3622.dat", "3660b.dat", "3068b.dat", "3460.dat", "87580.dat", "43888.dat", "3957a.dat", "2335.dat", "2431.dat", "6636.dat" ]


{-| A stand-in part: one square, 20 LDU wide and 24 LDU tall, so geometry
can be built without the network.
-}
boxPart : List LDrawLine
boxPart =
    Parser.parseFile "4 16 -10 0 -10 10 0 -10 10 24 10 -10 24 10"


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


{-| Part names and origins of a brick model, as `Ingest.ingestDocument` reads them.
-}
placementsOf : String -> List ( String, Vec3.Vec3 )
placementsOf ldr =
    Document.flattenedPlacements (Document.parse ldr)
        |> List.map (\p -> ( p.part, Mat4.transform p.matrix (vec3 0 0 0) ))


{-| The sloop's bricks with `n` extra flags hung on the mast.
-}
sloopWithFlags : Int -> List ( String, Vec3.Vec3 )
sloopWithFlags n =
    placementsOf Boats.default.ldraw
        ++ List.map (\i -> ( "2335.dat", vec3 0 (-60 - 24 * toFloat i) 0 )) (List.range 1 n)


inRange : ( Float, Float ) -> Float -> Expect.Expectation
inRange ( lo, hi ) v =
    Expect.all [ Expect.atLeast lo, Expect.atMost hi ] v


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
        , describe "Loading ships from build links"
            [ test "a ship shared from the building app decodes and floats" <|
                \_ ->
                    case Ingest.decodeShipInput Nothing playBuildHash of
                        Ok decoded ->
                            case Ingest.validateShip decoded.document of
                                Ok spec ->
                                    Expect.all
                                        [ \sp -> Expect.equal 8 sp.totalPartCount
                                        , \sp -> Expect.atLeast 2 sp.hullPartCount
                                        , \sp -> Expect.equal 1 sp.sailPartCount
                                        ]
                                        spec

                                Err err ->
                                    Expect.fail ("valid build was refused: " ++ err)

                        Err err ->
                            Expect.fail ("valid link did not decode: " ++ err)
            , test "the same link inside a full page URL with other fields still decodes" <|
                \_ ->
                    Ingest.decodeShipInput Nothing ("https://example.invalid/sails/?x=1" ++ String.replace "#b/" "#lang=en&b/" playBuildHash)
                        |> Result.map (.document >> Document.flattenedPlacements >> List.length)
                        |> Expect.equal (Ok 8)
            , test "a damaged link returns an error message, not a crash" <|
                \_ ->
                    case Ingest.decodeShipInput Nothing "#b/not-a-real-build" of
                        Err err ->
                            Expect.notEqual "" err

                        Ok _ ->
                            Expect.fail "damaged link was accepted"
            , test "text that is not a link returns an error message" <|
                \_ ->
                    Ingest.decodeShipInput Nothing "hello there"
                        |> Result.map .title
                        |> Expect.err
            , test "a build without a hull is refused as not a boat" <|
                \_ ->
                    case Ingest.decodeShipInput Nothing "0 Name: Pole\n1 4 0 -40 0 1 0 0 0 1 0 0 0 1 3957a.dat\n1 15 0 -120 0 1 0 0 0 1 0 0 0 1 2335.dat" of
                        Ok decoded ->
                            case Ingest.validateShip decoded.document of
                                Err err ->
                                    Expect.equal True (String.startsWith "Not a boat" err)

                                Ok _ ->
                                    Expect.fail "a mast and a flag were accepted as a boat"

                        Err err ->
                            Expect.fail err
            , test "a loaded build is shared as a build link that decodes to the same bricks" <|
                \_ ->
                    case Ingest.decodeShipInput Nothing playBuildHash of
                        Ok decoded ->
                            case Ingest.decodeSharedShip (Ingest.encodeShipLink decoded.document) of
                                Ingest.SharedBuild shared ->
                                    Expect.equal 8 (List.length (Document.flattenedPlacements shared.document))

                                _ ->
                                    Expect.fail "a shared build link was not read back as a build"

                        Err err ->
                            Expect.fail err
            , test "a built-in boat is shared by name" <|
                \_ ->
                    case Ingest.decodeSharedShip "Cutter" of
                        Ingest.SharedBoat name ->
                            Expect.equal "Cutter" name

                        _ ->
                            Expect.fail "a boat name was not recognised"
            , test "a shared build over the part limit is not used" <|
                \_ ->
                    let
                        bigBuild =
                            "0 Name: Raft\n"
                                ++ String.join "\n"
                                    (List.repeat (Ingest.maxShipParts + 1) "1 4 0 -24 0 1 0 0 0 1 0 0 0 1 3001.dat")
                    in
                    case Ingest.decodeSharedShip bigBuild of
                        Ingest.SharedUnusable err ->
                            Expect.equal True (String.contains "too big" err)

                        _ ->
                            Expect.fail "a build over the part limit was accepted"
            , test "the ship link check spots both link forms" <|
                \_ ->
                    Expect.equal [ True, True, False ]
                        (List.map Ingest.hasShipPayload [ playBuildHash, "#model=1%2016%200%200%200%201%200%200%200%201%200%200%200%201%203001.dat", "#room=ABCD" ])
            ]
        , describe "Built-in brick boats"
            [ test "every boat is a brick model of known parts with sails, mast and hull" <|
                \_ ->
                    Boats.all
                        |> List.map
                            (\b ->
                                let
                                    placements =
                                        Document.flattenedPlacements (Document.parse b.ldraw)

                                    sails =
                                        List.filter (\p -> Just p.color == b.sailColour) placements
                                in
                                { parts = List.length placements >= 25
                                , known = List.all (\p -> List.member p.part knownParts) placements
                                , sails = List.length sails >= 5
                                , floats = Ingest.validateShip (Document.parse b.ldraw) |> Result.map (\_ -> True) |> Result.withDefault False
                                }
                            )
                        |> Expect.equal (List.repeat 4 { parts = True, known = True, sails = True, floats = True })
            , test "boat geometry is scaled to the course and sits on the waterline" <|
                \_ ->
                    let
                        doc =
                            Document.parse Boats.default.ldraw

                        cache =
                            knownParts |> List.map (\n -> ( n, Loaded boxPart )) |> Dict.fromList

                        mesh =
                            ShipMesh.build (ShipMesh.sailByColour 15) doc cache

                        ys =
                            mesh.scene.surface
                                |> List.concatMap (\( a, b, c ) -> [ a, b, c ])
                                |> List.map (\v -> Vec3.getY (Mat4.transform mesh.toBoat v.position))

                        sailVertices =
                            mesh.scene.surface |> List.filter (\( a, _, _ ) -> a.inherit > 0.5) |> List.length
                    in
                    Expect.all
                        [ \_ -> Expect.atMost 4.6 mesh.lengthMeters
                        , \_ -> Expect.atLeast 3.5 mesh.lengthMeters
                        , \_ -> Expect.atMost 9.01 mesh.heightMeters
                        , \_ -> Expect.all [ Expect.lessThan -0.05, Expect.greaterThan -0.4 ] (List.minimum ys |> Maybe.withDefault 0)
                        , \_ -> Expect.greaterThan 3 (List.maximum ys |> Maybe.withDefault 0)
                        , \_ -> Expect.greaterThan 0 sailVertices
                        , \_ -> Expect.equal [] (ShipMesh.partProgress doc cache).needed
                        ]
                        ()
            , test "a boat lists its missing parts before anything is fetched" <|
                \_ ->
                    (ShipMesh.partProgress (Document.parse Boats.default.ldraw) Dict.empty).needed
                        |> List.length
                        |> Expect.atLeast 8
            ]
        , describe "Loaded build handling"
            [ test "a build with more sail accelerates harder and turns wider than one with less" <|
                \_ ->
                    let
                        small =
                            Ingest.ingestFromPlacements "Small rig" (sloopWithFlags 0) |> Handling.fromSpec

                        big =
                            Ingest.ingestFromPlacements "Big rig" (sloopWithFlags 4) |> Handling.fromSpec
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan (small.acceleration + 0.1) big.acceleration
                        , \_ -> Expect.lessThan (small.turnRate - 3) big.turnRate
                        , \_ -> Expect.atLeast small.topSpeed big.topSpeed
                        ]
                        ()
            , test "the extra sail makes the boat quicker off the line in the race physics" <|
                \_ ->
                    let
                        boatFrom n =
                            Boats.custom "" (Ingest.ingestFromPlacements "" (sloopWithFlags n) |> Handling.fromSpec)

                        start =
                            Physics.initSailingState course

                        after n =
                            (sail (boatFrom n) 0 1.0 start).speed
                    in
                    Expect.greaterThan (after 0 + 0.2) (after 4)
            , test "the sloop's own bricks derive the sloop's handling" <|
                \_ ->
                    let
                        h =
                            Handling.fromSpec (Ingest.ingestDocument "Sloop" (Document.parse Boats.default.ldraw))
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.1) Boats.default.topSpeed h.topSpeed
                        , \_ -> Expect.within (Expect.Absolute 1) Boats.default.turnRate h.turnRate
                        , \_ -> Expect.within (Expect.Absolute 0.02) Boats.default.acceleration h.acceleration
                        ]
                        ()
            , test "every derived value stays within the clamped range" <|
                \_ ->
                    let
                        builds =
                            [ sampleShipPlacements
                            , sloopWithFlags 0
                            , sloopWithFlags 30
                            , [ ( "3001.dat", vec3 0 -24 0 ), ( "3001.dat", vec3 0 -24 40 ), ( "3957a.dat", vec3 0 -40 0 ) ]
                            , List.map (\i -> ( "3001.dat", vec3 (toFloat (modBy 20 i) * 40) -24 (toFloat (i // 20) * 80) )) (List.range 0 199)
                            ]
                                ++ List.map (\b -> placementsOf b.ldraw) Boats.all

                        derived =
                            List.map (Ingest.ingestFromPlacements "Build" >> Handling.fromSpec) builds
                    in
                    Expect.all
                        (List.concatMap
                            (\h ->
                                [ \_ -> inRange Handling.limits.topSpeed h.topSpeed
                                , \_ -> inRange Handling.limits.turnRate h.turnRate
                                , \_ -> inRange Handling.limits.acceleration h.acceleration
                                ]
                            )
                            derived
                        )
                        ()
            , test "the four built-in boats keep their handling" <|
                \_ ->
                    Boats.all
                        |> List.map (\b -> ( b.name, ( b.topSpeed, b.turnRate, b.acceleration ) ))
                        |> Expect.equal
                            [ ( "Sloop", ( 9.0, 95.0, 1.3 ) )
                            , ( "Cutter", ( 8.6, 125.0, 1.3 ) )
                            , ( "Schooner", ( 9.5, 85.0, 1.3 ) )
                            , ( "Catamaran", ( 10.0, 72.0, 1.3 ) )
                            ]
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
            , test "camera looks at mid-mast height from above the tallest brick masthead, off to one side" <|
                \_ ->
                    let
                        view =
                            CameraRig.followView (CameraRig.initFollow (vec3 0 0 0) 0)

                        eye =
                            Vec3.toRecord view.eye

                        target =
                            Vec3.toRecord view.target
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 9.5 eye.y
                        , \_ -> Expect.within (Expect.Absolute 1.5) 3.5 target.y
                        , \_ -> Expect.lessThan -20 eye.z
                        , \_ -> Expect.notWithin (Expect.Absolute 3) 0 eye.x
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
            , test "camera chases close behind the boat, a few boat lengths back" <|
                \_ ->
                    let
                        boat =
                            vec3 0 0 0

                        settle next =
                            List.foldl (\_ f -> CameraRig.stepFollow 0.05 1.5 boat 0 next f)
                                (CameraRig.initFollow boat 0)
                                (List.range 1 100)

                        horizontal f =
                            let
                                eye =
                                    Vec3.toRecord (CameraRig.followView f).eye
                            in
                            sqrt (eye.x * eye.x + eye.z * eye.z)
                    in
                    Expect.all
                        [ \_ -> Expect.atMost 26 (horizontal (settle Nothing))
                        , \_ -> Expect.atMost 34 (horizontal (settle (Just (vec3 0 0 300))))
                        ]
                        ()
            ]
        ]
