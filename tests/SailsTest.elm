module SailsTest exposing (suite)

import Expect
import Game.Compass as Compass exposing (PointOfSail(..))
import Math.Vector3 exposing (vec3)
import Sails.Analyze as Analyze exposing (Seaworthiness(..))
import Sails.Ingest as Ingest
import Sails.Physics as Physics
import Sails.Types exposing (CourseId(..), ShipSpec)
import Test exposing (Test, describe, test)


sampleShipPlacements : List ( String, Math.Vector3.Vec3 )
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
                    let
                        spec =
                            Ingest.ingestFromPlacements "Micro Sloop" sampleShipPlacements

                        report =
                            Analyze.analyzeShip spec
                    in
                    Expect.equal Seaworthy report.seaworthiness
            , test "detects missing hull as unseaworthy" <|
                \_ ->
                    let
                        spec =
                            Ingest.ingestFromPlacements "Ghost Rig"
                                [ ( "3957a.dat", vec3 0.0 -40.0 0.0 )
                                , ( "2335.dat", vec3 0.0 -120.0 0.0 )
                                ]

                        report =
                            Analyze.analyzeShip spec
                    in
                    case report.seaworthiness of
                        Unseaworthy _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected unseaworthy rating for vessel without hull"
            ]
        , describe "Hydrodynamics & Wave Physics"
            [ test "waterElevation computes oscillating wave displacement" <|
                \_ ->
                    let
                        h0 =
                            Physics.waterElevation 0.0 0.0 0.0

                        h1 =
                            Physics.waterElevation 10.0 20.0 1.5
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0) 0.0 h0
                        , \_ -> Expect.within (Expect.Absolute 1.0) 0.0 h1
                        ]
                        ()
            , test "ship accelerates forward when close-hauled in a breeze" <|
                \_ ->
                    let
                        spec =
                            Ingest.ingestFromPlacements "Micro Sloop" sampleShipPlacements

                        wind =
                            { speed = 8.0, direction = 180.0 }

                        initState =
                            Physics.initSailingState Physics.defaultCourse

                        -- Heading 45° to wind (close-hauled)
                        closeHauledState =
                            { initState | heading = 45.0, sailSheet = 0.25 }

                        stepped =
                            Physics.stepPhysics 0.1 0.1 wind spec closeHauledState
                    in
                    Expect.greaterThan 0.0 stepped.waterSpeed
            , test "rudder deflects ship heading when moving through water" <|
                \_ ->
                    let
                        spec =
                            Ingest.ingestFromPlacements "Micro Sloop" sampleShipPlacements

                        wind =
                            { speed = 8.0, direction = 180.0 }

                        movingState =
                            { position = vec3 0.0 0.0 0.0
                            , velocity = vec3 0.0 0.0 3.0
                            , heading = 0.0
                            , angularVelocity = 0.0
                            , heelAngle = 0.0
                            , pitchAngle = 0.0
                            , heave = 0.0
                            , rudderAngle = 20.0 -- 20° right rudder
                            , sailSheet = 0.5
                            , apparentWindSpeed = 8.0
                            , apparentWindAngle = 90.0
                            , waterSpeed = 6.0
                            , driftAngle = 0.0
                            }

                        stepped =
                            Physics.stepPhysics 0.1 0.1 wind spec movingState
                    in
                    -- Rudder should generate angular turning velocity
                    Expect.notEqual 0.0 stepped.angularVelocity
            ]
        ]
