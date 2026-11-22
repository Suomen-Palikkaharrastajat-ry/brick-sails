module Sails.Types exposing
    ( BuoyKind(..)
    , CameraMode(..)
    , Course
    , CourseId(..)
    , CourseProgress
    , LeaderboardEntry
    , SailingState
    , SfxEvent(..)
    , ShipSpec
    , ViewMode(..)
    , Waypoint
    , Wind
    , courseIdFromString
    , courseIdToString
    , lduToMeters
    , metersToLdu
    )

{-| Domain data types for Brick Sails: ship geometry specs, sailing dynamics,
navigation courses, waypoints, and leaderboard records.
-}

import Math.Vector3 exposing (Vec3)


{-| Conversion factor: 1 LDU (LDraw Unit) ≈ 0.008 meters (0.8 cm; 1 stud = 20 LDU = 16 mm).
For sailing scale, we scale up by 5x so micro-builds (e.g. 100 LDU ~ 0.8m) behave as
lively dinghies/sloops (~4.0 m).
-}
lduToMeters : Float
lduToMeters =
    0.04


metersToLdu : Float
metersToLdu =
    1.0 / lduToMeters


{-| Buoy navigation marking according to IALA maritime buoyage system.
-}
type BuoyKind
    = PortBuoy -- Red cylindrical can buoy (leave to port when entering harbour)
    | StarboardBuoy -- Green conical nun buoy (leave to starboard when entering)
    | FinishGate


{-| Navigation waypoint in a course.
-}
type alias Waypoint =
    { index : Int
    , position : Vec3
    , radius : Float
    , kind : BuoyKind
    }


{-| Course identifier.
-}
type CourseId
    = CourseHarborSlalom
    | CourseArchipelagoBeat
    | CourseOpenOcean


courseIdToString : CourseId -> String
courseIdToString cid =
    case cid of
        CourseHarborSlalom ->
            "harbor-slalom"

        CourseArchipelagoBeat ->
            "archipelago-beat"

        CourseOpenOcean ->
            "open-ocean"


courseIdFromString : String -> CourseId
courseIdFromString str =
    case str of
        "archipelago-beat" ->
            CourseArchipelagoBeat

        "open-ocean" ->
            CourseOpenOcean

        _ ->
            CourseHarborSlalom


{-| Wind conditions in the sailing environment.
-}
type alias Wind =
    { speed : Float -- true wind velocity in m/s (e.g. 7.5 m/s ≈ 15 knots)
    , direction : Float -- compass degrees the wind is blowing TOWARDS (0° = blowing North)
    }


{-| A full sailing challenge course.
-}
type alias Course =
    { id : CourseId
    , title : String
    , description : String
    , waypoints : List Waypoint
    , wind : Wind
    , startPosition : Vec3
    , startHeading : Float
    , targetSeconds : Float
    }


{-| Ingested LEGO ship specification derived from LDraw model.
-}
type alias ShipSpec =
    { name : String
    , mass : Float -- kilograms
    , lengthMeters : Float
    , beamMeters : Float
    , draftMeters : Float
    , mastHeightMeters : Float
    , sailAreaSqMeters : Float
    , centerOfBuoyancy : Vec3
    , centerOfEffort : Vec3 -- sail center of aerodynamic force
    , centerOfLateralResistance : Vec3 -- keel/hull lateral hydrodynamic resistance center
    , metacentricHeight : Float -- GM in meters (>0 means stable upright)
    , hullPartCount : Int
    , sailPartCount : Int
    , mastPartCount : Int
    , totalPartCount : Int
    }


{-| Dynamic sailing simulation state updated every frame.
-}
type alias SailingState =
    { position : Vec3
    , velocity : Vec3
    , heading : Float -- compass degrees (0° = North, 90° = East)
    , angularVelocity : Float -- yaw rate rad/s
    , heelAngle : Float -- roll degrees (positive = tilted to starboard)
    , pitchAngle : Float -- pitch degrees (bow up positive)
    , heave : Float -- vertical water offset in meters
    , rudderAngle : Float -- rudder angle [-35°, +35°]
    , sailSheet : Float -- sail sheet trim [0.0 = tight in, 1.0 = eased fully out]
    , apparentWindSpeed : Float -- m/s
    , apparentWindAngle : Float -- relative degrees from bow [-180°, 180°]
    , waterSpeed : Float -- speed through water in knots
    , driftAngle : Float -- leeway angle in degrees
    }


{-| Active course progress tracking.
-}
type alias CourseProgress =
    { currentWaypointIndex : Int
    , totalWaypoints : Int
    , elapsedSeconds : Float
    , splits : List Float
    , finished : Bool
    }


{-| Camera viewing mode.
-}
type CameraMode
    = Chase
    | Orbit
    | Helm


{-| Top-level UI view mode.
-}
type ViewMode
    = ViewShipyard
    | ViewSailing
    | ViewResults


{-| Sound effect event.
-}
type SfxEvent
    = SfxBell
    | SfxSplash Float
    | SfxHorn
    | SfxCheckpoint
    | SfxFinish


{-| Leaderboard run entry.
-}
type alias LeaderboardEntry =
    { courseId : String
    , player : String
    , timeSeconds : Float
    , isoDate : String
    , vehicleName : String
    }
