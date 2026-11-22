module Sails.Types exposing
    ( Boat
    , BoatKind(..)
    , BuoyKind(..)
    , Cannonball
    , CombatImpact
    , Course
    , CourseProgress
    , Island
    , LeaderboardEntry
    , SailingState
    , ShipSpec
    , Target
    , ViewMode(..)
    , Waypoint
    , Wind
    , lduToMeters
    , metersToLdu
    )

{-| Domain data types for Brick Sails: the toy boats, the buoy course,
floating targets, the arcade sailing state and leaderboard records.
-}

import Math.Vector3 exposing (Vec3)


{-| Conversion factor: 1 LDU (LDraw Unit) = 0.04 m at the game's scale.
Used by the LDraw ingestion helpers.
-}
lduToMeters : Float
lduToMeters =
    0.04


metersToLdu : Float
metersToLdu =
    1.0 / lduToMeters


{-| Buoy colour / role.
-}
type BuoyKind
    = PortBuoy
    | StarboardBuoy
    | FinishGate


{-| A buoy the player must sail through, in order.
-}
type alias Waypoint =
    { index : Int
    , position : Vec3
    , radius : Float
    , kind : BuoyKind
    }


{-| A floating target the player can shoot for a time bonus.
-}
type alias Target =
    { id : Int
    , position : Vec3
    , sunk : Bool
    , sinkAge : Float
    }


{-| A round island the boat bumps off.
-}
type alias Island =
    { x : Float
    , z : Float
    , radius : Float
    , height : Float
    }


{-| Wind conditions: speed in m/s and the compass heading the wind blows towards.
-}
type alias Wind =
    { speed : Float
    , direction : Float
    }


{-| One short course: buoys in order, targets along the way and a few islands.
-}
type alias Course =
    { id : String
    , title : String
    , description : String
    , waypoints : List Waypoint
    , targets : List Target
    , islands : List Island
    , wind : Wind
    , startPosition : Vec3
    , startHeading : Float
    , targetSeconds : Float
    }


{-| The four built-in toy boats, plus a ship loaded from a build link.
-}
type BoatKind
    = Sloop
    | Cutter
    | Schooner
    | Catamaran
    | CustomShip


{-| A toy boat: what it is built from and how it handles.

  - `ldraw`: the LDraw model text of the boat. Every part of the boat on screen
    comes from these bricks (see `Sails.ShipMesh`). Empty for a ship loaded
    from a build link, whose parsed document is kept by `Main` instead.
  - `sailColour`: the LDraw colour code of the sail bricks. Bricks of this
    colour take the player colour when the boat is drawn for another player.
    `Nothing` means: find sails by part name (see `Sails.Ingest.isSailPart`).
  - `topSpeed` is in m/s, `turnRate` in degrees per second at full helm.

-}
type alias Boat =
    { kind : BoatKind
    , name : String
    , description : String
    , ldraw : String
    , sailColour : Maybe Int
    , topSpeed : Float
    , turnRate : Float
    }


{-| Specification ingested from an LDraw model (used by `Sails.Ingest` and
`Sails.Analyze`).
-}
type alias ShipSpec =
    { name : String
    , mass : Float
    , lengthMeters : Float
    , beamMeters : Float
    , draftMeters : Float
    , mastHeightMeters : Float
    , sailAreaSqMeters : Float
    , centerOfBuoyancy : Vec3
    , centerOfEffort : Vec3
    , centerOfLateralResistance : Vec3
    , metacentricHeight : Float
    , hullPartCount : Int
    , sailPartCount : Int
    , mastPartCount : Int
    , cannonCount : Int
    , totalPartCount : Int
    }


{-| Flying cannonball.
-}
type alias Cannonball =
    { id : Int
    , sourceSlot : Int
    , position : Vec3
    , velocity : Vec3
    , age : Float
    }


{-| Splash or hit flash.
-}
type alias CombatImpact =
    { id : Int
    , position : Vec3
    , age : Float
    , isHit : Bool
    }


{-| Arcade boat state, updated every frame.

  - `heading`: compass degrees, 0 = +Z, 90 = +X.
  - `speed`: m/s along the heading.
  - `steer`: smoothed helm, -1 (left) .. 1 (right).
  - `sailPower`: 0..1, how well the wind fills the sail on this heading.
  - `sailAngle`: visual boom angle in degrees.
  - `heel`, `pitch`: visual tilt in degrees.

-}
type alias SailingState =
    { position : Vec3
    , heading : Float
    , speed : Float
    , steer : Float
    , sailPower : Float
    , sailAngle : Float
    , heel : Float
    , pitch : Float
    , bumped : Bool
    }


{-| Progress through the course.
-}
type alias CourseProgress =
    { currentWaypointIndex : Int
    , totalWaypoints : Int
    , elapsedSeconds : Float
    , splits : List Float
    , finished : Bool
    , targetsHit : Int
    }


{-| Top-level UI view mode.
-}
type ViewMode
    = ViewShipyard
    | ViewSailing


{-| Leaderboard run entry.
-}
type alias LeaderboardEntry =
    { courseId : String
    , player : String
    , timeSeconds : Float
    , isoDate : String
    , vehicleName : String
    }
