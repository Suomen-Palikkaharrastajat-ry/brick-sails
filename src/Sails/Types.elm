module Sails.Types exposing
    ( Boat
    , BoatKind(..)
    , Cannonball
    , CombatImpact
    , Course
    , CourseProgress
    , Faction(..)
    , HarbourKind(..)
    , Island
    , LeaderboardEntry
    , SailingState
    , ShipSpec
    , Target
    , Town
    , Treasure
    , ViewMode(..)
    , Waypoint
    , Wind
    , lduToMeters
    , metersToLdu
    )

{-| Domain data types for Brick Sails: the toy boats, the harbour course,
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


{-| A harbour's role: one to dock at on the way, or the final harbour
under the pirate arch that ends the voyage.
-}
type HarbourKind
    = Harbour
    | FinalHarbour


{-| A harbour the player must dock at, in order: slow down inside its
`radius` and stay there for a moment (see `Sails.Physics.stepProgress`).
-}
type alias Waypoint =
    { index : Int
    , position : Vec3
    , radius : Float
    , kind : HarbourKind
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


{-| The three factions contending for supremacy in the archipelago.
-}
type Faction
    = Pirates
    | BlueCoats
    | RedCoats


{-| A small brick town on an island's shore: houses on the grass and a pier
running out to the water, facing out along the compass `bearing` from the
island's centre, belonging to an optional `faction`.
-}
type alias Town =
    { name : String
    , island : Island
    , bearing : Float
    , faction : Maybe Faction
    }


{-| A pirate treasure chest on an island's beach, at `position`, facing
out along `bearing`. Sailing close enough collects it for `bonusSeconds`
off the race time.
-}
type alias Treasure =
    { id : Int
    , position : Vec3
    , bearing : Float
    , bonusSeconds : Float
    }


{-| Wind conditions: speed in m/s and the compass heading the wind blows towards.
-}
type alias Wind =
    { speed : Float
    , direction : Float
    }


{-| One short course: harbours in order, targets along the way, a few islands,
the towns on them and the treasure hidden on them.
-}
type alias Course =
    { id : String
    , title : String
    , description : String
    , waypoints : List Waypoint
    , targets : List Target
    , islands : List Island
    , towns : List Town
    , treasures : List Treasure
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
  - `topSpeed` is in m/s, `turnRate` in degrees per second at full helm,
    `acceleration` the rate (1/s) the speed closes on its target.

-}
type alias Boat =
    { kind : BoatKind
    , name : String
    , description : String
    , ldraw : String
    , sailColour : Maybe Int
    , topSpeed : Float
    , turnRate : Float
    , acceleration : Float
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
  - `sail`: sail area set, as a share of the boat's full sail (see
    `Sails.Physics.sailLevels`); eases towards the level the player picks.

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
    , sail : Float
    }


{-| Progress through the course. `treasureFound` lists the ids of the
treasure collected, `treasureBonus` the seconds it takes off the time.
`berthSeconds` counts how long the boat has been moored, slow enough, at
the harbour it is docking at.
-}
type alias CourseProgress =
    { currentWaypointIndex : Int
    , totalWaypoints : Int
    , elapsedSeconds : Float
    , splits : List Float
    , finished : Bool
    , targetsHit : Int
    , treasureFound : List Int
    , treasureBonus : Float
    , berthSeconds : Float
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
