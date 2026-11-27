module Sails.Types exposing
    ( Boat
    , BoatKind(..)
    , Cannonball
    , CombatImpact
    , Course
    , CourseProgress
    , Decor
    , DecorKind(..)
    , Faction(..)
    , HarbourKind(..)
    , Island
    , IslandRole(..)
    , LeaderboardEntry
    , SailingState
    , ShipSpec
    , StandardState(..)
    , Target
    , Town
    , Treasure
    , ViewMode(..)
    , Waypoint
    , Wind
    , carriedBySlot
    , isAtCitadel
    , isAtSea
    , lduToMeters
    , metersToLdu
    , standardFaction
    , standardForCarrier
    )

{-| Domain data types for Brick Sails: the toy boats, the harbour course,
floating targets, the arcade sailing state and leaderboard records.
-}

import Game.MapParams exposing (MapParams)
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


{-| A floating target the player can shoot (a hit is counted; it wins no coins).
-}
type alias Target =
    { id : Int
    , position : Vec3
    , sunk : Bool
    , sinkAge : Float
    }


{-| What an island is for on a course: the `Hub` in the middle of the
archipelago (the fort or lighthouse, and the final harbour), one of the
`HarbourIsle`s in the ring round it, the small `TreasureIsle` with the
pirates' chest, or a `Skerry`, a rock out on the water.
-}
type IslandRole
    = Hub
    | HarbourIsle
    | TreasureIsle
    | Skerry


{-| A round island the boat bumps off.
-}
type alias Island =
    { x : Float
    , z : Float
    , radius : Float
    , height : Float
    , role : IslandRole
    }


{-| Decoration out on the water: a coral `Reef` (a shoal that deep keels
ground on, see `Sails.Physics.checkReefGrounding`), a `Wreck` run aground or
a rock `SeaArch`.
-}
type DecorKind
    = Reef
    | Wreck
    | SeaArch


{-| One piece of decoration on the water, turned `yaw` radians.
-}
type alias Decor =
    { kind : DecorKind
    , x : Float
    , z : Float
    , yaw : Float
    }


{-| The three factions contending for supremacy in the archipelago.
-}
type Faction
    = Pirates
    | BlueCoats
    | RedCoats


{-| A faction standard (Regalia) in the War of the Three Ensigns:
safe in its home citadel, carried aboard a ship, or bobbing in the sea
after a wreck.
-}
type StandardState
    = AtCitadel Faction
    | CarriedBy Int Faction
    | AtSea Faction Vec3 Float


{-| The faction whose standard this is.
-}
standardFaction : StandardState -> Faction
standardFaction std =
    case std of
        AtCitadel f ->
            f

        CarriedBy _ f ->
            f

        AtSea f _ _ ->
            f


{-| Whether this standard is currently safe at its citadel.
-}
isAtCitadel : Faction -> StandardState -> Bool
isAtCitadel f std =
    case std of
        AtCitadel sf ->
            sf == f

        _ ->
            False


{-| Whether this standard is carried by a specific ship slot.
-}
carriedBySlot : Int -> StandardState -> Maybe Faction
carriedBySlot slot std =
    case std of
        CarriedBy s f ->
            if s == slot then
                Just f

            else
                Nothing

        _ ->
            Nothing


{-| Find which standard is carried by a specific slot from a list of standards.
-}
standardForCarrier : Int -> List StandardState -> Maybe Faction
standardForCarrier slot standards =
    List.filterMap (carriedBySlot slot) standards
        |> List.head


{-| Whether this standard is bobbing at sea.
-}
isAtSea : StandardState -> Bool
isAtSea std =
    case std of
        AtSea _ _ _ ->
            True

        _ ->
            False


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
out along `bearing`. Sailing close enough collects its coins
(`Sails.Coins.treasureChest`). `bonusSeconds` is left from the time-based
scoring and no longer counts.
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
the towns on them and the treasure hidden on them, the decoration on the
water and the circle of sea the boats may sail in (`arena`). Every course is
made from a map (`Sails.Archipelago`); `id` is the map's leaderboard key
and `title` its map code (the screens show `Sails.I18n.mapTitle` of `map`).
-}
type alias Course =
    { id : String
    , map : MapParams
    , title : String
    , description : String
    , waypoints : List Waypoint
    , targets : List Target
    , islands : List Island
    , towns : List Town
    , treasures : List Treasure
    , decor : List Decor
    , wind : Wind
    , arena : { x : Float, z : Float, radius : Float }
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
    , carriedStandard : Maybe Faction
    }


{-| Progress through the course. `treasureFound` lists the ids of the
treasure collected, `coins` the gold coins earned so far, the run's score
(see `Sails.Coins`). `berthSeconds` counts how long the boat has been
moored, slow enough, at the harbour it is docking at.
-}
type alias CourseProgress =
    { currentWaypointIndex : Int
    , totalWaypoints : Int
    , elapsedSeconds : Float
    , splits : List Float
    , finished : Bool
    , targetsHit : Int
    , treasureFound : List Int
    , coins : Int
    , berthSeconds : Float
    }


{-| Top-level UI view mode.
-}
type ViewMode
    = ViewShipyard
    | ViewSailing


{-| Leaderboard run entry (`Sails.Records`): ranked by `coins`, the time is
kept to show and to break ties.
-}
type alias LeaderboardEntry =
    { courseId : String
    , player : String
    , coins : Int
    , timeSeconds : Float
    , isoDate : String
    , vehicleName : String
    }
