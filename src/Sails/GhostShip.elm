module Sails.GhostShip exposing
    ( GhostShip
    , canTarget
    , collisionRadius
    , ghostShipBonusSeconds
    , ghostShipSlot
    , init
    , isStormActive
    , patrolLoop
    , spectralTint
    , stateAt
    , step
    )

{-| The Ghost Ship (Aavelaiva): a spectral phantom vessel that cruises along
the outer mist and stormy atmospheric fringes.

Materializes when stormy weather is active (squall > 0.25 or thunder > 0.25),
renders with an ethereal glowing trans-green spectral tint, and can be targeted
by player cannons. When hit, it is banished for the remainder of the voyage,
bursting into sparkling trans-green brick debris, awarding -5.0s bonus time,
and triggering a celebratory toast.

-}

import Math.Vector3 exposing (Vec3, vec3)
import Sails.Types exposing (SailingState)
import Sails.Weather exposing (Weather)


{-| Ghost ship state: its sailing state, whether it is currently materialized,
and whether it has been banished for the rest of the voyage.
-}
type alias GhostShip =
    { state : SailingState
    , active : Bool
    , banished : Bool
    }


{-| Initial state: dormant at t=0, not yet banished.
-}
init : GhostShip
init =
    { state = stateAt 0
    , active = False
    , banished = False
    }


{-| Ethereal glowing trans-green spectral tint for rendering the Ghost Ship.
-}
spectralTint : Vec3
spectralTint =
    vec3 0.18 0.98 0.52


{-| Virtual cannon target slot for the ghost ship, avoiding collisions with
player slots (0..8) and pirate patrol slots (100..105).
-}
ghostShipSlot : Int
ghostShipSlot =
    999


{-| Seconds awarded as bonus time when the ghost ship is banished.
-}
ghostShipBonusSeconds : Float
ghostShipBonusSeconds =
    5.0


{-| Collision and hit detection radius (m) for cannonball impacts.
-}
collisionRadius : Float
collisionRadius =
    2.6


{-| Outer atmospheric patrol loop specification: sweeps around the outer waters
clear of the inner harbour loop.
-}
type alias PatrolLoop =
    { centreX : Float
    , centreZ : Float
    , radius : Float
    , lapSeconds : Float
    }


{-| The outer patrol circle: centre (0, 140), radius 175 m, lap 120 s.
-}
patrolLoop : PatrolLoop
patrolLoop =
    { centreX = 0
    , centreZ = 140
    , radius = 175
    , lapSeconds = 120.0
    }


{-| State of the ghost ship at time t along its patrol circle.
-}
stateAt : Float -> SailingState
stateAt t =
    let
        loop =
            patrolLoop

        angle =
            2 * pi * (t / loop.lapSeconds)

        x =
            loop.centreX + loop.radius * sin angle

        z =
            loop.centreZ + loop.radius * cos angle

        dx =
            cos angle

        dz =
            -(sin angle)

        heading =
            atan2 dx dz * 180 / pi
    in
    { position = vec3 x 0 z
    , heading = heading
    , speed = 2 * pi * loop.radius / loop.lapSeconds
    , steer = 0
    , sailPower = 1
    , sailAngle = 30
    , heel = -6.0
    , pitch = 2.0 * sin (t * 2.0)
    , bumped = False
    , sail = 1.0
    , carriedStandard = Nothing
    }


{-| Whether storm weather is severe enough for the Ghost Ship to materialize.
-}
isStormActive : Weather -> Bool
isStormActive weather =
    weather.squall > 0.25 || weather.thunder > 0.25


{-| Whether the ghost ship can be aimed at and struck by cannonballs.
-}
canTarget : GhostShip -> Bool
canTarget ship =
    ship.active && not ship.banished


{-| Step the ghost ship: checks for storm activation, advances patrol position,
and detects cannonball hits. Returns (updatedShip, justAppeared, justBanished).
-}
step : Float -> Weather -> List Int -> GhostShip -> ( GhostShip, Bool, Bool )
step time weather slotsHit ship =
    if ship.banished then
        ( ship, False, False )

    else
        let
            hit =
                ship.active && List.member ghostShipSlot slotsHit

            storm =
                isStormActive weather
        in
        if hit then
            ( { ship | active = False, banished = True, state = stateAt time }
            , False
            , True
            )

        else if storm then
            let
                justAppeared =
                    not ship.active
            in
            ( { ship | active = True, state = stateAt time }
            , justAppeared
            , False
            )

        else
            ( { ship | active = False, state = stateAt time }
            , False
            , False
            )
