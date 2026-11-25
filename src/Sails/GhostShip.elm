module Sails.GhostShip exposing
    ( GhostShip
    , PatrolLoop
    , canTarget
    , collisionRadius
    , ghostShipBonusSeconds
    , ghostShipSlot
    , init
    , isStormActive
    , patrolClearance
    , patrolFor
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
import Sails.Types exposing (Course, SailingState)
import Sails.Weather exposing (Weather)


{-| Ghost ship state: its patrol circle, its sailing state, whether it is
currently materialized, and whether it has been banished for the rest of the
voyage.
-}
type alias GhostShip =
    { loop : PatrolLoop
    , state : SailingState
    , active : Bool
    , banished : Bool
    }


{-| Initial state on a course's patrol circle (`patrolFor`): dormant at t=0,
not yet banished.
-}
init : Course -> GhostShip
init course =
    let
        loop =
            patrolFor course
    in
    { loop = loop
    , state = stateAt loop 0
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


{-| Metres the patrol circle keeps off every island's shore.
-}
patrolClearance : Float
patrolClearance =
    30


{-| The ghost ship's speed along its circle (m/s): the old fixed 175 m
circle sailed in 120 s.
-}
patrolSpeed : Float
patrolSpeed =
    2 * pi * 175 / 120


{-| The patrol circle of a course, taken from its arena: a circle inside the
arena that keeps `patrolClearance` off every island's shore, so the ghost
ship sails open water the boat can reach. The search prefers the arena
centre and a radius of three quarters of the arena radius; it tries radii
from 0.2 to 0.96 of the arena radius and centres moved up to half of it
away, cheapest first (`patrolCandidates`). When no circle is clear, it takes
the one that keeps furthest off the islands. The lap time keeps the ghost
ship at `patrolSpeed`.
-}
patrolFor : Course -> PatrolLoop
patrolFor course =
    let
        arena =
            course.arena

        circles =
            List.map
                (\( ox, oz, f ) -> ( arena.x + ox * arena.radius, arena.z + oz * arena.radius, f * arena.radius ))
                patrolCandidates

        clearanceOf ( cx, cz, r ) =
            course.islands
                |> List.map (\isl -> abs (sqrt ((isl.x - cx) ^ 2 + (isl.z - cz) ^ 2) - r) - isl.radius)
                |> List.minimum
                |> Maybe.withDefault 1.0e9

        ( centreX, centreZ, radius ) =
            case firstClear (\c -> clearanceOf c >= patrolClearance) circles of
                Just c ->
                    c

                Nothing ->
                    List.foldl
                        (\c ( best, bestClearance ) ->
                            let
                                clearance =
                                    clearanceOf c
                            in
                            if clearance > bestClearance then
                                ( c, clearance )

                            else
                                ( best, bestClearance )
                        )
                        ( ( arena.x, arena.z, 0.75 * arena.radius ), -1.0e9 )
                        circles
                        |> Tuple.first
    in
    { centreX = centreX
    , centreZ = centreZ
    , radius = radius
    , lapSeconds = 2 * pi * radius / patrolSpeed
    }


{-| The first item that passes, without checking the rest.
-}
firstClear : (a -> Bool) -> List a -> Maybe a
firstClear ok items =
    case items of
        [] ->
            Nothing

        x :: rest ->
            if ok x then
                Just x

            else
                firstClear ok rest


{-| The patrol circles `patrolFor` tries, as the centre's offset from the
arena centre and the radius, both in arena radii, cheapest first: the
distance of the radius from three quarters plus the offset of the centre.
Every circle lies inside the arena with 4 % to spare.
-}
patrolCandidates : List ( Float, Float, Float )
patrolCandidates =
    let
        steps =
            List.range -10 10 |> List.map (\i -> 0.05 * toFloat i)
    in
    List.range 0 38
        |> List.map (\i -> 0.2 + 0.02 * toFloat i)
        |> List.concatMap (\f -> List.concatMap (\ox -> List.map (\oz -> ( ox, oz, f )) steps) steps)
        |> List.filter (\( ox, oz, f ) -> sqrt (ox * ox + oz * oz) + f <= 0.96)
        |> List.sortBy (\( ox, oz, f ) -> abs (f - 0.75) + sqrt (ox * ox + oz * oz))


{-| State of the ghost ship at time t along a patrol circle.
-}
stateAt : PatrolLoop -> Float -> SailingState
stateAt loop t =
    let
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
            ( { ship | active = False, banished = True, state = stateAt ship.loop time }
            , False
            , True
            )

        else if storm then
            let
                justAppeared =
                    not ship.active
            in
            ( { ship | active = True, state = stateAt ship.loop time }
            , justAppeared
            , False
            )

        else
            ( { ship | active = False, state = stateAt ship.loop time }
            , False
            , False
            )
