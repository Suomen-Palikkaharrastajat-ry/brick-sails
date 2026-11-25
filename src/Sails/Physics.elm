module Sails.Physics exposing
    ( StepInput
    , angleDiff
    , beachHeight
    , bonusPerHit
    , cannonRange
    , checkShipHits
    , checkTargetHits
    , courseCurrent
    , courseFreeRange
    , courseRoute
    , defaultCourse
    , defaultSailLevel
    , dockSeconds
    , dockSpeed
    , driftWith
    , finalTime
    , fireCannons
    , grassHeight
    , harbourRadius
    , harbourThrottle
    , helmCurve
    , initProgress
    , initSailingState
    , nearestCoursePoint
    , overpower
    , reachedTreasure
    , rockImpact
    , roughSeaStrain
    , sailDrive
    , sailLevel
    , sailLevels
    , sailLoad
    , seaElevation
    , shorePoint
    , starRating
    , steerResponse
    , stepBoat
    , stepImpacts
    , stepProgress
    , stepProjectiles
    , stepTargets
    , swellHeading
    , treasureReach
    , waterElevation
    , waveScale
    , windFactor
    )

{-| Simple, forgiving arcade sailing for a small toy boat.

The boat always keeps moving. The wind changes how fast it goes: sailing
straight into the wind is slower, a reach across the wind is fastest. The
player sets how much sail is up (`sailLevels`): more sail is faster and heels
more, less sail is slower and steers more easily. In a strong wind too much
sail overpowers the boat (`overpower`), so reefing pays. There is no stall
and no capsizing.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Types
    exposing
        ( Boat
        , Cannonball
        , CombatImpact
        , Course
        , CourseProgress
        , HarbourKind(..)
        , Island
        , SailingState
        , Target
        , Treasure
        , Waypoint
        , Wind
        )



-- COURSE


{-| A loop round the islands: six harbours to dock at in order, each one a
dock on the shore of one of the course's islands (see `harbour`). The loop
leaves from the lighthouse island in the middle (its first harbour beside
Tortuga Cove), calls at the east, northern, southern and outer islands
beside their towns, and comes back to the lighthouse island's final harbour
under the pirate arch, on the side facing the start. Seven floating targets
sit beside the legs for bonus time.
-}
defaultCourse : Course
defaultCourse =
    { id = "harbour-loop"
    , title = "Island Loop"
    , description = "Dock at each harbour in order, round the islands and back to the final harbour. Pirate treasure lies on the small island past harbour 3."
    , waypoints =
        [ harbour 0 Harbour lighthouseIsland 135
        , harbour 1 Harbour harbourIsland 297
        , harbour 2 Harbour smugglerIsland 212
        , harbour 3 Harbour anchorageIsland 110
        , harbour 4 Harbour outerIsland 20
        , harbour 5 FinalHarbour lighthouseIsland 215
        ]
    , targets =
        List.indexedMap
            (\i ( x, z ) -> { id = i, position = vec3 x 0 z, sunk = False, sinkAge = 0 })
            [ ( -11, 42 ), ( 63, 81 ), ( 64, 165 ), ( 52, 251 ), ( -45, 238 ), ( -134, 105 ), ( -84, 37 ) ]
    , islands =
        [ lighthouseIsland
        , harbourIsland
        , anchorageIsland
        , smugglerIsland
        , outerIsland
        , treasureIsland
        ]
    , towns =
        [ { name = "Tortuga Cove", island = lighthouseIsland, bearing = 100 }
        , { name = "Port Royal Brick", island = harbourIsland, bearing = 268 }
        , { name = "Governor's Anchorage", island = anchorageIsland, bearing = 130 }
        , { name = "Smuggler's Reach", island = smugglerIsland, bearing = 195 }
        ]
    , treasures =
        [ { id = 0
          , position = Vec3.setY (beachHeight treasureIsland) (shorePoint treasureIsland 330 0.86)
          , bearing = 330
          , bonusSeconds = 6
          }
        ]
    , wind = { speed = 7.0, direction = 270.0 }
    , startPosition = vec3 0 0 0
    , startHeading = 5.0
    , targetSeconds = 110.0
    }


{-| The island in the middle of the course, with the fort's watchtower and
a little colonial cove town: the loop leaves from its first harbour and
ends at its final harbour, both on the shore facing the start.
-}
lighthouseIsland : Island
lighthouseIsland =
    { x = -12, z = 100, radius = 24, height = 6 }


{-| The big island east of the course, with the colonial harbour town.
-}
harbourIsland : Island
harbourIsland =
    { x = 150, z = 90, radius = 40, height = 10 }


{-| The large southern island with the governor's anchorage and harbour.
-}
anchorageIsland : Island
anchorageIsland =
    { x = -170, z = 230, radius = 50, height = 12 }


{-| The northern island overlooking the outer reef and harbour.
-}
smugglerIsland : Island
smugglerIsland =
    { x = 40, z = 330, radius = 45, height = 9 }


{-| The island out past the reef behind the start line.
-}
outerIsland : Island
outerIsland =
    { x = -130, z = -60, radius = 35, height = 7 }


{-| A small island beside the leg from harbour 3 to harbour 4, where the
pirates buried their chest on the shore facing the route: a short detour
off the route.
-}
treasureIsland : Island
treasureIsland =
    { x = -78, z = 205, radius = 10, height = 5 }


{-| A point on an island at a compass bearing from its centre, `share` of
its radius out (1 is the water's edge), at sea level.
-}
shorePoint : Island -> Float -> Float -> Vec3
shorePoint island bearing share =
    vec3
        (island.x + sin (degrees bearing) * island.radius * share)
        0
        (island.z + cos (degrees bearing) * island.radius * share)


{-| Height of an island's beach and of the grass above it. `Sails.Render`
draws an island as four tiers of 2 x 2 round bricks (`Game.BrickModels.island`,
100 LDU tall with the studs) stretched to 1.5 x the island's height, its base
half the height below -0.6 m: the beach is the top of the second tier
(48 LDU) and the grass the top of the third (72 LDU).
-}
beachHeight : Island -> Float
beachHeight island =
    0.22 * island.height - 0.6


grassHeight : Island -> Float
grassHeight island =
    0.58 * island.height - 0.6


{-| A harbour on the way, with index `i`, docked on the shore of an island
at a compass bearing from its centre: `harbourOffshore` off the beach, just
outside the rock the boat bumps into (`boatRadius` off the shore, see
`pushOutOfIsland`). With the brake assist the boat stops a few metres short
of the harbour's centre, out in open water, and moors alongside the pier.
-}
harbour : Int -> HarbourKind -> Island -> Float -> Waypoint
harbour i kind island bearing =
    { index = i
    , position = shorePoint island bearing ((island.radius + harbourOffshore) / island.radius)
    , radius = harbourRadius
    , kind = kind
    }


{-| How far off an island's shore its harbours lie, in metres.
-}
harbourOffshore : Float
harbourOffshore =
    3.0


{-| How close to a harbour's centre the boat must be to dock there.
-}
harbourRadius : Float
harbourRadius =
    10.0


{-| The boat docks only at this speed or slower, in m/s.
-}
dockSpeed : Float
dockSpeed =
    2.0


{-| Seconds the boat must stay moored, slow and inside the harbour's
radius, to dock. Time spent too fast pauses the count; leaving the radius
starts it again.
-}
dockSeconds : Float
dockSeconds =
    1.0


{-| Centre of the play area and how far the boat may wander from it.
-}
arenaCenter : ( Float, Float )
arenaCenter =
    ( -7, 95 )


arenaRadius : Float
arenaRadius =
    240.0


initSailingState : Course -> SailingState
initSailingState course =
    { position = course.startPosition
    , heading = course.startHeading
    , speed = 0
    , steer = 0
    , sailPower = windFactor course.wind course.startHeading
    , sailAngle = 0
    , heel = 0
    , pitch = 0
    , bumped = False
    , sail = 1
    }


initProgress : Course -> CourseProgress
initProgress course =
    { currentWaypointIndex = 0
    , totalWaypoints = List.length course.waypoints
    , elapsedSeconds = 0
    , splits = []
    , finished = False
    , targetsHit = 0
    , treasureFound = []
    , treasureBonus = 0
    , berthSeconds = 0
    }



-- WATER


{-| Swell height at (x, z) and time t, in metres, in normal weather.
Must match the vertex shader in `Sails.OceanShader`.
-}
waterElevation : Float -> Float -> Float -> Float
waterElevation =
    seaElevation 1


{-| Swell height at (x, z) and time t for a sea state, in metres.

The sea is two crossing swells, grouped into wave sets: every 150 m a group
of waves up to half again as high rolls through (`waveSet`). `sea` scales
the swells: 1 is normal weather (the swell between sets is the game's
original gentle sea), below 1 a glassy calm, above 1 a storm sea, which
also raises a long storm swell of its own.

Must match `seaHeight` in the shaders of `Sails.OceanShader`.

-}
seaElevation : Float -> Float -> Float -> Float -> Float
seaElevation sea x z t =
    let
        k1 =
            2.0 * pi / 36.0

        k2 =
            2.0 * pi / 22.0

        k0 =
            2.0 * pi / 64.0
    in
    sea
        * waveSet x z t
        * (0.22
            * sin (k1 * (z * 0.9 + x * 0.4) - k1 * 3.0 * t)
            + 0.1
            * sin (k2 * (x * 0.8 - z * 0.5) - k2 * 2.4 * t)
          )
        + stormSwell sea
        * sin (k0 * (x * 0.3 + z * 0.95) - k0 * 4.2 * t)


{-| Height of the long storm swell: none up to normal weather.
-}
stormSwell : Float -> Float
stormSwell sea =
    0.45 * max 0 (sea - 1)


{-| Wave-set envelope at (x, z, t): 1 between sets, up to 1.5 in a set.
Sets travel with the main swell at half its speed, as wave groups do.
-}
waveSet : Float -> Float -> Float -> Float
waveSet x z t =
    let
        kg =
            2.0 * pi / 150.0
    in
    1 + 0.5 * (max 0 (sin (kg * (z * 0.9 + x * 0.4) - kg * 1.5 * t)) ^ 4)


{-| How big the waves are at (x, z, t) compared with the swell of normal
weather between sets: the sea state times the wave-set envelope.
-}
waveScale : Float -> Float -> Float -> Float -> Float
waveScale sea x z t =
    sea * waveSet x z t + stormSwell sea / 0.32


{-| Compass heading the main swell travels towards (from its wave vector,
0.4 x + 0.9 z).
-}
swellHeading : Float
swellHeading =
    atan2 0.4 0.9 * 180 / pi



-- SAILING


{-| Signed smallest difference a - b in degrees, in (-180, 180].
-}
angleDiff : Float -> Float -> Float
angleDiff a b =
    let
        d =
            a - b - 360.0 * toFloat (floor ((a - b + 180.0) / 360.0))
    in
    d


smoothstep : Float -> Float -> Float -> Float
smoothstep e0 e1 x =
    let
        t =
            clamp 0 1 ((x - e0) / (e1 - e0))
    in
    t * t * (3 - 2 * t)


{-| How well the wind fills the sail on a heading: 0.65 straight into the
wind, 1.0 across it, a little less dead downwind. Never zero, so the boat
never stops.
-}
windFactor : Wind -> Float -> Float
windFactor wind heading =
    let
        from =
            wind.direction + 180.0

        offWind =
            abs (angleDiff heading from)
    in
    0.65 + 0.35 * smoothstep 25 95 offWind - 0.12 * smoothstep 140 180 offWind


{-| One fixed step of input.

  - `steer`: -1 (left) .. 1 (right).
  - `throttle`: 1 while racing, 0 while held at the start line.
  - `sea`: sea state (see `seaElevation`), 1 in normal weather.
  - `sail`: the sail area the player has set (see `sailLevels`), 1 for
    full sail.

-}
type alias StepInput =
    { dt : Float
    , time : Float
    , steer : Float
    , throttle : Float
    , sea : Float
    , sail : Float
    }



-- SAILS


{-| The sail settings the player steps through, from lowered to every sail
up, with the share of full sail each sets. "Full" is the game's tuned
sailing and the level a race starts at.
-}
sailLevels : List ( String, Float )
sailLevels =
    [ ( "Down", 0 )
    , ( "Storm", 0.35 )
    , ( "Reefed", 0.65 )
    , ( "Full", 1.0 )
    , ( "Press", 1.2 )
    ]


{-| Index of "Full" in `sailLevels`.
-}
defaultSailLevel : Int
defaultSailLevel =
    3


{-| Name and area of a sail level, clamped to the levels there are.
-}
sailLevel : Int -> ( String, Float )
sailLevel i =
    List.drop (clamp 0 (List.length sailLevels - 1) i) sailLevels
        |> List.head
        |> Maybe.withDefault ( "Full", 1.0 )


{-| The wind speed the boats are tuned for: the course wind.
-}
referenceWind : Float
referenceWind =
    7.0


{-| How hard the wind presses on the sail set: 1 for full sail in the
course wind. It grows with the sail area and the square of the wind speed.
-}
sailLoad : Float -> Wind -> Float
sailLoad area wind =
    (max 0 area ^ 1.5) * ((wind.speed / referenceWind) ^ 2)


{-| How far the sail is overpowered: 0 until the load passes 1.25 (full
sail stays clean in the course wind and in light gusts). An overpowered
boat heels hard, loses speed, turns sluggishly and rounds up into the wind.
Reefing brings it back.
-}
overpower : Float -> Wind -> Float
overpower area wind =
    max 0 (sailLoad area wind - 1.25)


{-| Drive of the sail set, as a share of full sail in the course wind:
more sail and more wind drive harder, an overpowered sail spills it.
Bare poles still drift along at 15 %.
-}
sailDrive : Float -> Wind -> Float
sailDrive area wind =
    (0.15 + 0.85 * (max 0 area ^ 0.7))
        * sqrt (clamp 0.3 2.5 (wind.speed / referenceWind))
        / (1 + 0.35 * overpower area wind)


{-| Helm response of the sail set: less sail steers more easily (up to 50 %
quicker with the sails down), pressing on with extra sail and an
overpowered rig turn wider.
-}
sailHelm : Float -> Wind -> Float
sailHelm area wind =
    (1 + 0.5 * (1 - clamp 0 1 area))
        * (1 - 0.6 * max 0 (area - 1))
        / (1 + 0.3 * overpower area wind)


{-| Speed lost sailing into the swell, as a share of the speed: none in
normal weather between wave sets, growing with the waves above that.
Following seas cost nothing.
-}
headSeaLoss : Float -> Float -> Float
headSeaLoss scale heading =
    let
        headOn =
            max 0 -(cos (degrees (heading - swellHeading)))
    in
    clamp 0 0.25 (0.06 * headOn * max 0 (scale - 1))


{-| Advance the boat one step. `wind` is the wind blowing now (the course
wind in fair weather, see `Sails.Weather`).
-}
stepBoat : StepInput -> Wind -> Boat -> List Island -> SailingState -> SailingState
stepBoat input wind boat islands state =
    let
        dt =
            clamp 0.001 0.05 input.dt

        steer =
            easeHelm dt (clamp -1 1 input.steer) state.steer

        speedRatio =
            clamp 0 1 (state.speed / boat.topSpeed)

        -- Hoisting or lowering takes a second or two.
        sail =
            state.sail + (clamp 0 1.2 input.sail - state.sail) * min 1 (dt * 1.6)

        over =
            overpower sail wind

        -- An overpowered boat rounds up: the helm is pulled towards the wind.
        roundUp =
            roundUpRate * over * sin (degrees (angleDiff (wind.direction + 180) state.heading))

        -- Steering right lowers the heading (screen right in the follow camera).
        heading =
            normalize
                (state.heading
                    - turnRateAt boat speedRatio
                    * sailHelm sail wind
                    * helmCurve steer
                    * dt
                    + roundUp
                    * dt
                )

        power =
            windFactor wind heading

        p =
            Vec3.toRecord state.position

        waves =
            waveScale input.sea p.x p.z input.time

        targetSpeed =
            input.throttle
                * boat.topSpeed
                * power
                * sailDrive sail wind
                * (1 - 0.12 * abs steer)
                * (1 - headSeaLoss waves heading)
                |> min (1.3 * boat.topSpeed)

        speed =
            state.speed + (targetSpeed - state.speed) * min 1 (dt * accelerationAt boat speedRatio)

        rad =
            degrees heading

        movedX =
            p.x + sin rad * speed * dt

        movedZ =
            p.z + cos rad * speed * dt

        ( bx, bz, bumped ) =
            List.foldl pushOutOfIsland ( movedX, movedZ, False ) islands
                |> keepInArena

        finalSpeed =
            if bumped then
                speed * 0.6

            else
                speed

        water =
            \dx dz -> seaElevation input.sea (bx + dx) (bz + dz) input.time

        y =
            water 0 0

        -- The hull follows the slope of the water under it: bow and stern
        -- samples pitch it, port and starboard samples roll it.
        fore =
            ( sin rad * halfLength, cos rad * halfLength )

        side =
            ( cos rad * halfBeam, -(sin rad) * halfBeam )

        slopeAngle ( dx, dz ) span =
            atan ((water dx dz - water -dx -dz) / (2 * span)) * 180 / pi

        wavePitch =
            slopeAngle fore halfLength

        waveRoll =
            -(slopeAngle side halfBeam)

        -- Boom swings out to the side away from the wind.
        windSide =
            angleDiff (wind.direction + 180) heading

        sailAngle =
            if windSide > 0 then
                20 + 50 * smoothstep 30 170 (abs windSide)

            else
                -(20 + 50 * smoothstep 30 170 (abs windSide))

        -- Sail pressed past its load heels the boat away from the wind.
        windHeel =
            let
                h =
                    max 0 (sailLoad sail wind - 1)
            in
            (if windSide > 0 then
                -1

             else
                1
            )
                * 28
                * h
                / (1 + h)

        heelTarget =
            steer * 14 * speedRatio + waveRoll + windHeel
    in
    { position = vec3 bx y bz
    , heading = heading
    , speed = finalSpeed
    , steer = steer
    , sailPower = power
    , sailAngle = state.sailAngle + (sailAngle - state.sailAngle) * min 1 (dt * 4)
    , heel = state.heel + (heelTarget - state.heel) * min 1 (dt * 5)
    , pitch = 2.5 * sin (input.time * 2.1) * (0.4 + 0.6 * speedRatio) + wavePitch
    , bumped = bumped
    , sail = sail
    }



-- CONTROL RESPONSE


{-| How fast the helm follows the player's input, per second, while
putting it over (`helmAttack`) and while easing it back or reversing it
(`helmRelease`). The helm goes over gently and comes back quickly, so a
turn stops when the player lets go instead of carrying on past the course
they wanted (no overshoot on a tack).
-}
helmAttack : Float
helmAttack =
    5.0


helmRelease : Float
helmRelease =
    10.0


{-| Move the helm one step towards the input (see `helmAttack`).
-}
easeHelm : Float -> Float -> Float -> Float
easeHelm dt input current =
    let
        rate =
            if abs input < abs current || input * current < 0 then
                helmRelease

            else
                helmAttack
    in
    current + (input - current) * min 1 (dt * rate)


{-| Turn response to the helm: soft near the middle, so small tilts and
taps make small corrections, full at full helm. Odd: steering left and
right are mirror images.
-}
helmCurve : Float -> Float
helmCurve s =
    let
        a =
            clamp 0 1 (abs s)
    in
    (if s < 0 then
        -1

     else
        1
    )
        * (0.4 * a + 0.6 * a * a)


{-| Share of a boat's `turnRate` used at full helm. The built-in boats'
rates (60 to 130 degrees per second) are their relative agility; at full
rate the boat spun round faster than a player can follow on a phone.
-}
helmGain : Float
helmGain =
    0.7


{-| Degrees per second the boat turns at full helm at a share of its top
speed: it keeps two thirds of its steering from a standstill, so it can be
pointed at the first harbour right after the start.
-}
turnRateAt : Boat -> Float -> Float
turnRateAt boat speedRatio =
    boat.turnRate * helmGain * (0.65 + 0.35 * clamp 0 1 speedRatio)


{-| Rate (1/s) the speed closes on its target: twice the boat's
`acceleration` from a standstill, easing to its own value at top speed,
so the boat gets going quickly off the line and after a bump.
-}
accelerationAt : Boat -> Float -> Float
accelerationAt boat speedRatio =
    boat.acceleration * (2 - clamp 0 1 speedRatio)


{-| Degrees per second an overpowered boat rounds up, per unit of
`overpower`: a gust slews the bow gently, it does not spin the boat.
-}
roundUpRate : Float
roundUpRate =
    2.5


{-| Half the hull length and beam, where the water is sampled for pitch and
roll.
-}
halfLength : Float
halfLength =
    2.2


halfBeam : Float
halfBeam =
    1.1


normalize : Float -> Float
normalize h =
    h - 360.0 * toFloat (floor (h / 360.0))


boatRadius : Float
boatRadius =
    2.0


pushOutOfIsland : Island -> ( Float, Float, Bool ) -> ( Float, Float, Bool )
pushOutOfIsland island ( x, z, bumped ) =
    let
        dx =
            x - island.x

        dz =
            z - island.z

        d =
            sqrt (dx * dx + dz * dz)

        minD =
            island.radius + boatRadius
    in
    if d < minD && d > 0.001 then
        ( island.x + dx / d * minD, island.z + dz / d * minD, True )

    else
        ( x, z, bumped )


keepInArena : ( Float, Float, Bool ) -> ( Float, Float, Bool )
keepInArena ( x, z, bumped ) =
    let
        ( cx, cz ) =
            arenaCenter

        dx =
            x - cx

        dz =
            z - cz

        d =
            sqrt (dx * dx + dz * dz)
    in
    if d > arenaRadius then
        ( cx + dx / d * arenaRadius, cz + dz / d * arenaRadius, True )

    else
        ( x, z, bumped )



-- COURSE RECOVERY


{-| The route the course follows: the start, then every harbour in order.
-}
courseRoute : Course -> List Vec3
courseRoute course =
    course.startPosition :: List.map .position course.waypoints


{-| Distance in metres from the route the boat may wander freely, before
the current starts carrying it back: far enough for a wide rounding or the
detour to the treasure.
-}
courseFreeRange : Float
courseFreeRange =
    45.0


{-| The nearest point on the route (any leg, and the leg back from the
finish to the start) to a position, and how far away it is.
-}
nearestCoursePoint : List Vec3 -> Vec3 -> Maybe { point : Vec3, distance : Float }
nearestCoursePoint route position =
    let
        p =
            Vec3.toRecord position

        legs =
            case route of
                first :: rest ->
                    List.map2 Tuple.pair route (rest ++ [ first ])

                [] ->
                    []

        onLeg ( a, b ) =
            let
                ra =
                    Vec3.toRecord a

                rb =
                    Vec3.toRecord b

                ex =
                    rb.x - ra.x

                ez =
                    rb.z - ra.z

                len2 =
                    ex * ex + ez * ez

                t =
                    if len2 < 1.0e-6 then
                        0

                    else
                        clamp 0 1 (((p.x - ra.x) * ex + (p.z - ra.z) * ez) / len2)

                cx =
                    ra.x + ex * t

                cz =
                    ra.z + ez * t
            in
            { point = vec3 cx 0 cz, distance = sqrt ((p.x - cx) ^ 2 + (p.z - cz) ^ 2) }
    in
    (case legs of
        [] ->
            List.map (\a -> onLeg ( a, a )) route

        _ ->
            List.map onLeg legs
    )
        |> List.sortBy .distance
        |> List.head


{-| The current that carries a boat that strayed from the route back to
it, as a velocity (x, z) in m/s: none within `courseFreeRange`, then
0.5 m/s growing over the next 40 m to 3 m/s, a third of a boat's top
speed. It
only moves the boat; the player still steers and sails, and can sail
against it.
-}
courseCurrent : List Vec3 -> Vec3 -> ( Float, Float )
courseCurrent route position =
    case nearestCoursePoint route position of
        Just near ->
            let
                excess =
                    near.distance - courseFreeRange
            in
            if excess <= 0 || near.distance < 0.001 then
                ( 0, 0 )

            else
                let
                    strength =
                        0.5 + 2.5 * clamp 0 1 (excess / 40)

                    p =
                        Vec3.toRecord position

                    c =
                        Vec3.toRecord near.point
                in
                ( (c.x - p.x) / near.distance * strength, (c.z - p.z) / near.distance * strength )

        Nothing ->
            ( 0, 0 )


{-| Move the boat with a current (x, z, in m/s) for a step, keeping it
on the water's surface height it already has.
-}
driftWith : Float -> ( Float, Float ) -> SailingState -> SailingState
driftWith dt ( vx, vz ) state =
    if vx == 0 && vz == 0 then
        state

    else
        let
            p =
                Vec3.toRecord state.position
        in
        { state | position = vec3 (p.x + vx * dt) p.y (p.z + vz * dt) }



-- HAZARDS


{-| How hard the boat struck rock this step, in m/s: its speed into the
island's shore (the part of the speed towards the island's centre), when it
has just touched an island it was not touching the step before. Scraping
along a shore after the first knock counts as nothing.
-}
rockImpact : List Island -> SailingState -> SailingState -> Maybe Float
rockImpact islands before after =
    let
        touching state island =
            let
                p =
                    Vec3.toRecord state.position
            in
            sqrt ((p.x - island.x) ^ 2 + (p.z - island.z) ^ 2) <= island.radius + boatRadius + 0.05

        struck =
            List.filter (\i -> touching after i && not (before.bumped && touching before i)) islands
                |> List.head
    in
    case struck of
        Just island ->
            let
                p =
                    Vec3.toRecord before.position

                dx =
                    island.x - p.x

                dz =
                    island.z - p.z

                d =
                    max 0.001 (sqrt (dx * dx + dz * dz))

                rad =
                    degrees before.heading

                closing =
                    (sin rad * dx + cos rad * dz) / d
            in
            Just (before.speed * clamp 0 1 closing)

        Nothing ->
            Nothing


{-| How hard a rough sea is pounding the hull, 0 and up: nothing until the
waves are two and a half times the normal swell (a strong squall or a
thunderstorm), then more for bigger waves, for sailing into them rather than
across or away, and for sailing fast.
-}
roughSeaStrain : Float -> Float -> Boat -> SailingState -> Float
roughSeaStrain sea time boat state =
    let
        p =
            Vec3.toRecord state.position

        waves =
            waveScale sea p.x p.z time

        headOn =
            max 0 -(cos (degrees (state.heading - swellHeading)))
    in
    max 0 (waves - 2.5) * headOn * clamp 0 1 (state.speed / max 0.1 boat.topSpeed)


{-| Degrees per second the boat turns for a helm input held long enough to
settle, at a share of its top speed, with full sail in the course wind:
the control response the player feels.
-}
steerResponse : Boat -> Float -> Float -> Float
steerResponse boat speedRatio input =
    turnRateAt boat speedRatio * helmCurve input



-- PROGRESS


{-| Count time and dock at the next harbour. The boat docks once it has
been inside the harbour's radius at `dockSpeed` or slower for `dockSeconds`
in all: sailing too fast inside the radius pauses the count, leaving the
radius starts it again. Returns the index of the harbour just docked at, if
any.
-}
stepProgress : Float -> SailingState -> List Waypoint -> CourseProgress -> ( CourseProgress, Maybe Int )
stepProgress dt state waypoints progress =
    if progress.finished then
        ( progress, Nothing )

    else
        let
            elapsed =
                progress.elapsedSeconds + dt

            ticked =
                { progress | elapsedSeconds = elapsed }
        in
        case List.drop progress.currentWaypointIndex waypoints |> List.head of
            Just wp ->
                if horizontalDistance state.position wp.position > wp.radius then
                    ( { ticked | berthSeconds = 0 }, Nothing )

                else if state.speed > dockSpeed then
                    ( ticked, Nothing )

                else
                    let
                        berthed =
                            progress.berthSeconds + dt
                    in
                    if berthed >= dockSeconds then
                        let
                            next =
                                progress.currentWaypointIndex + 1
                        in
                        ( { ticked
                            | currentWaypointIndex = next
                            , splits = progress.splits ++ [ elapsed ]
                            , finished = next >= progress.totalWaypoints
                            , berthSeconds = 0
                          }
                        , Just progress.currentWaypointIndex
                        )

                    else
                        ( { ticked | berthSeconds = berthed }, Nothing )

            Nothing ->
                ( { ticked | finished = True }, Nothing )


{-| The brake assist that helps the boat moor: 0 (the sails let fly, so the
boat slows down) while it is inside the radius of the harbour it is docking
at, 1 otherwise and once the course is done.
-}
harbourThrottle : List Waypoint -> CourseProgress -> SailingState -> Float
harbourThrottle waypoints progress state =
    if progress.finished then
        1

    else
        case List.drop progress.currentWaypointIndex waypoints |> List.head of
            Just wp ->
                if horizontalDistance state.position wp.position <= wp.radius then
                    0

                else
                    1

            Nothing ->
                1


horizontalDistance : Vec3 -> Vec3 -> Float
horizontalDistance a b =
    let
        pa =
            Vec3.toRecord a

        pb =
            Vec3.toRecord b
    in
    sqrt ((pa.x - pb.x) ^ 2 + (pa.z - pb.z) ^ 2)


{-| Seconds taken off the time for each target hit.
-}
bonusPerHit : Float
bonusPerHit =
    2.0


{-| Race time minus the target and treasure bonuses.
-}
finalTime : CourseProgress -> Float
finalTime progress =
    max 0 (progress.elapsedSeconds - bonusPerHit * toFloat progress.targetsHit - progress.treasureBonus)


{-| How close the boat must sail to a chest to take it aboard.
-}
treasureReach : Float
treasureReach =
    7.0


{-| Treasure the boat is close enough to collect now, leaving out what is
already found.
-}
reachedTreasure : SailingState -> List Treasure -> List Int -> List Treasure
reachedTreasure state treasures found =
    List.filter
        (\t -> not (List.member t.id found) && horizontalDistance state.position t.position <= treasureReach)
        treasures


{-| 3 stars at or under the target time, 2 within 30 % of it, otherwise 1.
-}
starRating : Float -> Float -> Int
starRating targetSeconds time =
    if time <= targetSeconds then
        3

    else if time <= targetSeconds * 1.3 then
        2

    else
        1



-- CANNONS


{-| Targets further than this are out of reach.
-}
cannonRange : Float
cannonRange =
    34.0


muzzleSpeed : Float
muzzleSpeed =
    26.0


gravity : Float
gravity =
    9.81


{-| Fire. If something is within range, one ball is lobbed straight at the
nearest one (it will hit if the target stays put). Otherwise a broadside
goes out to both sides for the splash.
-}
fireCannons : Int -> SailingState -> List Vec3 -> Int -> List Cannonball
fireCannons slot state aimPoints startId =
    let
        origin =
            Vec3.add state.position (vec3 0 1.0 0)

        nearest =
            aimPoints
                |> List.map (\p -> ( horizontalDistance origin p, p ))
                |> List.filter (\( d, _ ) -> d <= cannonRange && d > 1.5)
                |> List.sortBy Tuple.first
                |> List.head
    in
    case nearest of
        Just ( d, aim ) ->
            let
                o =
                    Vec3.toRecord origin

                a =
                    Vec3.toRecord aim

                t =
                    max 0.25 (d / muzzleSpeed)

                vy =
                    (a.y + 0.6 - o.y + 0.5 * gravity * t * t) / t
            in
            [ { id = startId
              , sourceSlot = slot
              , position = origin
              , velocity = vec3 ((a.x - o.x) / t) vy ((a.z - o.z) / t)
              , age = 0
              }
            ]

        Nothing ->
            let
                rad =
                    degrees state.heading

                side sign =
                    vec3 (sign * cos rad * 18) 5 (sign * -(sin rad) * 18)
            in
            [ { id = startId, sourceSlot = slot, position = origin, velocity = side 1, age = 0 }
            , { id = startId + 1, sourceSlot = slot, position = origin, velocity = side -1, age = 0 }
            ]


{-| Move cannonballs; the ones that reach the water become splashes.
-}
stepProjectiles : Float -> Float -> List Cannonball -> ( List Cannonball, List CombatImpact )
stepProjectiles dt time balls =
    let
        stepOne ball ( alive, splashes ) =
            let
                p =
                    Vec3.toRecord ball.position

                v =
                    Vec3.toRecord ball.velocity

                vy =
                    v.y - gravity * dt

                nx =
                    p.x + v.x * dt

                ny =
                    p.y + vy * dt

                nz =
                    p.z + v.z * dt

                water =
                    waterElevation nx nz time
            in
            if ball.age + dt > 4.0 then
                ( alive, splashes )

            else if ny <= water then
                ( alive, { id = ball.id, position = vec3 nx water nz, age = 0, isHit = False } :: splashes )

            else
                ( { ball | position = vec3 nx ny nz, velocity = vec3 v.x vy v.z, age = ball.age + dt } :: alive
                , splashes
                )
    in
    List.foldl stepOne ( [], [] ) balls


targetHitRadius : Float
targetHitRadius =
    2.2


{-| Sink targets the balls touch. Returns surviving balls, hit flashes,
updated targets and the number of new hits.
-}
checkTargetHits : List Cannonball -> List Target -> ( List Cannonball, List CombatImpact, ( List Target, Int ) )
checkTargetHits balls targets =
    let
        hits ball target =
            not target.sunk && Vec3.distance ball.position (Vec3.add target.position (vec3 0 0.6 0)) <= targetHitRadius

        step ball ( alive, flashes, ( ts, count ) ) =
            case List.filter (hits ball) ts |> List.head of
                Just struck ->
                    ( alive
                    , { id = ball.id, position = ball.position, age = 0, isHit = True } :: flashes
                    , ( List.map
                            (\t ->
                                if t.id == struck.id then
                                    { t | sunk = True }

                                else
                                    t
                            )
                            ts
                      , count + 1
                      )
                    )

                Nothing ->
                    ( ball :: alive, flashes, ( ts, count ) )
    in
    List.foldl step ( [], [], ( targets, 0 ) ) balls


{-| Balls hitting other boats (multiplayer). Returns surviving balls, flashes
and the slots hit.
-}
checkShipHits : List Cannonball -> List { slot : Int, position : Vec3 } -> ( List Cannonball, List CombatImpact, List Int )
checkShipHits balls ships =
    let
        step ball ( alive, flashes, slots ) =
            case List.filter (\s -> s.slot /= ball.sourceSlot && Vec3.distance ball.position s.position <= 2.6) ships |> List.head of
                Just ship ->
                    ( alive, { id = ball.id, position = ball.position, age = 0, isHit = True } :: flashes, ship.slot :: slots )

                Nothing ->
                    ( ball :: alive, flashes, slots )
    in
    List.foldl step ( [], [], [] ) balls


{-| Age sinking targets; keep their bob in sync with the water of the sea
state `sea`.
-}
stepTargets : Float -> Float -> Float -> List Target -> List Target
stepTargets sea dt time targets =
    List.map
        (\t ->
            let
                p =
                    Vec3.toRecord t.position
            in
            { t
                | position = vec3 p.x (seaElevation sea p.x p.z time) p.z
                , sinkAge =
                    if t.sunk then
                        t.sinkAge + dt

                    else
                        0
            }
        )
        targets


stepImpacts : Float -> List CombatImpact -> List CombatImpact
stepImpacts dt impacts =
    List.filterMap
        (\imp ->
            if imp.age + dt < 0.7 then
                Just { imp | age = imp.age + dt }

            else
                Nothing
        )
        impacts
