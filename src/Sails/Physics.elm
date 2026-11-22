module Sails.Physics exposing
    ( StepInput
    , angleDiff
    , bonusPerHit
    , cannonRange
    , checkShipHits
    , checkTargetHits
    , defaultCourse
    , defaultSailLevel
    , finalTime
    , fireCannons
    , initProgress
    , initSailingState
    , overpower
    , sailDrive
    , sailLevel
    , sailLevels
    , sailLoad
    , seaElevation
    , starRating
    , stepBoat
    , stepImpacts
    , stepProgress
    , stepProjectiles
    , stepTargets
    , swellHeading
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
        , BuoyKind(..)
        , Cannonball
        , CombatImpact
        , Course
        , CourseProgress
        , Island
        , SailingState
        , Target
        , Waypoint
        , Wind
        )



-- COURSE


{-| A short loop around a small island: seven buoys in order, the last one
is the finish gate back near the start. Seven floating targets sit beside
the legs for bonus time.
-}
defaultCourse : Course
defaultCourse =
    { id = "island-loop"
    , title = "Island Loop"
    , description = "Sail through the buoys in order, round the island and back to the finish gate."
    , waypoints =
        [ buoy 0 30 38 PortBuoy
        , buoy 1 68 98 StarboardBuoy
        , buoy 2 30 158 PortBuoy
        , buoy 3 -30 188 StarboardBuoy
        , buoy 4 -82 142 PortBuoy
        , buoy 5 -68 75 StarboardBuoy
        , { index = 6, position = vec3 -22 0 22, radius = 10.0, kind = FinishGate }
        ]
    , targets =
        List.indexedMap
            (\i ( x, z ) -> { id = i, position = vec3 x 0 z, sunk = False, sinkAge = 0 })
            [ ( 26, 8 ), ( 62, 60 ), ( 64, 134 ), ( 4, 190 ), ( -66, 175 ), ( -92, 108 ), ( -56, 36 ) ]
    , islands =
        [ { x = -12, z = 100, radius = 24, height = 6 }
        , { x = 150, z = 90, radius = 40, height = 10 }
        , { x = -170, z = 230, radius = 50, height = 12 }
        , { x = 40, z = 330, radius = 45, height = 9 }
        , { x = -130, z = -60, radius = 35, height = 7 }
        ]
    , wind = { speed = 7.0, direction = 270.0 }
    , startPosition = vec3 0 0 0
    , startHeading = 38.0
    , targetSeconds = 50.0
    }


buoy : Int -> Float -> Float -> BuoyKind -> Waypoint
buoy i x z kind =
    { index = i, position = vec3 x 0 z, radius = 8.0, kind = kind }


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
            state.steer + (clamp -1 1 input.steer - state.steer) * min 1 (dt * 9)

        speedRatio =
            clamp 0 1 (state.speed / boat.topSpeed)

        -- Hoisting or lowering takes a second or two.
        sail =
            state.sail + (clamp 0 1.2 input.sail - state.sail) * min 1 (dt * 1.6)

        over =
            overpower sail wind

        -- An overpowered boat rounds up: the helm is pulled towards the wind.
        roundUp =
            5 * over * sin (degrees (angleDiff (wind.direction + 180) state.heading))

        -- Steering right lowers the heading (screen right in the follow camera).
        heading =
            normalize
                (state.heading
                    - boat.turnRate
                    * sailHelm sail wind
                    * steer
                    * (0.55 + 0.45 * speedRatio)
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
            state.speed + (targetSpeed - state.speed) * min 1 (dt * boat.acceleration)

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



-- PROGRESS


{-| Count time and tick off the next buoy when the boat is within its radius.
Returns the index of the buoy just passed, if any.
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
                if horizontalDistance state.position wp.position <= wp.radius then
                    let
                        next =
                            progress.currentWaypointIndex + 1
                    in
                    ( { ticked
                        | currentWaypointIndex = next
                        , splits = progress.splits ++ [ elapsed ]
                        , finished = next >= progress.totalWaypoints
                      }
                    , Just progress.currentWaypointIndex
                    )

                else
                    ( ticked, Nothing )

            Nothing ->
                ( { ticked | finished = True }, Nothing )


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


{-| Race time minus the target bonus.
-}
finalTime : CourseProgress -> Float
finalTime progress =
    max 0 (progress.elapsedSeconds - bonusPerHit * toFloat progress.targetsHit)


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


{-| Age sinking targets; keep their bob in sync with the water.
-}
stepTargets : Float -> Float -> List Target -> List Target
stepTargets dt time targets =
    List.map
        (\t ->
            let
                p =
                    Vec3.toRecord t.position
            in
            { t
                | position = vec3 p.x (waterElevation p.x p.z time) p.z
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
