module Sails.Physics exposing
    ( defaultCourse
    , initSailingState
    , stepPhysics
    , waterElevation
    , waterNormal
    )

{-| Hydrodynamic, aerodynamic, and hydrostatic physics engine for Brick Sails.
-}

import Game.Compass as Compass
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Types
    exposing
        ( BuoyKind(..)
        , Course
        , CourseId(..)
        , SailingState
        , ShipSpec
        , Waypoint
        , Wind
        )


{-| Default Harbor Slalom regatta course with wind blowing from the North (0°).
-}
defaultCourse : Course
defaultCourse =
    { id = CourseHarborSlalom
    , title = "Harbor Regatta Slalom"
    , description = "Tack upwind through the harbor channel buoys, reach across the lighthouse bay, and run downwind to the finish gate."
    , waypoints =
        [ { index = 0, position = vec3 0.0 0.0 60.0, radius = 22.0, kind = PortBuoy }
        , { index = 1, position = vec3 45.0 0.0 150.0, radius = 22.0, kind = StarboardBuoy }
        , { index = 2, position = vec3 -55.0 0.0 260.0, radius = 25.0, kind = PortBuoy }
        , { index = 3, position = vec3 70.0 0.0 380.0, radius = 25.0, kind = StarboardBuoy }
        , { index = 4, position = vec3 0.0 0.0 460.0, radius = 30.0, kind = FinishGate }
        ]
    , wind =
        { speed = 8.0 -- ~16 knots steady sailing breeze
        , direction = 180.0 -- blowing from North to South (towards 180°)
        }
    , startPosition = vec3 0.0 0.0 0.0
    , startHeading = 30.0 -- sailing close-hauled initially
    , targetSeconds = 65.0
    }


{-| Initialize sailing state at the start line.
-}
initSailingState : Course -> SailingState
initSailingState course =
    let
        p =
            course.startPosition

        initV =
            vec3 0.0 0.0 0.0
    in
    { position = p
    , velocity = initV
    , heading = course.startHeading
    , angularVelocity = 0.0
    , heelAngle = 0.0
    , pitchAngle = 0.0
    , heave = 0.0
    , rudderAngle = 0.0
    , sailSheet = 0.35 -- trimmed for close-hauled by default
    , apparentWindSpeed = course.wind.speed
    , apparentWindAngle = 0.0
    , waterSpeed = 0.0
    , driftAngle = 0.0
    }


{-| Gerstner wave water elevation in meters at world coordinates (x, z) and time t.
-}
waterElevation : Float -> Float -> Float -> Float
waterElevation x z t =
    let
        -- Primary swell: wavelength 40m, speed 4 m/s, amplitude 0.45m
        k1 =
            2.0 * pi / 40.0

        w1 =
            k1 * 4.0

        elev1 =
            0.45 * sin (k1 * (z * 0.9 + x * 0.4) - w1 * t)

        -- Secondary cross-chop: wavelength 16m, amplitude 0.20m
        k2 =
            2.0 * pi / 16.0

        w2 =
            k2 * 3.2

        elev2 =
            0.20 * sin (k2 * (x * 0.8 - z * 0.5) - w2 * t)
    in
    elev1 + elev2


{-| Surface normal of the water at (x, z) for ship wave alignment.
-}
waterNormal : Float -> Float -> Float -> Vec3
waterNormal x z t =
    let
        eps =
            0.5

        hC =
            waterElevation x z t

        hX =
            waterElevation (x + eps) z t

        hZ =
            waterElevation x (z + eps) t

        dhDx =
            (hX - hC) / eps

        dhDz =
            (hZ - hC) / eps
    in
    Vec3.normalize (vec3 -dhDx 1.0 -dhDz)


{-| Advance ship physics by time step dt (seconds).
Calculates apparent wind, sail lift/drag, rudder turning, wave interaction, and heel angle.
-}
stepPhysics : Float -> Float -> Wind -> ShipSpec -> SailingState -> SailingState
stepPhysics dt simTime wind spec state =
    let
        clampedDt =
            clamp 0.001 0.05 dt

        posRec =
            Vec3.toRecord state.position

        velRec =
            Vec3.toRecord state.velocity

        -- 1. Forward and lateral unit vectors
        headingRad =
            state.heading * pi / 180.0

        forwardX =
            sin headingRad

        forwardZ =
            cos headingRad

        rightX =
            cos headingRad

        rightZ =
            -sin headingRad

        -- Forward and lateral velocity components over water
        vForward =
            velRec.x * forwardX + velRec.z * forwardZ

        vLateral =
            velRec.x * rightX + velRec.z * rightZ

        speedOverGround =
            sqrt (velRec.x * velRec.x + velRec.z * velRec.z)

        waterSpeedKnots =
            vForward * 1.94384

        -- 2. Apparent wind vector
        app =
            Compass.apparentWind
                { windSpeed = wind.speed, windHeading = wind.direction }
                { shipSpeed = speedOverGround, shipHeading = state.heading }

        appSpeed =
            app.speed

        relWindDeg =
            app.relativeAngle -- [-180, 180] relative to bow

        absRelWind =
            abs relWindDeg

        -- 3. Sail aerodynamics (Lift & Drag)
        -- Sail sheet limits how far the boom can swing out (0.0 = tight at 10°, 1.0 = eased at 80°)
        maxBoomAngle =
            10.0 + state.sailSheet * 70.0

        -- Boom swings to leeward side of apparent wind
        boomAngle =
            if relWindDeg > 0.0 then
                min absRelWind maxBoomAngle

            else
                -(min absRelWind maxBoomAngle)

        -- Angle of attack between sail fabric and apparent wind
        angleAtkDeg =
            absRelWind - abs boomAngle

        angleAtkRad =
            angleAtkDeg * pi / 180.0

        -- In irons check: if apparent wind is within 34° of bow, sails luff/stall
        inIrons =
            absRelWind < 34.0

        ( liftCoeff, dragCoeff ) =
            if inIrons then
                ( 0.0, 0.45 )

            else
                let
                    -- Peak lift around 18-22 degrees angle of attack
                    cl =
                        1.5 * sin (2.0 * clamp 0.0 (pi / 4.0) angleAtkRad)

                    cd =
                        0.12 + 1.1 * (sin angleAtkRad * sin angleAtkRad)
                in
                ( cl, cd )

        -- Dynamic air pressure q = 0.5 * rho * v^2
        airPressure =
            0.5 * 1.225 * (appSpeed * appSpeed)

        sailArea =
            spec.sailAreaSqMeters

        liftForce =
            liftCoeff * airPressure * sailArea

        dragForce =
            dragCoeff * airPressure * sailArea

        -- Project aerodynamic forces into ship forward (thrust) and sideways (heel/drift)
        relWindRad =
            relWindDeg * pi / 180.0

        -- Force along wind direction (drag) + perpendicular to wind (lift)
        fSailAlongWind =
            dragForce

        fSailPerpWind =
            if relWindDeg > 0.0 then
                -liftForce

            else
                liftForce

        -- Project into ship forward axis
        fThrust =
            if inIrons then
                -dragForce * 0.3 -- In irons, wind pushes ship slowly backwards

            else
                fSailPerpWind * sin relWindRad - fSailAlongWind * cos relWindRad

        -- Project into ship lateral axis (heeling & drift force)
        fHeelLateral =
            if inIrons then
                0.0

            else
                fSailPerpWind * cos relWindRad + fSailAlongWind * sin relWindRad

        -- 4. Hydrodynamic water resistance
        -- Forward skin friction + quadratic wave-making resistance
        fricCoeff =
            0.08 * spec.mass

        waveResistCoeff =
            0.02 * spec.mass * (1.0 + max 0.0 (vForward - 3.5) * 1.5)

        waterDrag =
            fricCoeff * vForward + waveResistCoeff * vForward * abs vForward

        -- Hull/keel lateral resistance prevents sliding sideways
        lateralDrag =
            -vLateral * (spec.mass * 4.5)

        -- Total longitudinal and lateral acceleration
        netForwardForce =
            fThrust - waterDrag

        accelForward =
            netForwardForce / spec.mass

        accelLateral =
            (fHeelLateral + lateralDrag) / spec.mass

        -- Integrate linear velocity
        newVForward =
            max -1.5 (vForward + accelForward * clampedDt)

        newVLateral =
            vLateral + accelLateral * clampedDt

        newVelX =
            newVForward * forwardX + newVLateral * rightX

        newVelZ =
            newVForward * forwardZ + newVLateral * rightZ

        newPosX =
            posRec.x + newVelX * clampedDt

        newPosZ =
            posRec.z + newVelZ * clampedDt

        -- 5. Steering and Rudder dynamics
        -- Turning torque depends on water flowing past the rudder (vForward)
        rudderRad =
            state.rudderAngle * pi / 180.0

        rudderTorque =
            -sin rudderRad * (newVForward * 1.8)

        yawDamping =
            -state.angularVelocity * 2.2

        angularAccel =
            rudderTorque + yawDamping

        newAngVel =
            state.angularVelocity + angularAccel * clampedDt

        newHeading =
            modBy 360 (round ((state.heading + newAngVel * (180.0 / pi) * clampedDt) + 360.0))
                |> toFloat

        -- 6. Heeling and Wave hydrostatics
        -- Aerodynamic heeling moment: M_heel = F_heel * mast_lever_arm
        ceRec =
            Vec3.toRecord spec.centerOfEffort

        clrRec =
            Vec3.toRecord spec.centerOfLateralResistance

        leverArm =
            max 1.5 (ceRec.y - clrRec.y)

        heelingMoment =
            fHeelLateral * leverArm

        -- Restoring righting moment: M_righting = mass * g * GM * sin(heel)
        targetHeelRad =
            clamp (-pi / 3.0) (pi / 3.0) (heelingMoment / (spec.mass * 9.81 * max 0.4 spec.metacentricHeight))

        targetHeelDeg =
            targetHeelRad * 180.0 / pi

        -- Smooth heel response
        newHeelDeg =
            state.heelAngle + (targetHeelDeg - state.heelAngle) * (clampedDt * 4.0)

        -- Water wave interaction
        waveElev =
            waterElevation newPosX newPosZ simTime

        norm =
            Vec3.toRecord (waterNormal newPosX newPosZ simTime)

        wavePitchDeg =
            -asin (clamp -0.99 0.99 (norm.z * forwardZ + norm.x * forwardX)) * (180.0 / pi)

        newPitchDeg =
            state.pitchAngle + (wavePitchDeg - state.pitchAngle) * (clampedDt * 5.0)
    in
    { position = vec3 newPosX waveElev newPosZ
    , velocity = vec3 newVelX 0.0 newVelZ
    , heading = newHeading
    , angularVelocity = newAngVel
    , heelAngle = newHeelDeg
    , pitchAngle = newPitchDeg
    , heave = waveElev
    , rudderAngle = state.rudderAngle
    , sailSheet = state.sailSheet
    , apparentWindSpeed = appSpeed
    , apparentWindAngle = relWindDeg
    , waterSpeed = waterSpeedKnots
    , driftAngle = atan2 newVLateral (max 0.1 (abs newVForward)) * (180.0 / pi)
    }
