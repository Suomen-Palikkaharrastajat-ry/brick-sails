module Sails.CameraRig exposing
    ( Follow
    , chaseDistance
    , dampAngle
    , followView
    , initFollow
    , jolt
    , knockShake
    , lookAt
    , lookYawDegrees
    , setBroadsideBias
    , setShootFocus
    , stepFollow
    )

{-| Camera placement for Brick Sails.

`Render.Camera.Camera` stores its viewpoint as a `target` plus spherical
`azimuth`, `elevation` and `distance`, with the eye at
`target + eyeOffset`. `lookAt` converts a world-space eye and look-at point
into those values.

The game camera is a close chase camera, framed like Brick Racer's chase
camera: it sits about one and a half boat lengths behind the boat, looking
down at about 24 degrees (0.42 rad, inside the racer's
0.16 to 0.45 rad chase range), a little round to one side (a three-quarter
view) so the tall brick masts are seen from the side rather than end-on. It
aims at the boat at mid-mast height, half a metre ahead as the racer aims
just ahead of the car, follows the boat quickly, turns towards
the next buoy and pulls back only a few metres when the buoy is far (the
on-water arrow points at it before it comes into view). It stays centred on
the boat (it no longer slides towards the buoy), so the close camera keeps
the hull on screen.
As in Brick Racer, its aim leads a little towards the way the bow is turning
(`turnLeadMax`), speed sinks the view a degree or two (`speedSinkMax`), and a
hard knock on rock or a break-up jolts it with a short shake scaled to the
hit (`knockShake`, `jolt`); light nudges do not. The distance is unchanged,
so the whole boat stays on a landscape phone screen.
It is close enough for the player's own build to fill most of a phone
screen. At this range the tip of the tallest brick masthead (9 m, see
`Sails.ShipMesh`) and the stern corner nearest the camera reach the edges of
the 45 degree view.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera exposing (Camera)


{-| Place the camera eye at the first point, looking at the second.
-}
lookAt : Vec3 -> Vec3 -> Camera -> Camera
lookAt eye target cam =
    let
        offset =
            Vec3.toRecord (Vec3.sub eye target)

        dist =
            max 0.001 (sqrt (offset.x * offset.x + offset.y * offset.y + offset.z * offset.z))
    in
    { cam
        | target = target
        , distance = dist
        , elevation = asin (clamp -1 1 (offset.y / dist))
        , azimuth = atan2 offset.x offset.z
    }


{-| Smoothed follow-camera state: the compass yaw the camera looks along,
its distance from the focus point and the focus point itself; the boat's
heading last frame (for its turn rate), the sideways turn lead in metres,
how many degrees speed sinks the view, and the shake (0 to 1) with the clock
that drives its jitter.
-}
type alias Follow =
    { yaw : Float
    , distance : Float
    , focus : Vec3
    , heading : Float
    , lead : Float
    , sink : Float
    , shake : Float
    , clock : Float
    , broadsideBias : Float
    , shootFocusTimer : Float
    , targetBroadsideBias : Float
    , shootBearing : Float
    , side : Float
    }


pitchDegrees : Float
pitchDegrees =
    24.0


{-| The eye sits this many degrees round from straight behind the boat, a
three-quarter view: brick sails stand in the boat's centre line, so from
dead astern they would be seen edge-on.
-}
sideDegrees : Float
sideDegrees =
    18.0


{-| Height above the water the camera aims at: mid-mast (half of the 9 m
brick masthead), so the boat sits in the middle of the view as the racer's
car does.
-}
lookHeight : Float
lookHeight =
    4.0


initFollow : Vec3 -> Float -> Follow
initFollow boat heading =
    { yaw = heading
    , distance = chaseDistance
    , focus = boat
    , heading = heading
    , lead = 0
    , sink = 0
    , shake = 0
    , clock = 0
    , broadsideBias = 0
    , shootFocusTimer = 0
    , targetBroadsideBias = 0
    , shootBearing = 0
    , side = 1
    }


{-| Resting follow distance in metres, about one and a half boat lengths (a
brick boat is scaled to about 8.4 m long): close, so the player sees their
own build well.
-}
chaseDistance : Float
chaseDistance =
    13.0


{-| The most the camera pulls back past `chaseDistance` for a far buoy.
-}
maxPullBack : Float
maxPullBack =
    3.0


{-| How far ahead of the boat the camera aims, in metres: the racer aims
0.85 m (about one car radius) ahead of the car.
-}
lookAhead : Float
lookAhead =
    0.5


{-| Most the aim leads sideways towards a turn, in metres, and metres of
lead per radian a second of turn: a hard turn (about 30 degrees a second)
moves the aim a metre and a half, an eighth of the chase distance, as the
racer's 0.5 m is at its 3 to 4 m.
-}
turnLeadMax : Float
turnLeadMax =
    1.5


turnLeadGain : Float
turnLeadGain =
    3.0


{-| Most degrees speed sinks the view, at `sinkSpeed` m/s and above, for a
lower, faster feel without the boat getting smaller.
-}
speedSinkMax : Float
speedSinkMax =
    2.0


sinkSpeed : Float
sinkSpeed =
    10.0


{-| How far a full shake moves the aim, in metres, and how fast it dies
away (per second, as the racer's).
-}
shakeAmplitude : Float
shakeAmplitude =
    0.5


shakeDecay : Float
shakeDecay =
    5.0


{-| Shake (0 to 1) from a knock on rock at a closing speed in m/s: none for
a nudge that does no harm (under 2.5 m/s, see `Sails.Hull.knockDamage`),
then rising to about 0.8 for a full-speed crash (9 m/s).
-}
knockShake : Float -> Float
knockShake speed =
    clamp 0 1 (max 0 (speed - 2.5) * 0.12)


{-| Smoothly pivot camera towards a shot's world bearing (degrees) to frame the cannonballs.
-}
setBroadsideBias : Float -> Follow -> Follow
setBroadsideBias bearing follow =
    setShootFocus bearing 1.8 follow


{-| Smoothly pivot camera towards cannon shot direction and hold focus while shooting is active.
`bearing` is the shot's world bearing in degrees (same convention as heading);
each frame the bias is recomputed as `bearing - yaw`, so the camera keeps
framing the shot while the ship turns. When the timer expires, the camera
starts returning to its usual position.
-}
setShootFocus : Float -> Float -> Follow -> Follow
setShootFocus bearing duration follow =
    { follow
        | shootBearing = bearing
        , shootFocusTimer = max 0 duration
    }


{-| Add a shake (0 to 1) to the camera; shakes add up to at most 1.
-}
jolt : Float -> Follow -> Follow
jolt amount follow =
    { follow | shake = clamp 0 1 (follow.shake + max 0 amount) }


{-| Move an angle (degrees) towards a goal along the short way round.
-}
dampAngle : Float -> Float -> Float -> Float -> Float
dampAngle rate current goal dt =
    let
        d =
            goal - current - 360.0 * toFloat (floor ((goal - current + 180.0) / 360.0))
    in
    current + d * min 1 (rate * dt)


{-| Advance the follow camera one frame. `aspect` is width / height: a
narrow portrait screen pulls the camera back a little further. `speed` is
the boat's speed in m/s.
-}
stepFollow : Float -> Float -> Vec3 -> Float -> Float -> Maybe Vec3 -> Follow -> Follow
stepFollow dt aspect boat heading speed next follow =
    let
        b =
            Vec3.toRecord boat

        ( goalYaw, goalFocus, separation ) =
            case next of
                Just n ->
                    let
                        nr =
                            Vec3.toRecord n

                        dx =
                            nr.x - b.x

                        dz =
                            nr.z - b.z

                        sep =
                            sqrt (dx * dx + dz * dz)

                        bearing =
                            atan2 dx dz * 180 / pi

                        lean =
                            bearing - heading - 360.0 * toFloat (floor ((bearing - heading + 180.0) / 360.0))

                        maxLean =
                            if aspect < 1 then
                                30

                            else
                                45
                    in
                    ( heading + clamp -maxLean maxLean (lean * 0.5)
                    , vec3 b.x 0 b.z
                    , sep
                    )

                Nothing ->
                    ( heading, vec3 b.x 0 b.z, 0 )

        -- A narrow portrait screen pulls back so a boat seen from the side
        -- still fits across it.
        portraitBoost =
            if aspect < 1 then
                1.25

            else
                1.0

        goalDistance =
            clamp chaseDistance (chaseDistance + maxPullBack) (chaseDistance + separation * 0.02) * portraitBoost

        -- Turn rate in radians a second, positive while the bow swings to
        -- starboard (the compass heading grows).
        turnRate =
            if dt > 0 then
                degrees (heading - follow.heading - 360.0 * toFloat (floor ((heading - follow.heading + 180.0) / 360.0))) / dt

            else
                0

        goalLead =
            clamp -turnLeadMax turnLeadMax (turnRate * turnLeadGain)

        goalSink =
            speedSinkMax * clamp 0 1 (abs speed / sinkSpeed)

        newYaw =
            dampAngle 3.5 follow.yaw goalYaw dt

        -- While the shot focus runs, aim the camera at the shot's world
        -- bearing, whatever the ship is doing.
        shotRel =
            follow.shootBearing - newYaw - 360.0 * toFloat (floor ((follow.shootBearing - newYaw + 180.0) / 360.0))

        shotBias =
            (if shotRel >= 0 then
                1.0

             else
                -1.0
            )
                * clamp 40.0 65.0 (abs shotRel)
    in
    -- Rates as the racer's chase camera (distance 5, target 8.5 per second);
    -- yaw is slower than the racer's 9 so a tack or a buoy rounding swings
    -- the view round smoothly instead of snapping.
    { yaw = newYaw
    , distance = follow.distance + (goalDistance - follow.distance) * min 1 (5.0 * dt)
    , focus = Vec3.add follow.focus (Vec3.scale (min 1 (8.5 * dt)) (Vec3.sub goalFocus follow.focus))
    , heading = heading
    , lead = follow.lead + (goalLead - follow.lead) * min 1 (3.0 * dt)
    , sink = follow.sink + (goalSink - follow.sink) * min 1 (2.0 * dt)
    , shake =
        if follow.shake < 0.01 then
            0

        else
            follow.shake * max 0 (1 - shakeDecay * dt)
    , clock = follow.clock + dt
    , broadsideBias =
        if follow.shootFocusTimer > 0 then
            follow.broadsideBias + (shotBias - follow.broadsideBias) * min 1 (6.0 * dt)

        else if abs follow.broadsideBias < 0.1 then
            0

        else
            follow.broadsideBias * max 0 (1 - 2.5 * dt)
    , shootFocusTimer =
        max 0 (follow.shootFocusTimer - dt)
    , targetBroadsideBias =
        if follow.shootFocusTimer > 0 then
            shotBias

        else
            0
    , shootBearing = follow.shootBearing
    , side =
        -- Eases between the two sides, so a shot to port does not swing the
        -- view 36 degrees in one frame when its bias crosses zero.
        follow.side
            + ((if follow.broadsideBias < 0 then
                    -1

                else
                    1
               )
                - follow.side
              )
            * min 1 (3.0 * dt)
    }


{-| The yaw the camera looks along, in degrees. The side offset follows the
side of the shot: it mirrors when the broadside bias is negative, so a right
shot is framed as a mirror of a left shot. With no bias (sign of 0) it keeps
the old +sideDegrees behaviour.
-}
lookYawDegrees : Follow -> Float
lookYawDegrees follow =
    follow.yaw + follow.broadsideBias + follow.side * sideDegrees


{-| Eye and look-at point for the current follow state.
-}
followView : Follow -> { eye : Vec3, target : Vec3 }
followView follow =
    let
        effectiveYaw =
            follow.yaw + follow.broadsideBias

        yawRad =
            degrees effectiveYaw

        eyeRad =
            degrees (lookYawDegrees follow)

        pitch =
            degrees (pitchDegrees - follow.sink)

        back =
            follow.distance * cos pitch

        up =
            follow.distance * sin pitch

        f =
            Vec3.toRecord follow.focus

        -- Look a little ahead of the focus, at mid-mast height, and a
        -- little towards the way the bow is turning (starboard is
        -- (cos yaw, -sin yaw)).
        ahead =
            lookAhead

        lead =
            follow.lead

        t =
            follow.clock

        amp =
            follow.shake * shakeAmplitude

        jitter =
            if amp < 0.005 then
                vec3 0 0 0

            else
                vec3 (amp * sin (t * 53.0)) (amp * 0.6 * sin (t * 71.0)) (amp * cos (t * 61.0))

        target =
            vec3 (f.x + sin yawRad * ahead + cos yawRad * lead) lookHeight (f.z + cos yawRad * ahead - sin yawRad * lead)
    in
    { eye = vec3 (f.x - sin eyeRad * back) (lookHeight + up) (f.z - cos eyeRad * back)
    , target = Vec3.add target jitter
    }
