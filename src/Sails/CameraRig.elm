module Sails.CameraRig exposing
    ( Follow
    , dampAngle
    , followView
    , initFollow
    , lookAt
    , stepFollow
    )

{-| Camera placement for Brick Sails.

`Render.Camera.Camera` stores its viewpoint as a `target` plus spherical
`azimuth`, `elevation` and `distance`, with the eye at
`target + eyeOffset`. `lookAt` converts a world-space eye and look-at point
into those values.

The game camera is a high follow camera: it sits well behind and above the
boat, looking down at about 25 degrees so the tall brick masts are seen from
the side rather than end-on (a little round to one side, a three-quarter
view), aims at mid-mast height so the whole boat stays
in the middle of the screen, turns smoothly with the boat, leans
towards the next buoy and pulls back when the buoy is far, so the boat
stays on screen and the buoy comes into view as early as the screen shape
allows (the on-water arrow points at it before that).

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
its distance from the focus point and the focus point itself.
-}
type alias Follow =
    { yaw : Float
    , distance : Float
    , focus : Vec3
    }


pitchDegrees : Float
pitchDegrees =
    25.0


{-| The eye sits this many degrees round from straight behind the boat, a
three-quarter view: brick sails stand in the boat's centre line, so from
dead astern they would be seen edge-on.
-}
sideDegrees : Float
sideDegrees =
    18.0


{-| Height above the water the camera aims at: about half a brick mast, so
hull and masthead both stay on screen.
-}
lookHeight : Float
lookHeight =
    3.5


initFollow : Vec3 -> Float -> Follow
initFollow boat heading =
    { yaw = heading, distance = 40.0, focus = boat }


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
narrow portrait screen pulls the camera back a little further.
-}
stepFollow : Float -> Float -> Vec3 -> Float -> Maybe Vec3 -> Follow -> Follow
stepFollow dt aspect boat heading next follow =
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

                        -- On a narrow portrait screen keep the boat centred; on a
                        -- wide screen there is room to shift towards the buoy.
                        pull =
                            if aspect < 1 then
                                0

                            else
                                min 8 (sep * 0.2) / max 0.001 sep

                        maxLean =
                            if aspect < 1 then
                                30

                            else
                                45
                    in
                    ( heading + clamp -maxLean maxLean (lean * 0.5)
                    , vec3 (b.x + dx * pull) 0 (b.z + dz * pull)
                    , sep
                    )

                Nothing ->
                    ( heading, vec3 b.x 0 b.z, 0 )

        portraitBoost =
            if aspect < 1 then
                1.3

            else
                1.0

        goalDistance =
            clamp 38 60 (30 + separation * 0.4) * portraitBoost
    in
    { yaw = dampAngle 2.2 follow.yaw goalYaw dt
    , distance = follow.distance + (goalDistance - follow.distance) * min 1 (1.2 * dt)
    , focus = Vec3.add follow.focus (Vec3.scale (min 1 (3.0 * dt)) (Vec3.sub goalFocus follow.focus))
    }


{-| Eye and look-at point for the current follow state.
-}
followView : Follow -> { eye : Vec3, target : Vec3 }
followView follow =
    let
        yawRad =
            degrees follow.yaw

        eyeRad =
            degrees (follow.yaw + sideDegrees)

        pitch =
            degrees pitchDegrees

        back =
            follow.distance * cos pitch

        up =
            follow.distance * sin pitch

        f =
            Vec3.toRecord follow.focus

        -- Look a little ahead of the focus, at mid-mast height.
        ahead =
            follow.distance * 0.05
    in
    { eye = vec3 (f.x - sin eyeRad * back) (lookHeight + up) (f.z - cos eyeRad * back)
    , target = vec3 (f.x + sin yawRad * ahead) lookHeight (f.z + cos yawRad * ahead)
    }
