module Sails.CameraRig exposing
    ( Follow
    , chaseDistance
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

The game camera is a close chase camera, framed like Brick Racer's chase
camera: it sits a few boat lengths behind the boat and a little above the
masthead, looking down at about 24 degrees (0.42 rad, inside the racer's
0.16 to 0.45 rad chase range), a little round to one side (a three-quarter
view) so the tall brick masts are seen from the side rather than end-on. It
aims at the boat a little under mid-mast height, half a metre ahead as the
racer aims just ahead of the car, follows the boat quickly, turns towards
the next buoy and pulls back only a few metres when the buoy is far (the
on-water arrow points at it before it comes into view). It stays centred on
the boat (it no longer slides towards the buoy), so the close camera keeps
the hull and the masthead on screen.
It is close enough for the player's own build to fill a good part of a
phone screen, and far enough that the tallest brick masthead (9 m, see
`Sails.ShipMesh`) and the whole hull stay in the 45 degree view.

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
    24.0


{-| The eye sits this many degrees round from straight behind the boat, a
three-quarter view: brick sails stand in the boat's centre line, so from
dead astern they would be seen edge-on.
-}
sideDegrees : Float
sideDegrees =
    18.0


{-| Height above the water the camera aims at: a little under half a brick
mast, so from close in the hull stays clear of the bottom of the screen and
the masthead stays on it.
-}
lookHeight : Float
lookHeight =
    3.2


initFollow : Vec3 -> Float -> Follow
initFollow boat heading =
    { yaw = heading, distance = chaseDistance, focus = boat }


{-| Resting follow distance in metres, about two boat lengths (a brick
boat is scaled to about 8.4 m long): close, so the player sees their own
build well.
-}
chaseDistance : Float
chaseDistance =
    17.0


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
    in
    -- Rates as the racer's chase camera (distance 5, target 8.5 per second);
    -- yaw is slower than the racer's 9 so a tack or a buoy rounding swings
    -- the view round smoothly instead of snapping.
    { yaw = dampAngle 3.5 follow.yaw goalYaw dt
    , distance = follow.distance + (goalDistance - follow.distance) * min 1 (5.0 * dt)
    , focus = Vec3.add follow.focus (Vec3.scale (min 1 (8.5 * dt)) (Vec3.sub goalFocus follow.focus))
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
            lookAhead
    in
    { eye = vec3 (f.x - sin eyeRad * back) (lookHeight + up) (f.z - cos eyeRad * back)
    , target = vec3 (f.x + sin yawRad * ahead) lookHeight (f.z + cos yawRad * ahead)
    }
