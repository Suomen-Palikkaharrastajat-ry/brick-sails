module Sails.CameraRig exposing (lookAt)

{-| Eye/target placement for the shared orbit camera.

`Render.Camera.Camera` stores its viewpoint as a `target` plus spherical
`azimuth`, `elevation` and `distance`, with the eye at
`target + eyeOffset`. Sails' chase, orbit and helm controllers compute a
world-space eye and look-at point instead; `lookAt` converts between the two
so `Render.Camera.position camera` returns the requested eye.

-}

import Math.Vector3 as Vec3 exposing (Vec3)
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
