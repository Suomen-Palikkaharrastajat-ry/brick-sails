module Sails.CannonPick exposing
    ( pickCannon
    , pickRadius
    , projectMuzzles
    , projectToScreen
    )

{-| Which of the player's cannons a tap lands on. Each muzzle (boat space,
`Sails.ShipMesh.Muzzle`) is placed in the world with `Sails.Physics.muzzleWorld`
and projected to screen pixels with the same view and projection the renderer
draws with (`Sails.OceanShader.makeCtx` on the follow camera's eye and target,
as `Sails.Render.renderWorld` does). A tap within `pickRadius` pixels of a
projected muzzle picks the nearest such cannon.
-}

import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Sails.OceanShader as Ocean
import Sails.Physics as Physics
import Sails.Types exposing (SailingState)


{-| How close (in CSS pixels) a tap must be to a projected muzzle to pick it.
-}
pickRadius : Float
pickRadius =
    40.0


{-| Project a world point to screen pixels (origin top left, y down) for a
`width` x `height` viewport seen from `cam`. Nothing when the point is behind
the camera.
-}
projectToScreen : { eye : Vec3, target : Vec3 } -> Int -> Int -> Vec3 -> Maybe { x : Float, y : Float }
projectToScreen cam width height point =
    let
        ctx =
            Ocean.makeCtx cam 0 width height

        m =
            Mat4.toRecord (Mat4.mul ctx.perspective ctx.view)

        p =
            Vec3.toRecord point

        cx =
            m.m11 * p.x + m.m12 * p.y + m.m13 * p.z + m.m14

        cy =
            m.m21 * p.x + m.m22 * p.y + m.m23 * p.z + m.m24

        cw =
            m.m41 * p.x + m.m42 * p.y + m.m43 * p.z + m.m44
    in
    if cw <= 0.0001 then
        Nothing

    else
        Just
            { x = (cx / cw + 1) * 0.5 * toFloat (max 1 width)
            , y = (1 - cy / cw) * 0.5 * toFloat (max 1 height)
            }


{-| The player's muzzles with their screen positions (those in front of the
camera).
-}
projectMuzzles :
    { eye : Vec3, target : Vec3 }
    -> Int
    -> Int
    -> SailingState
    -> List { a | position : Vec3 }
    -> List ( { a | position : Vec3 }, { x : Float, y : Float } )
projectMuzzles cam width height state muzzles =
    List.filterMap
        (\mz ->
            projectToScreen cam width height (Physics.muzzleWorld state mz)
                |> Maybe.map (\s -> ( mz, s ))
        )
        muzzles


{-| The cannon a tap at `tap` (screen pixels) lands on: the muzzle nearest
the tap on screen within `pickRadius`, or Nothing.
-}
pickCannon :
    { eye : Vec3, target : Vec3 }
    -> Int
    -> Int
    -> SailingState
    -> List { a | position : Vec3 }
    -> { x : Float, y : Float }
    -> Maybe { a | position : Vec3 }
pickCannon cam width height state muzzles tap =
    projectMuzzles cam width height state muzzles
        |> List.map
            (\( mz, s ) ->
                ( sqrt ((s.x - tap.x) ^ 2 + (s.y - tap.y) ^ 2), mz )
            )
        |> List.filter (\( d, _ ) -> d <= pickRadius)
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.map Tuple.second
