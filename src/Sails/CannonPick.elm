module Sails.CannonPick exposing
    ( allCannons
    , nearestFoe
    , pickCannon
    , pickRadius
    , projectMuzzles
    , projectToScreen
    , selectCannon
    , tapCandidates
    )

{-| Which of the player's cannons a tap lands on. Each muzzle (boat space,
`Sails.ShipMesh.Muzzle`) is placed in the world with `Sails.Physics.muzzleWorld`
and projected to screen pixels with the same view and projection the renderer
draws with (`Sails.OceanShader.makeCtx` on the follow camera's eye and target,
as `Sails.Render.renderWorld` does). A tap within `pickRadius` pixels of a
projected muzzle picks the nearest such cannon.

The tap path (`tapCandidates`) and the desktop F key (`allCannons`) both
choose their cannon with `selectCannon`: of the candidate cannons, the one
nearest the foe being fired at (`nearestFoe`), ties going to the one nearest
the tap.

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
    selectCannon state Nothing (tapCandidates cam width height state muzzles tap)


{-| The cannons a tap at `tap` (screen pixels) is near: those whose
projected muzzle is within `pickRadius`, each with its distance (pixels)
from the tap.
-}
tapCandidates :
    { eye : Vec3, target : Vec3 }
    -> Int
    -> Int
    -> SailingState
    -> List { a | position : Vec3 }
    -> { x : Float, y : Float }
    -> List ( { a | position : Vec3 }, Float )
tapCandidates cam width height state muzzles tap =
    projectMuzzles cam width height state muzzles
        |> List.map
            (\( mz, s ) ->
                ( mz, sqrt ((s.x - tap.x) ^ 2 + (s.y - tap.y) ^ 2) )
            )
        |> List.filter (\( _, d ) -> d <= pickRadius)


{-| Every cannon as a candidate for the F key, which has no tap point: the
tie-break is how far the muzzle is from the middle of the hull fore and aft
(as a broadside picks its muzzle in `Sails.Physics.fireTowardsFrom`).
-}
allCannons : List { a | position : Vec3 } -> List ( { a | position : Vec3 }, Float )
allCannons =
    List.map (\mz -> ( mz, abs (Vec3.getZ mz.position) ))


{-| The foe the cannons are fired at: the aim point (see `Pages.Sail`'s
`aimPoints`) nearest the ship, or Nothing with no foe about.
-}
nearestFoe : SailingState -> List Vec3 -> Maybe Vec3
nearestFoe state aimPoints =
    aimPoints
        |> List.sortBy (Physics.horizontalDistance state.position)
        |> List.head


{-| Within this (m) two cannons are as near the foe as each other, and the
tie-break decides.
-}
sameDistance : Float
sameDistance =
    0.05


{-| Of the `candidates` (each with its tie-break distance, from the tap for
a tap), the cannon to fire: the one whose muzzle (in the world, see
`Sails.Physics.muzzleWorld`) is nearest the `foe`, ties (within
`sameDistance`) going to the smaller tie-break distance. With no foe, the
smallest tie-break distance wins. Nothing with no candidates.
-}
selectCannon : SailingState -> Maybe Vec3 -> List ( { a | position : Vec3 }, Float ) -> Maybe { a | position : Vec3 }
selectCannon state foe candidates =
    let
        foeDistance mz =
            case foe of
                Just f ->
                    Physics.horizontalDistance (Physics.muzzleWorld state mz) f

                Nothing ->
                    0

        order ( a, ta ) ( b, tb ) =
            let
                da =
                    foeDistance a

                db =
                    foeDistance b
            in
            if abs (da - db) > sameDistance then
                compare da db

            else
                compare ta tb
    in
    candidates
        |> List.sortWith order
        |> List.head
        |> Maybe.map Tuple.first
