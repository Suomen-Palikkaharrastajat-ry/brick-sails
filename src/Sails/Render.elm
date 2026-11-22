module Sails.Render exposing
    ( buildShipWorldMatrix
    , renderSailingViewport
    , renderShipyardViewport
    )

{-| WebGL rendering compositor for Brick Sails:
combines sky dome, Gerstner wave ocean (`Sails.OceanShader`), archipelago islands,
IALA navigation buoys, and articulated LEGO ship model (`Render.Scene.renderSceneWithTransform`).
-}

import Html exposing (Html)
import Html.Attributes as Attr
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera exposing (Camera)
import Render.Scene as Scene exposing (Scene)
import Render.Style as Style
import Sails.OceanShader as OceanShader
import Sails.Types
    exposing
        ( Course
        , SailingState
        , ShipSpec
        , lduToMeters
        )
import WebGL


{-| Build the 4x4 world transformation matrix for the ship:
Translation(x, y, z) * RotateY(yaw) * RotateZ(heel) * RotateX(pitch) * Scale(lduToMeters).
-}
buildShipWorldMatrix : SailingState -> Mat4
buildShipWorldMatrix state =
    let
        p =
            Vec3.toRecord state.position

        headingRad =
            state.heading * pi / 180.0

        heelRad =
            state.heelAngle * pi / 180.0

        pitchRad =
            state.pitchAngle * pi / 180.0

        s =
            lduToMeters

        scaleMat =
            Mat4.makeScale3 s s s

        rotPitch =
            Mat4.makeRotate pitchRad (vec3 1 0 0)

        rotHeel =
            Mat4.makeRotate heelRad (vec3 0 0 1)

        rotYaw =
            Mat4.makeRotate -headingRad (vec3 0 1 0)

        transMat =
            Mat4.makeTranslate (vec3 p.x p.y p.z)
    in
    transMat
        |> (\m -> Mat4.mul m rotYaw)
        |> (\m -> Mat4.mul m rotHeel)
        |> (\m -> Mat4.mul m rotPitch)
        |> (\m -> Mat4.mul m scaleMat)


{-| Nautical daylight lighting style for LEGO plastics on the open ocean.
-}
sailingStyle : Style.Style
sailingStyle =
    let
        base =
            Style.defaultStyle

        sunDir =
            Vec3.normalize (vec3 0.45 0.85 0.35)
    in
    { base
        | lightDirection = sunDir
        , ambientStrength = 0.68
        , lightStrength = 0.52
        , specularStrength = 0.22
        , specularPower = 28.0
        , rimStrength = 0.15
        , vibrance = 0.20
        , edgeWidth = 1.2
    }


{-| Render the active 3D Sailing Viewport.
-}
renderSailingViewport :
    Camera
    -> SailingState
    -> ShipSpec
    -> Scene
    -> Course
    -> Float
    -> Int
    -> Int
    -> Html msg
renderSailingViewport camera state spec shipScene course simTime width height =
    let
        shipMat =
            buildShipWorldMatrix state

        sky =
            OceanShader.skyEntity camera width height

        ocean =
            OceanShader.oceanEntity camera simTime width height

        buoys =
            OceanShader.buoyEntity camera course.waypoints width height

        islands =
            OceanShader.islandEntity camera width height

        shipEntities =
            Scene.renderSceneWithTransform shipMat shipScene camera sailingStyle width height

        allEntities =
            [ sky, islands, ocean, buoys ] ++ shipEntities
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False
        , WebGL.antialias
        , WebGL.depth 1
        ]
        [ Attr.width width
        , Attr.height height
        , Attr.class "w-full h-full block"
        ]
        allEntities


{-| Render the Shipyard inspection plinth.
-}
renderShipyardViewport :
    Camera
    -> ShipSpec
    -> Scene
    -> Int
    -> Int
    -> Html msg
renderShipyardViewport camera spec shipScene width height =
    let
        s =
            lduToMeters

        scaleMat =
            Mat4.makeScale3 s s s

        sky =
            OceanShader.skyEntity camera width height

        islands =
            OceanShader.islandEntity camera width height

        ocean =
            OceanShader.oceanEntity camera 0.0 width height

        shipEntities =
            Scene.renderSceneWithTransform scaleMat shipScene camera sailingStyle width height

        allEntities =
            [ sky, islands, ocean ] ++ shipEntities
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False
        , WebGL.antialias
        , WebGL.depth 1
        ]
        [ Attr.width width
        , Attr.height height
        , Attr.class "w-full h-full block"
        ]
        allEntities
