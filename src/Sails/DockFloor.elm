module Sails.DockFloor exposing
    ( Vertex
    , entities
    )

{-| The harbour around the berth of the Brick Sails ship builder:

  - The brick dock of `Sails.BuilderDock` (studded plates: a side pier, a
    stern quay, kerb rails and mooring bollards), fixed scenery that is never
    part of the ship.
  - Translucent marine sea water surface plane beneath the ship's waterline.
  - Multi-ring soft contact shadow / water hull reflection directly beneath the ship.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.Camera as Camera exposing (Camera)
import Render.Scene as Scene exposing (Scene)
import Render.Style exposing (Style)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


{-| Vertex with 3D position and RGBA color.
-}
type alias Vertex =
    { position : Vec3
    , color : Vec4
    }


type alias Uniforms =
    { viewMatrix : Mat4
    , projectionMatrix : Mat4
    , modelMatrix : Mat4
    }


vertexShader : Shader Vertex Uniforms { vColor : Vec4 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec4 color;

        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform mat4 modelMatrix;

        varying vec4 vColor;

        void main() {
            vColor = color;
            gl_Position = projectionMatrix * viewMatrix * (modelMatrix * vec4(position, 1.0));
        }
    |]


fragmentShader : Shader {} Uniforms { vColor : Vec4 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec4 vColor;

        void main() {
            gl_FragColor = vColor;
        }
    |]



-- ── Geometry Helpers ─────────────────────────────────────────────────────────


quad : Vertex -> Vertex -> Vertex -> Vertex -> List ( Vertex, Vertex, Vertex )
quad v1 v2 v3 v4 =
    [ ( v1, v2, v3 )
    , ( v1, v3, v4 )
    ]


horizontalQuad : Float -> Float -> Float -> Float -> Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
horizontalQuad y x1 x2 z1 z2 col =
    let
        v1 =
            { position = vec3 x1 y z1, color = col }

        v2 =
            { position = vec3 x2 y z1, color = col }

        v3 =
            { position = vec3 x2 y z2, color = col }

        v4 =
            { position = vec3 x1 y z2, color = col }
    in
    quad v1 v2 v3 v4



-- ── Water Palette ────────────────────────────────────────────────────────────


waterColor : Vec4
waterColor =
    vec4 0.06 0.36 0.52 0.78


waterLevelY : Float
waterLevelY =
    12.0



-- ── Harbor Water & Hull Contact Shadow ───────────────────────────────────────


buildWaterTriangles : List ( Vertex, Vertex, Vertex )
buildWaterTriangles =
    -- Main translucent harbor sea surface plane (uniform marine water without harsh slipway rectangle)
    horizontalQuad waterLevelY -380.0 380.0 -380.0 380.0 waterColor


buildShadowTriangles : List ( Vertex, Vertex, Vertex )
buildShadowTriangles =
    let
        shadowY =
            waterLevelY - 0.08

        segments =
            24

        step =
            (2.0 * pi) / toFloat segments

        -- 3 Concentric elliptical rings matched to ship hull proportions
        r1X =
            26.0

        r1Z =
            75.0

        col1 =
            vec4 0.0 0.08 0.14 0.65

        r2X =
            48.0

        r2Z =
            110.0

        col2 =
            vec4 0.0 0.08 0.14 0.32

        r3X =
            75.0

        r3Z =
            150.0

        col3 =
            vec4 0.0 0.08 0.14 0.0

        center =
            { position = vec3 0.0 shadowY 0.0, color = vec4 0.0 0.08 0.14 0.75 }

        ringPoint rx rz col angle =
            { position = vec3 (rx * cos angle) shadowY (rz * sin angle), color = col }

        buildFan i =
            let
                a1 =
                    toFloat i * step

                a2 =
                    toFloat (i + 1) * step

                p1Inner =
                    ringPoint r1X r1Z col1 a1

                p2Inner =
                    ringPoint r1X r1Z col1 a2

                p1Mid =
                    ringPoint r2X r2Z col2 a1

                p2Mid =
                    ringPoint r2X r2Z col2 a2

                p1Outer =
                    ringPoint r3X r3Z col3 a1

                p2Outer =
                    ringPoint r3X r3Z col3 a2
            in
            List.concat
                [ -- Center triangle
                  [ ( center, p2Inner, p1Inner ) ]
                , -- Inner to mid quad
                  quad p1Inner p2Inner p2Mid p1Mid
                , -- Mid to outer quad
                  quad p1Mid p2Mid p2Outer p1Outer
                ]
    in
    List.concatMap buildFan (List.range 0 (segments - 1))


waterMesh : Mesh Vertex
waterMesh =
    WebGL.triangles (List.concat [ buildWaterTriangles, buildShadowTriangles ])



-- ── Entities API ─────────────────────────────────────────────────────────────


{-| WebGL entities for the harbour around the berth:

  - The brick dock (`dock`, the mesh of `Sails.BuilderDock`), once its parts
    are in; drawn with the studio's `style` on a `width` x `height` canvas.
  - Translucent harbor sea water surface plane.
  - Soft aquatic hull shadow directly under the ship.

-}
entities :
    { baselineY : Float
    , dock : Maybe Scene
    , style : Style
    , width : Int
    , height : Int
    }
    -> Camera
    -> Float
    -> List Entity
entities config camera aspect =
    let
        -- The water is modelled Y down (larger y = lower, as in LDraw), but
        -- extra entities are drawn in render space, Y up. Flip Y only (Z
        -- keeps the stern quay where it is), then lift by `baselineY`, so a
        -- dock y lands at render y = baselineY - y. No face culling is set,
        -- so the reversed winding is harmless.
        modelMatrix =
            Mat4.mul
                (Mat4.makeTranslate (vec3 0.0 config.baselineY 0.0))
                (Mat4.makeScale3 1.0 -1.0 1.0)

        -- The brick dock is already Y-up: `Sails.ShipMesh.buildScenery`
        -- negates LDraw y and z (and `Sails.BuilderDock` stores -z), so it
        -- only needs the same lift to land where the water does.
        dockMatrix =
            Mat4.makeTranslate (vec3 0.0 config.baselineY 0.0)

        uniforms =
            { viewMatrix = Camera.viewMatrix camera
            , projectionMatrix = Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)
            , modelMatrix = modelMatrix
            }

        translucentSettings =
            [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
            , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            ]

        dockEntities =
            case config.dock of
                Just scene ->
                    Scene.renderSceneWithTransform dockMatrix scene camera config.style config.width config.height

                Nothing ->
                    []

        waterEntity =
            WebGL.entityWith translucentSettings vertexShader fragmentShader waterMesh uniforms
    in
    dockEntities ++ [ waterEntity ]
