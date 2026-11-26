module Sails.DockFloor exposing
    ( Vertex
    , entities
    )

{-| Dedicated 3D wooden dock harbor environment for the Brick Sails ship builder.

Features:

  - Authentic nautical wooden pier running alongside the ship berth with
    weathered timber planks (warm teak, aged oak, cedar, pine) and dark plank seams.
  - Heavy bumper timber edge beam (rub-rail) with dark iron strapping facing
    the vessel berth.
  - Stern boardwalk cross-quay enclosing the mooring slip.
  - Vertical wooden mooring pilings / bollard posts standing proud of the pier deck,
    capped with dark cast iron collars.
  - Translucent marine sea water surface plane beneath the ship's waterline.
  - Multi-ring soft contact shadow / water hull reflection directly beneath the ship.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.Camera as Camera exposing (Camera)
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


verticalQuadX : Float -> Float -> Float -> Float -> Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
verticalQuadX x y1 y2 z1 z2 col =
    let
        v1 =
            { position = vec3 x y1 z1, color = col }

        v2 =
            { position = vec3 x y1 z2, color = col }

        v3 =
            { position = vec3 x y2 z2, color = col }

        v4 =
            { position = vec3 x y2 z1, color = col }
    in
    quad v1 v2 v3 v4


verticalQuadZ : Float -> Float -> Float -> Float -> Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
verticalQuadZ z x1 x2 y1 y2 col =
    let
        v1 =
            { position = vec3 x1 y1 z, color = col }

        v2 =
            { position = vec3 x2 y1 z, color = col }

        v3 =
            { position = vec3 x2 y2 z, color = col }

        v4 =
            { position = vec3 x1 y2 z, color = col }
    in
    quad v1 v2 v3 v4


box : Float -> Float -> Float -> Float -> Float -> Float -> Vec4 -> List ( Vertex, Vertex, Vertex )
box x1 x2 y1 y2 z1 z2 col =
    List.concat
        [ horizontalQuad y1 x1 x2 z1 z2 col
        , horizontalQuad y2 x1 x2 z1 z2 col
        , verticalQuadX x1 y1 y2 z1 z2 col
        , verticalQuadX x2 y1 y2 z1 z2 col
        , verticalQuadZ z1 x1 x2 y1 y2 col
        , verticalQuadZ z2 x1 x2 y1 y2 col
        ]



-- ── Nautical Dock Palette ────────────────────────────────────────────────────


plankTeak : Vec4
plankTeak =
    vec4 0.52 0.38 0.24 1.0


plankOak : Vec4
plankOak =
    vec4 0.46 0.33 0.2 1.0


plankCedar : Vec4
plankCedar =
    vec4 0.4 0.28 0.18 1.0


plankPine : Vec4
plankPine =
    vec4 0.56 0.41 0.26 1.0


plankSeam : Vec4
plankSeam =
    vec4 0.14 0.1 0.06 1.0


timberBumper : Vec4
timberBumper =
    vec4 0.28 0.18 0.1 1.0


ironFitting : Vec4
ironFitting =
    vec4 0.14 0.16 0.18 1.0


pilingTimber : Vec4
pilingTimber =
    vec4 0.36 0.25 0.16 1.0


waterColor : Vec4
waterColor =
    vec4 0.06 0.36 0.52 0.78


waterShallowColor : Vec4
waterShallowColor =
    vec4 0.1 0.46 0.58 0.85



-- ── Pier & Mooring Geometry ──────────────────────────────────────────────────


pierDeckY : Float
pierDeckY =
    waterLevelY - 0.2


waterLevelY : Float
waterLevelY =
    12.0


underwaterSkirtY : Float
underwaterSkirtY =
    24.0


buildPierPlanks : List ( Vertex, Vertex, Vertex )
buildPierPlanks =
    let
        plankWidth =
            12.0

        gap =
            2.0

        stepZ =
            plankWidth + gap

        colorForIndex idx =
            case modBy 4 idx of
                0 ->
                    plankTeak

                1 ->
                    plankOak

                2 ->
                    plankCedar

                _ ->
                    plankPine

        buildSidePlank idx =
            let
                zStart =
                    -180.0 + toFloat idx * stepZ

                zEnd =
                    zStart + plankWidth

                plankCol =
                    colorForIndex idx
            in
            List.concat
                [ -- Wooden plank top
                  horizontalQuad pierDeckY 46.0 180.0 zStart zEnd plankCol
                , -- Dark seam gap between planks
                  horizontalQuad (pierDeckY + 0.1) 46.0 180.0 zEnd (zEnd + gap) plankSeam
                ]

        sidePlanksCount =
            25

        sidePlanks =
            List.concatMap buildSidePlank (List.range 0 (sidePlanksCount - 1))

        buildSternPlank idx =
            let
                xStart =
                    -140.0 + toFloat idx * (plankWidth + gap)

                xEnd =
                    xStart + plankWidth

                plankCol =
                    colorForIndex (idx + 1)
            in
            List.concat
                [ -- Wooden plank top on stern quay
                  horizontalQuad pierDeckY xStart xEnd 126.0 180.0 plankCol
                , -- Dark seam gap
                  horizontalQuad (pierDeckY + 0.1) xEnd (xEnd + gap) 126.0 180.0 plankSeam
                ]

        sternPlanksCount =
            13

        sternPlanks =
            List.concatMap buildSternPlank (List.range 0 (sternPlanksCount - 1))
    in
    List.concat [ sidePlanks, sternPlanks ]


buildDockStructure : List ( Vertex, Vertex, Vertex )
buildDockStructure =
    let
        -- Heavy edge timber bumper beam (rub-rail) along starboard berth (X = 42..46, Z = -180..180)
        sideBumper =
            List.concat
                [ -- Top face of bumper
                  horizontalQuad pierDeckY 42.0 46.0 -180.0 180.0 timberBumper
                , -- Vertical face facing the ship berth extending down into the water
                  verticalQuadX 42.0 pierDeckY underwaterSkirtY -180.0 180.0 timberBumper
                , -- Outer vertical side of pier foundation (X = 180.0)
                  verticalQuadX 180.0 pierDeckY underwaterSkirtY -180.0 180.0 timberBumper
                , -- Pier front endcap (Z = -180.0)
                  verticalQuadZ -180.0 42.0 180.0 pierDeckY underwaterSkirtY timberBumper
                ]

        -- Heavy timber rub-rail along stern cross-quay (Z = 122..126, X = -140..42)
        sternBumper =
            List.concat
                [ -- Top face of stern bumper
                  horizontalQuad pierDeckY -140.0 42.0 122.0 126.0 timberBumper
                , -- Vertical face facing water
                  verticalQuadZ 122.0 -140.0 42.0 pierDeckY underwaterSkirtY timberBumper
                , -- West endcap of stern quay (X = -140.0)
                  verticalQuadX -140.0 pierDeckY underwaterSkirtY 122.0 180.0 timberBumper
                , -- South perimeter of stern quay (Z = 180.0)
                  verticalQuadZ 180.0 -140.0 180.0 pierDeckY underwaterSkirtY timberBumper
                ]

        -- Iron strapping brackets along bumper rail at intervals
        ironBandsZ =
            [ -150.0, -90.0, -30.0, 30.0, 90.0, 150.0 ]

        ironBandZ zPos =
            List.concat
                [ horizontalQuad (pierDeckY - 0.1) 41.8 46.2 (zPos - 1.5) (zPos + 1.5) ironFitting
                , verticalQuadX 41.8 (pierDeckY - 0.1) (pierDeckY + 10.0) (zPos - 1.5) (zPos + 1.5) ironFitting
                ]

        sideBands =
            List.concatMap ironBandZ ironBandsZ
    in
    List.concat
        [ sideBumper
        , sternBumper
        , sideBands
        ]


buildMooringPilings : List ( Vertex, Vertex, Vertex )
buildMooringPilings =
    let
        -- 8x8 LDU wooden mooring post
        pilingHalfSize =
            4.0

        pilingTopY =
            pierDeckY - 14.0

        pilingBottomY =
            26.0

        collarHeight =
            3.5

        buildOnePiling ( posX, posZ ) =
            let
                x1 =
                    posX - pilingHalfSize

                x2 =
                    posX + pilingHalfSize

                z1 =
                    posZ - pilingHalfSize

                z2 =
                    posZ + pilingHalfSize

                -- Wooden post body
                post =
                    box x1 x2 pilingTopY pilingBottomY z1 z2 pilingTimber

                -- Dark cast-iron collar ring around post head
                collar =
                    box (x1 - 0.4) (x2 + 0.4) pilingTopY (pilingTopY + collarHeight) (z1 - 0.4) (z2 + 0.4) ironFitting
            in
            List.concat [ post, collar ]

        pilingPositions =
            [ ( 49.0, -140.0 )
            , ( 49.0, -60.0 )
            , ( 49.0, 20.0 )
            , ( 49.0, 100.0 )
            , ( -35.0, 128.0 )
            , ( -95.0, 128.0 )
            ]
    in
    List.concatMap buildOnePiling pilingPositions


buildDockTriangles : List ( Vertex, Vertex, Vertex )
buildDockTriangles =
    List.concat
        [ buildPierPlanks
        , buildDockStructure
        , buildMooringPilings
        ]


dockMesh : Mesh Vertex
dockMesh =
    WebGL.triangles buildDockTriangles



-- ── Harbor Water & Hull Contact Shadow ───────────────────────────────────────


buildWaterTriangles : List ( Vertex, Vertex, Vertex )
buildWaterTriangles =
    let
        -- Main translucent harbor sea surface plane
        mainWater =
            horizontalQuad waterLevelY -380.0 380.0 -380.0 380.0 waterColor

        -- Calmer shallow slipway water near the wooden berth
        slipWater =
            horizontalQuad (waterLevelY - 0.05) -150.0 42.0 -185.0 122.0 waterShallowColor
    in
    List.concat [ mainWater, slipWater ]


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


{-| WebGL entities for the 3D dock harbor environment:

  - Wooden pier, weathered planks, bumper rail, and mooring pilings.
  - Translucent harbor sea water surface plane.
  - Soft aquatic hull shadow directly under the ship.

-}
entities : Float -> Camera -> Float -> List Entity
entities baselineY camera aspect =
    let
        modelMatrix =
            Mat4.makeTranslate (vec3 0.0 baselineY 0.0)

        uniforms =
            { viewMatrix = Camera.viewMatrix camera
            , projectionMatrix = Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)
            , modelMatrix = modelMatrix
            }

        opaqueSettings =
            [ DepthTest.lessOrEqual { write = True, near = 0, far = 1 }
            ]

        translucentSettings =
            [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
            , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            ]

        dockEntity =
            WebGL.entityWith opaqueSettings vertexShader fragmentShader dockMesh uniforms

        waterEntity =
            WebGL.entityWith translucentSettings vertexShader fragmentShader waterMesh uniforms
    in
    [ dockEntity
    , waterEntity
    ]
