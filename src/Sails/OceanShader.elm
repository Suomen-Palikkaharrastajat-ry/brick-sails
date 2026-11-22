module Sails.OceanShader exposing
    ( Ctx
    , PropVertex
    , makeCtx
    , oceanEntity
    , propEntity
    , propView
    , skyEntity
    , sunDirection
    )

{-| WebGL shaders for Brick Sails: a sea of blue studded plates on a gentle
swell and the brick sky (both the shared brick look of `Game.BrickLook`),
the camera, sun and fog the brick-built props are drawn with (`propView`),
and one flat-shaded shader for the course markers (the ring round the next
buoy and the guide arrow).
-}

import Game.BrickLook as BrickLook
import Game.BrickProps as BrickProps
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL
import WebGL.Settings.DepthTest as DepthTest


{-| Per-frame camera context shared by all entities.
-}
type alias Ctx =
    { view : Mat4
    , perspective : Mat4
    , eye : Vec3
    , time : Float
    }


fovY : Float
fovY =
    45.0


makeCtx : { eye : Vec3, target : Vec3 } -> Float -> Int -> Int -> Ctx
makeCtx cam time width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)
    in
    { view = Mat4.makeLookAt cam.eye cam.target (vec3 0 1 0)
    , perspective = Mat4.makePerspective fovY aspect 0.5 1500
    , eye = cam.eye
    , time = time
    }


{-| Direction towards the sun, shared by the sea, the props and the boats.
-}
sunDirection : Vec3
sunDirection =
    sunDir


sunDir : Vec3
sunDir =
    Vec3.normalize (vec3 0.45 0.85 0.35)



-- OCEAN


type alias WaterVertex =
    { position : Vec3 }


type alias WaterUniforms =
    { u_view : Mat4
    , u_perspective : Mat4
    , u_time : Float
    , u_deep : Vec3
    , u_shallow : Vec3
    , u_eyePos : Vec3
    , u_sunDir : Vec3
    , u_fogColor : Vec3
    , u_fogNear : Float
    , u_fogFar : Float
    , u_fogMax : Float
    , u_skyAmbient : Vec3
    , u_groundAmbient : Vec3
    , u_studPitch : Float
    , u_plateStuds : Float
    , u_gloss : Float
    }


skyColour : Vec3
skyColour =
    vec3 0.75 0.88 0.97


{-| Fog of the sea and the props: none near the boats, all sky colour at the
edge of the world.
-}
fogNear : Float
fogNear =
    140


fogFar : Float
fogFar =
    520


{-| 64 x 64 grid of 12 m cells around the play area.
-}
waterMesh : WebGL.Mesh WaterVertex
waterMesh =
    let
        n =
            64

        step =
            12.0

        half =
            toFloat n * step / 2

        v i j =
            { position = vec3 (toFloat i * step - half) 0 (toFloat j * step - half + 95) }

        quad i j =
            [ ( v i j, v i (j + 1), v (i + 1) j ), ( v (i + 1) j, v i (j + 1), v (i + 1) (j + 1) ) ]
    in
    List.range 0 (n - 1)
        |> List.concatMap (\i -> List.range 0 (n - 1) |> List.concatMap (quad i))
        |> WebGL.triangles


{-| The sea: blue studded plates (`Game.BrickLook.surfaceFragmentShader`),
dark blue in the troughs and dark azure on the crests, rising and falling
with the swell.
-}
oceanEntity : Ctx -> WebGL.Entity
oceanEntity ctx =
    WebGL.entityWith [ DepthTest.default ]
        waterVertexShader
        BrickLook.surfaceFragmentShader
        waterMesh
        { u_view = ctx.view
        , u_perspective = ctx.perspective
        , u_time = ctx.time
        , u_deep = BrickLook.colour 1
        , u_shallow = BrickLook.colour 321
        , u_eyePos = ctx.eye
        , u_sunDir = sunDir
        , u_fogColor = skyColour
        , u_fogNear = fogNear
        , u_fogFar = fogFar
        , u_fogMax = 1
        , u_skyAmbient = skyColour
        , u_groundAmbient = vec3 0.3 0.4 0.5
        , u_studPitch = 0.35
        , u_plateStuds = 16
        , u_gloss = 0.7
        }


waterVertexShader : WebGL.Shader WaterVertex WaterUniforms BrickLook.SurfaceVaryings
waterVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        uniform mat4 u_view;
        uniform mat4 u_perspective;
        uniform float u_time;
        uniform vec3 u_deep;
        uniform vec3 u_shallow;
        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;
        varying float v_studs;

        void main () {
            float k1 = 6.28318 / 36.0;
            float k2 = 6.28318 / 22.0;
            float a1 = k1 * (position.z * 0.9 + position.x * 0.4) - k1 * 3.0 * u_time;
            float a2 = k2 * (position.x * 0.8 - position.z * 0.5) - k2 * 2.4 * u_time;
            float h = 0.22 * sin(a1) + 0.10 * sin(a2);
            float dx = 0.22 * k1 * 0.4 * cos(a1) + 0.10 * k2 * 0.8 * cos(a2);
            float dz = 0.22 * k1 * 0.9 * cos(a1) - 0.10 * k2 * 0.5 * cos(a2);
            vec3 p = vec3(position.x, h, position.z);
            v_worldPos = p;
            v_normal = normalize(vec3(-dx, 1.0, -dz));
            v_color = mix(u_deep, u_shallow, smoothstep(-0.25, 0.3, h));
            v_studs = 1.0;
            gl_Position = u_perspective * u_view * vec4(p, 1.0);
        }
    |]



-- SKY


{-| The shared brick sky: a sun disc of yellow bricks and clouds of white
bricks drifting along the horizon.
-}
skyEntity : Ctx -> WebGL.Entity
skyEntity ctx =
    BrickLook.skyEntity
        { viewProjection = Mat4.mul ctx.perspective ctx.view
        , eye = ctx.eye
        , zenith = vec3 0.2 0.52 0.9
        , horizon = skyColour
        , sunDir = sunDir
        , time = ctx.time
        }


{-| The camera, sun and fog for the brick-built props (`Game.BrickProps`).
-}
propView : Ctx -> Int -> Int -> BrickProps.View
propView ctx width height =
    { view = ctx.view
    , projection = ctx.perspective
    , eye = ctx.eye
    , sunDir = sunDir
    , fogColor = skyColour
    , fogNear = fogNear
    , fogFar = fogFar
    , fogMax = 1
    , width = max 1 width
    , height = max 1 height
    }



-- PROPS


{-| Prop vertex: position and normal in model space, flat colour in `color`.
-}
type alias PropVertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    }


type alias PropUniforms =
    { u_model : Mat4
    , u_view : Mat4
    , u_perspective : Mat4
    , u_eyePos : Vec3
    , u_sunDir : Vec3
    , u_tint : Vec3
    }


{-| Draw a prop mesh with a model matrix. Vertex colours are multiplied by
`tint`; meshes painted white take the tint as their colour.
-}
propEntity : Ctx -> Mat4 -> Vec3 -> WebGL.Mesh PropVertex -> WebGL.Entity
propEntity ctx model tint mesh =
    WebGL.entityWith [ DepthTest.default ]
        propVertexShader
        propFragmentShader
        mesh
        { u_model = model
        , u_view = ctx.view
        , u_perspective = ctx.perspective
        , u_eyePos = ctx.eye
        , u_sunDir = sunDir
        , u_tint = tint
        }


propVertexShader : WebGL.Shader PropVertex PropUniforms { v_worldPos : Vec3, v_normal : Vec3, v_color : Vec3 }
propVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        uniform mat4 u_model;
        uniform mat4 u_view;
        uniform mat4 u_perspective;
        uniform vec3 u_tint;
        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;

        void main () {
            vec4 world = u_model * vec4(position, 1.0);
            v_worldPos = world.xyz;
            v_normal = (u_model * vec4(normal, 0.0)).xyz;
            v_color = color * u_tint;
            gl_Position = u_perspective * u_view * world;
        }
    |]


propFragmentShader : WebGL.Shader {} PropUniforms { v_worldPos : Vec3, v_normal : Vec3, v_color : Vec3 }
propFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 u_eyePos;
        uniform vec3 u_sunDir;
        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;

        void main () {
            vec3 V = normalize(u_eyePos - v_worldPos);
            vec3 N = normalize(v_normal);
            // Light whichever side faces the viewer, so mesh winding never matters.
            if (dot(N, V) < 0.0) { N = -N; }
            vec3 L = normalize(u_sunDir);
            float diff = max(0.0, dot(N, L));
            vec3 H = normalize(L + V);
            float spec = pow(max(0.0, dot(N, H)), 32.0) * 0.25;
            vec3 col = v_color * (0.55 + 0.55 * diff) + vec3(spec);
            float dist = length(v_worldPos - u_eyePos);
            float fog = smoothstep(140.0, 520.0, dist);
            gl_FragColor = vec4(mix(col, vec3(0.75, 0.88, 0.97), fog), 1.0);
        }
    |]
