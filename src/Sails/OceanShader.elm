module Sails.OceanShader exposing
    ( Ctx
    , PropVertex
    , makeCtx
    , oceanEntity
    , propEntity
    , skyEntity
    , sunDirection
    , withWeather
    )

{-| WebGL shaders for Brick Sails: a bright toy-like sea with a gentle swell,
the sky dome, and one flat-shaded shader for every prop (boats, buoys,
islands, targets, cannonballs and splashes).
-}

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
    , sea : Float
    , gloom : Float
    , flash : Float
    }


{-| Weather for the shaders: `sea` state (1 normal, see
`Sails.Physics.seaElevation`), `gloom` 0..1 (storm clouds: darker sky, sea
and light, closer fog) and lightning `flash` 0..1.
-}
withWeather : { a | sea : Float, gloom : Float, flash : Float } -> Ctx -> Ctx
withWeather w ctx =
    { ctx | sea = w.sea, gloom = clamp 0 1 w.gloom, flash = clamp 0 1 w.flash }


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
    , sea = 1
    , gloom = 0
    , flash = 0
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
    , u_eyePos : Vec3
    , u_sunDir : Vec3
    , u_time : Float
    , u_sea : Float
    , u_gloom : Float
    , u_flash : Float
    }


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


oceanEntity : Ctx -> WebGL.Entity
oceanEntity ctx =
    WebGL.entityWith [ DepthTest.default ]
        waterVertexShader
        waterFragmentShader
        waterMesh
        { u_view = ctx.view
        , u_perspective = ctx.perspective
        , u_eyePos = ctx.eye
        , u_sunDir = sunDir
        , u_time = ctx.time
        , u_sea = ctx.sea
        , u_gloom = ctx.gloom
        , u_flash = ctx.flash
        }


waterVertexShader : WebGL.Shader WaterVertex WaterUniforms { v_worldPos : Vec3 }
waterVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        uniform mat4 u_view;
        uniform mat4 u_perspective;
        uniform float u_time;
        varying vec3 v_worldPos;

        uniform float u_sea;

        // Same sea as Sails.Physics.seaElevation.
        float seaHeight(vec2 p, float t) {
            float k1 = 6.28318 / 36.0;
            float k2 = 6.28318 / 22.0;
            float k0 = 6.28318 / 64.0;
            float kg = 6.28318 / 150.0;
            float grp = 1.0 + 0.5 * pow(max(0.0, sin(kg * (p.y * 0.9 + p.x * 0.4) - kg * 1.5 * t)), 4.0);
            return u_sea * grp * (0.22 * sin(k1 * (p.y * 0.9 + p.x * 0.4) - k1 * 3.0 * t)
                                + 0.10 * sin(k2 * (p.x * 0.8 - p.y * 0.5) - k2 * 2.4 * t))
                 + 0.45 * max(0.0, u_sea - 1.0) * sin(k0 * (p.x * 0.3 + p.y * 0.95) - k0 * 4.2 * t);
        }

        void main () {
            float h = seaHeight(position.xz, u_time);
            vec3 p = vec3(position.x, h, position.z);
            v_worldPos = p;
            gl_Position = u_perspective * u_view * vec4(p, 1.0);
        }
    |]


waterFragmentShader : WebGL.Shader {} WaterUniforms { v_worldPos : Vec3 }
waterFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 u_eyePos;
        uniform vec3 u_sunDir;
        uniform float u_time;
        uniform float u_sea;
        uniform float u_gloom;
        uniform float u_flash;
        varying vec3 v_worldPos;

        void main () {
            vec2 p = v_worldPos.xz;
            float k1 = 6.28318 / 36.0;
            float k2 = 6.28318 / 22.0;
            float k3 = 6.28318 / 5.0;
            float k0 = 6.28318 / 64.0;
            float kg = 6.28318 / 150.0;
            float a1 = k1 * (p.y * 0.9 + p.x * 0.4) - k1 * 3.0 * u_time;
            float a2 = k2 * (p.x * 0.8 - p.y * 0.5) - k2 * 2.4 * u_time;
            float a3 = k3 * (p.x * 0.6 + p.y * 0.8) - k3 * 1.5 * u_time;
            float k4 = 6.28318 / 8.3;
            float a4 = k4 * (p.x * 0.95 - p.y * 0.3) - k4 * 1.9 * u_time;
            float a0 = k0 * (p.x * 0.3 + p.y * 0.95) - k0 * 4.2 * u_time;
            // Wave-set envelope and storm swell, as in Sails.Physics.seaElevation.
            float s = u_sea * (1.0 + 0.5 * pow(max(0.0, sin(kg * (p.y * 0.9 + p.x * 0.4) - kg * 1.5 * u_time)), 4.0));
            float storm = 0.45 * max(0.0, u_sea - 1.0);
            // Two swells plus two small ripples at unrelated angles and lengths
            // (crossed equal ripples would paint a visible checkerboard).
            float ripple = 1.0 + 1.5 * max(0.0, u_sea - 1.0);
            float dx = s * (0.22 * k1 * 0.4 * cos(a1) + 0.10 * k2 * 0.8 * cos(a2))
                     + ripple * (0.015 * k3 * 0.6 * cos(a3) + 0.025 * k4 * 0.95 * cos(a4))
                     + storm * k0 * 0.3 * cos(a0);
            float dz = s * (0.22 * k1 * 0.9 * cos(a1) - 0.10 * k2 * 0.5 * cos(a2))
                     + ripple * (0.015 * k3 * 0.8 * cos(a3) - 0.025 * k4 * 0.3 * cos(a4))
                     + storm * k0 * 0.95 * cos(a0);
            vec3 N = normalize(vec3(-dx, 1.0, -dz));
            // Height from the same formula as the vertex shader, per pixel, so the
            // colour does not show the 12 m grid.
            float h = s * (0.22 * sin(a1) + 0.10 * sin(a2)) + storm * sin(a0);
            float hn = h / max(0.6, s + storm * 3.0);
            vec3 V = normalize(u_eyePos - v_worldPos);
            vec3 L = normalize(u_sunDir);

            vec3 deep = mix(vec3(0.02, 0.33, 0.62), vec3(0.05, 0.16, 0.24), u_gloom);
            vec3 light = mix(vec3(0.07, 0.52, 0.8), vec3(0.16, 0.3, 0.38), u_gloom);
            vec3 col = mix(deep, light, smoothstep(-0.25, 0.3, hn));

            float fresnel = 0.02 + 0.35 * pow(1.0 - max(0.0, dot(N, V)), 4.0);
            vec3 sky = mix(vec3(0.75, 0.88, 0.97), vec3(0.34, 0.38, 0.44), u_gloom);
            col = mix(col, sky, fresnel);

            vec3 H = normalize(L + V);
            col += vec3(pow(max(0.0, dot(N, H)), 200.0) * 0.5 * (1.0 - u_gloom));

            // White caps: more and lower down the waves as the sea gets up.
            float capLine = 0.31 - 0.12 * clamp(u_sea - 1.0, 0.0, 1.5);
            float crest = smoothstep(capLine - 0.03, capLine + 0.02, hn + 0.03 * sin(a3 * 2.0));
            col = mix(col, vec3(0.9, 0.97, 1.0), crest * (0.35 + 0.25 * clamp(u_sea - 1.0, 0.0, 1.0)));

            col += vec3(0.55, 0.6, 0.7) * u_flash;

            float dist = length(v_worldPos - u_eyePos);
            float fogStart = 160.0 - 90.0 * u_gloom;
            float fog = clamp((dist - fogStart) / (320.0 - 120.0 * u_gloom), 0.0, 1.0);
            gl_FragColor = vec4(mix(col, sky + vec3(0.5) * u_flash, fog), 1.0);
        }
    |]



-- SKY


type alias SkyUniforms =
    { u_view : Mat4
    , u_perspective : Mat4
    , u_eyePos : Vec3
    , u_sunDir : Vec3
    , u_gloom : Float
    , u_flash : Float
    }


skyEntity : Ctx -> WebGL.Entity
skyEntity ctx =
    WebGL.entityWith [ DepthTest.always { write = False, near = 0, far = 1 } ]
        skyVertexShader
        skyFragmentShader
        skyMesh
        { u_view = ctx.view
        , u_perspective = ctx.perspective
        , u_eyePos = ctx.eye
        , u_sunDir = sunDir
        , u_gloom = ctx.gloom
        , u_flash = ctx.flash
        }


skyMesh : WebGL.Mesh { position : Vec3 }
skyMesh =
    let
        r =
            600.0

        rings =
            8

        segs =
            16

        v i j =
            let
                phi =
                    (toFloat i / toFloat rings) * (pi / 2.0) - 0.2

                theta =
                    (toFloat j / toFloat segs) * (2.0 * pi)
            in
            { position = vec3 (r * cos phi * sin theta) (r * sin phi) (r * cos phi * cos theta) }

        quad i j =
            [ ( v i j, v i (j + 1), v (i + 1) j ), ( v (i + 1) j, v i (j + 1), v (i + 1) (j + 1) ) ]
    in
    List.range 0 (rings - 1)
        |> List.concatMap (\i -> List.range 0 (segs - 1) |> List.concatMap (quad i))
        |> WebGL.triangles


skyVertexShader : WebGL.Shader { position : Vec3 } SkyUniforms { v_dir : Vec3 }
skyVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        uniform mat4 u_view;
        uniform mat4 u_perspective;
        uniform vec3 u_eyePos;
        varying vec3 v_dir;

        void main () {
            v_dir = position;
            gl_Position = (u_perspective * u_view * vec4(position + u_eyePos, 1.0)).xyww;
        }
    |]


skyFragmentShader : WebGL.Shader {} SkyUniforms { v_dir : Vec3 }
skyFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 u_sunDir;
        uniform float u_gloom;
        uniform float u_flash;
        varying vec3 v_dir;

        void main () {
            vec3 dir = normalize(v_dir);
            float y = max(0.0, dir.y);
            vec3 sky = mix(vec3(0.75, 0.88, 0.97), vec3(0.2, 0.52, 0.9), pow(y, 0.6));
            // Storm clouds: a low grey ceiling with darker rolls in it.
            float rolls = 0.5 + 0.5 * sin(dir.x * 9.0 + dir.z * 5.0) * sin(dir.z * 7.0 - dir.x * 3.0);
            vec3 cloud = mix(vec3(0.34, 0.38, 0.44), vec3(0.2, 0.23, 0.28), rolls * pow(y, 0.4));
            sky = mix(sky, cloud, u_gloom);
            float s = max(0.0, dot(dir, normalize(u_sunDir)));
            sky += vec3(pow(s, 200.0) * 0.8) * (1.0 - u_gloom);
            gl_FragColor = vec4(sky + vec3(0.75, 0.8, 0.95) * u_flash, 1.0);
        }
    |]



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
    , u_gloom : Float
    , u_flash : Float
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
        , u_gloom = ctx.gloom
        , u_flash = ctx.flash
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
        uniform float u_gloom;
        uniform float u_flash;
        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;

        void main () {
            vec3 V = normalize(u_eyePos - v_worldPos);
            vec3 N = normalize(v_normal);
            // Light whichever side faces the viewer, so mesh winding never matters.
            if (dot(N, V) < 0.0) { N = -N; }
            vec3 L = normalize(u_sunDir);
            float diff = max(0.0, dot(N, L)) * (1.0 - 0.7 * u_gloom);
            vec3 H = normalize(L + V);
            float spec = pow(max(0.0, dot(N, H)), 32.0) * 0.25 * (1.0 - u_gloom);
            vec3 col = v_color * (0.55 - 0.15 * u_gloom + 0.55 * diff + 0.6 * u_flash) + vec3(spec);
            float dist = length(v_worldPos - u_eyePos);
            float fogStart = 160.0 - 90.0 * u_gloom;
            float fog = clamp((dist - fogStart) / (320.0 - 120.0 * u_gloom), 0.0, 1.0);
            vec3 sky = mix(vec3(0.75, 0.88, 0.97), vec3(0.34, 0.38, 0.44), u_gloom);
            gl_FragColor = vec4(mix(col, sky, fog), 1.0);
        }
    |]
