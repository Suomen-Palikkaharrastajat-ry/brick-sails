module Sails.OceanShader exposing
    ( Ctx
    , PropVertex
    , makeCtx
    , oceanEntity
    , propEntity
    , propView
    , skyEntity
    , sunDirection
    , withCourse
    , withMap
    , withWeather
    )

{-| WebGL shaders for Brick Sails: a tropical sea of turquoise studded plates
on the swell of `Sails.Physics.seaElevation` and the brick sky (both the shared brick look
of `Game.BrickLook`), the camera, sun and fog the brick-built props are drawn
with (`propView`), and one flat-shaded shader for the course markers and the
island scenery. The weather (`withWeather`) raises the sea, darkens the sea,
sky and light under storm clouds, pulls the fog in and lights everything up
in a lightning flash. The course (`withCourse`) sets the map's sea and sky
colours (`Sails.World.SeaPalette`), where the sea is centred and the
islands the lagoon and the surf ring; `withMap` sets only the colours (the
shipyard preview).
-}

import Game.BrickLook as BrickLook
import Game.BrickProps as BrickProps
import Game.MapParams exposing (MapParams)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Sails.Types exposing (Course, Island)
import Sails.World as World exposing (SeaPalette)
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
    , fog : Float
    , palette : SeaPalette
    , centre : ( Float, Float )
    , isles : List Vec4
    }


{-| Weather for the shaders: `sea` state (1 normal, see
`Sails.Physics.seaElevation`), `gloom` 0..1 (storm clouds: darker sky, sea
and light, closer fog), lightning `flash` 0..1 and how far the `fog` reaches
(1 normal).
-}
withWeather : { a | sea : Float, gloom : Float, flash : Float, fog : Float } -> Ctx -> Ctx
withWeather w ctx =
    { ctx | sea = w.sea, gloom = clamp 0 1 w.gloom, flash = clamp 0 1 w.flash, fog = clamp 0.2 1.5 w.fog }


{-| The course for the shaders: the sea and sky colours of its map, the sea
centred on its arena, and its largest islands (at most `maxIsles`) for the
lagoon colour and the surf round their shores.
-}
withCourse : Course -> Ctx -> Ctx
withCourse course ctx =
    let
        coloured =
            withMap course.map ctx
    in
    { coloured
        | centre = ( course.arena.x, course.arena.z )
        , isles =
            course.islands
                |> List.sortBy (\i -> negate i.radius)
                |> List.take maxIsles
                |> List.map islandVec
    }


{-| The sea and sky colours of a map (`Sails.World.seaPalette`), with
nothing else of its course: the shipyard preview's open water.
-}
withMap : MapParams -> Ctx -> Ctx
withMap map ctx =
    { ctx | palette = World.seaPalette map }


islandVec : Island -> Vec4
islandVec isl =
    vec4 isl.x isl.z isl.radius 0


{-| How many islands the sea shader knows about.
-}
maxIsles : Int
maxIsles =
    12


{-| An island far off the sea, for the unused island slots.
-}
noIsle : Vec4
noIsle =
    vec4 0 0 -10000 0


isleAt : Int -> List Vec4 -> Vec4
isleAt i isles =
    List.drop i isles |> List.head |> Maybe.withDefault noIsle


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
    , fog = 1
    , palette = World.seaPalette { biome = Game.MapParams.Desert, season = Game.MapParams.Summer, weather = Game.MapParams.Clear, seed = 1 }
    , centre = ( 0, 95 )
    , isles = []
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
    , u_sea : Float
    , u_deep : Vec3
    , u_shallow : Vec3
    , u_cap : Vec3
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
    , u_centre : Vec3
    , u_lagoon : Vec3
    , u_isl0 : Vec4
    , u_isl1 : Vec4
    , u_isl2 : Vec4
    , u_isl3 : Vec4
    , u_isl4 : Vec4
    , u_isl5 : Vec4
    , u_isl6 : Vec4
    , u_isl7 : Vec4
    , u_isl8 : Vec4
    , u_isl9 : Vec4
    , u_isl10 : Vec4
    , u_isl11 : Vec4
    }


{-| The low grey ceiling of storm clouds.
-}
stormColour : Vec3
stormColour =
    vec3 0.34 0.38 0.44


mixV : Vec3 -> Vec3 -> Float -> Vec3
mixV a b f =
    Vec3.add (Vec3.scale (1 - f) a) (Vec3.scale f b)


{-| The sky colour at the horizon (and of the fog) in this weather: grey
under storm clouds, white in a lightning flash.
-}
horizonColour : Ctx -> Vec3
horizonColour ctx =
    Vec3.add (mixV ctx.palette.horizon stormColour ctx.gloom) (Vec3.scale (0.5 * ctx.flash) (vec3 1 1 1))


{-| Fog of the sea and the props: none near the boats, all sky colour at the
edge of the world. Storm clouds pull it in, and so does a foggy map (`fog`).
-}
fogNear : Ctx -> Float
fogNear ctx =
    (140 - 80 * ctx.gloom) * ctx.fog


fogFar : Ctx -> Float
fogFar ctx =
    (520 - 240 * ctx.gloom) * ctx.fog


{-| How much a lightning flash brightens what it lights.
-}
flashLight : Ctx -> Vec3 -> Vec3
flashLight ctx colour =
    Vec3.add colour (Vec3.scale (0.6 * ctx.flash) (vec3 0.55 0.6 0.7))


{-| 64 x 64 grid of 12 m cells, centred on the origin; the shader moves it
to the course's arena (`u_centre`).
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
            { position = vec3 (toFloat i * step - half) 0 (toFloat j * step - half) }

        quad i j =
            [ ( v i j, v i (j + 1), v (i + 1) j ), ( v (i + 1) j, v i (j + 1), v (i + 1) (j + 1) ) ]
    in
    List.range 0 (n - 1)
        |> List.concatMap (\i -> List.range 0 (n - 1) |> List.concatMap (quad i))
        |> WebGL.triangles


{-| The sea: tropical studded plates (`Game.BrickLook.surfaceFragmentShader`),
dark azure in the troughs and light turquoise on the crests, with white caps
on the highest crests, rising and falling with the swell. Under storm clouds the
plates are a dull grey-blue and the sea loses its sheen.
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
        , u_sea = ctx.sea
        , u_deep = flashLight ctx (mixV ctx.palette.deep (vec3 0.05 0.16 0.24) ctx.gloom)
        , u_shallow = flashLight ctx (mixV ctx.palette.shallow (vec3 0.16 0.3 0.38) ctx.gloom)
        , u_cap = flashLight ctx (mixV ctx.palette.cap (vec3 0.7 0.76 0.8) ctx.gloom)
        , u_eyePos = ctx.eye
        , u_sunDir = sunDir
        , u_fogColor = horizonColour ctx
        , u_fogNear = fogNear ctx
        , u_fogFar = fogFar ctx
        , u_fogMax = 1
        , u_skyAmbient = horizonColour ctx
        , u_groundAmbient = mixV (vec3 0.32 0.44 0.46) (vec3 0.15 0.18 0.22) ctx.gloom
        , u_studPitch = 0.35
        , u_plateStuds = 16
        , u_gloss = 0.7 * (1 - 0.6 * ctx.gloom)
        , u_centre = vec3 (Tuple.first ctx.centre) 0 (Tuple.second ctx.centre)
        , u_lagoon = mixV ctx.palette.lagoon (vec3 0.16 0.3 0.38) ctx.gloom
        , u_isl0 = isleAt 0 ctx.isles
        , u_isl1 = isleAt 1 ctx.isles
        , u_isl2 = isleAt 2 ctx.isles
        , u_isl3 = isleAt 3 ctx.isles
        , u_isl4 = isleAt 4 ctx.isles
        , u_isl5 = isleAt 5 ctx.isles
        , u_isl6 = isleAt 6 ctx.isles
        , u_isl7 = isleAt 7 ctx.isles
        , u_isl8 = isleAt 8 ctx.isles
        , u_isl9 = isleAt 9 ctx.isles
        , u_isl10 = isleAt 10 ctx.isles
        , u_isl11 = isleAt 11 ctx.isles
        }


waterVertexShader : WebGL.Shader WaterVertex WaterUniforms BrickLook.SurfaceVaryings
waterVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        uniform mat4 u_view;
        uniform mat4 u_perspective;
        uniform float u_time;
        uniform float u_sea;
        uniform vec3 u_deep;
        uniform vec3 u_shallow;
        uniform vec3 u_cap;
        uniform vec3 u_centre;
        uniform vec3 u_lagoon;
        uniform vec4 u_isl0;
        uniform vec4 u_isl1;
        uniform vec4 u_isl2;
        uniform vec4 u_isl3;
        uniform vec4 u_isl4;
        uniform vec4 u_isl5;
        uniform vec4 u_isl6;
        uniform vec4 u_isl7;
        uniform vec4 u_isl8;
        uniform vec4 u_isl9;
        uniform vec4 u_isl10;
        uniform vec4 u_isl11;
        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;
        varying float v_studs;

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

        float isle(vec2 p, vec4 i) {
            return length(p - i.xy) - i.z;
        }

        float distToIslands(vec2 p) {
            float d = isle(p, u_isl0);
            d = min(d, isle(p, u_isl1));
            d = min(d, isle(p, u_isl2));
            d = min(d, isle(p, u_isl3));
            d = min(d, isle(p, u_isl4));
            d = min(d, isle(p, u_isl5));
            d = min(d, isle(p, u_isl6));
            d = min(d, isle(p, u_isl7));
            d = min(d, isle(p, u_isl8));
            d = min(d, isle(p, u_isl9));
            d = min(d, isle(p, u_isl10));
            d = min(d, isle(p, u_isl11));
            return d;
        }

        void main () {
            vec2 q = position.xz + u_centre.xz;
            float h = seaHeight(q, u_time);

            // Slope of the two swells and the storm swell, scaled by the wave
            // set, for the normal (the set envelope itself is long and flat).
            float k1 = 6.28318 / 36.0;
            float k2 = 6.28318 / 22.0;
            float k0 = 6.28318 / 64.0;
            float kg = 6.28318 / 150.0;
            float a1 = k1 * (q.y * 0.9 + q.x * 0.4) - k1 * 3.0 * u_time;
            float a2 = k2 * (q.x * 0.8 - q.y * 0.5) - k2 * 2.4 * u_time;
            float a0 = k0 * (q.x * 0.3 + q.y * 0.95) - k0 * 4.2 * u_time;
            float s = u_sea * (1.0 + 0.5 * pow(max(0.0, sin(kg * (q.y * 0.9 + q.x * 0.4) - kg * 1.5 * u_time)), 4.0));
            float storm = 0.45 * max(0.0, u_sea - 1.0);
            float dx = s * (0.22 * k1 * 0.4 * cos(a1) + 0.10 * k2 * 0.8 * cos(a2)) + storm * k0 * 0.3 * cos(a0);
            float dz = s * (0.22 * k1 * 0.9 * cos(a1) - 0.10 * k2 * 0.5 * cos(a2)) + storm * k0 * 0.95 * cos(a0);

            // Height relative to the sea state, for the colour and the caps.
            float hn = h / max(0.6, s + storm * 3.0);
            vec3 col = mix(u_deep, u_shallow, smoothstep(-0.25, 0.3, hn));

            // Shallow turquoise lagoon around islands.
            float dShore = distToIslands(q);
            float lagoon = 1.0 - smoothstep(0.0, 20.0, max(0.0, dShore));
            vec3 lagoonColor = mix(u_shallow, u_lagoon, 0.75);
            col = mix(col, lagoonColor, lagoon * 0.8);

            // White caps: a little lower down the waves as the sea gets up.
            float capLine = 0.31 - 0.05 * clamp(u_sea - 1.0, 0.0, 1.5);
            float crest = smoothstep(capLine - 0.04, capLine + 0.04, hn);

            // Shoreline beach surf foam lapping at the sand.
            float surfWave = sin(u_time * 2.6 - dShore * 1.4);
            float surfFoam = smoothstep(0.45, 0.9, surfWave) * (1.0 - smoothstep(0.0, 5.0, max(0.0, dShore)));
            float foam = clamp(crest * 0.35 + surfFoam * 0.8, 0.0, 1.0);

            vec3 p = vec3(q.x, h, q.y);
            v_worldPos = p;
            v_normal = normalize(vec3(-dx, 1.0, -dz));
            v_color = mix(col, u_cap, foam);
            v_studs = 1.0;
            gl_Position = u_perspective * u_view * vec4(p, 1.0);
        }
    |]



-- SKY


{-| The shared brick sky: a sun disc of yellow bricks and clouds of white
bricks drifting along the horizon, under a sky that greys over in a storm.
-}
skyEntity : Ctx -> WebGL.Entity
skyEntity ctx =
    BrickLook.skyEntity
        { viewProjection = Mat4.mul ctx.perspective ctx.view
        , eye = ctx.eye
        , zenith = Vec3.add (mixV ctx.palette.zenith (vec3 0.2 0.23 0.28) ctx.gloom) (Vec3.scale (0.75 * ctx.flash) (vec3 1 1 1))
        , horizon = horizonColour ctx
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
    , fogColor = horizonColour ctx
    , fogNear = fogNear ctx
    , fogFar = fogFar ctx
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
    , u_gloom : Float
    , u_flash : Float
    , u_fogColor : Vec3
    , u_fogNear : Float
    , u_fogFar : Float
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
        , u_fogColor = horizonColour ctx
        , u_fogNear = fogNear ctx
        , u_fogFar = fogFar ctx
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
        uniform vec3 u_fogColor;
        uniform float u_fogNear;
        uniform float u_fogFar;
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
            // The same fog as the sea and the brick-built props.
            float fog = smoothstep(u_fogNear, u_fogFar, dist);
            gl_FragColor = vec4(mix(col, u_fogColor, fog), 1.0);
        }
    |]
