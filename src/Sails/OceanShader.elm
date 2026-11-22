module Sails.OceanShader exposing
    ( OceanVertex
    , buildBuoyMesh
    , buildCombatMesh
    , buildIslandMesh
    , buildWaterMesh
    , buoyEntity
    , combatEntity
    , islandEntity
    , oceanEntity
    , skyEntity
    )

{-| WebGL shaders and procedural meshes for Brick Sails:
dynamic Gerstner wave water displacement, foam caps, specular sun glint,
sky dome, archipelago islands, naval combat projectiles, and IALA navigation buoys.
-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera exposing (Camera)
import Sails.Types exposing (BuoyKind(..), Cannonball, CombatImpact, Waypoint)
import WebGL
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


type alias OceanVertex =
    { position : Vec3
    , normal : Vec3
    , uv : Vec3
    }


type alias Uniforms =
    { u_view : Mat4
    , u_perspective : Mat4
    , u_eyePos : Vec3
    , u_sunDir : Vec3
    , u_time : Float
    }


type alias SkyUniforms =
    { u_view : Mat4
    , u_perspective : Mat4
    , u_eyePos : Vec3
    , u_zenith : Vec3
    , u_horizon : Vec3
    , u_sunDir : Vec3
    }


{-| Build a large tessellated water grid centered on the origin.
-}
buildWaterMesh : WebGL.Mesh OceanVertex
buildWaterMesh =
    let
        gridSize =
            32

        step =
            30.0 -- 32 * 30 = 960m x 960m ocean area

        half =
            (toFloat gridSize * step) / 2.0

        makeQuad i j =
            let
                x0 =
                    toFloat i * step - half

                x1 =
                    toFloat (i + 1) * step - half

                z0 =
                    toFloat j * step - half

                z1 =
                    toFloat (j + 1) * step - half

                up =
                    vec3 0.0 1.0 0.0

                v00 =
                    OceanVertex (vec3 x0 0.0 z0) up (vec3 (x0 / 40.0) (z0 / 40.0) 0.0)

                v10 =
                    OceanVertex (vec3 x1 0.0 z0) up (vec3 (x1 / 40.0) (z0 / 40.0) 0.0)

                v01 =
                    OceanVertex (vec3 x0 0.0 z1) up (vec3 (x0 / 40.0) (z1 / 40.0) 0.0)

                v11 =
                    OceanVertex (vec3 x1 0.0 z1) up (vec3 (x1 / 40.0) (z1 / 40.0) 0.0)
            in
            [ ( v00, v10, v01 ), ( v10, v11, v01 ) ]

        allTriangles =
            List.range 0 (gridSize - 1)
                |> List.concatMap
                    (\i ->
                        List.range 0 (gridSize - 1)
                            |> List.concatMap (\j -> makeQuad i j)
                    )
    in
    WebGL.triangles allTriangles


{-| Render the animated ocean surface with Gerstner wave displacement.
-}
oceanEntity : Camera -> Float -> Int -> Int -> WebGL.Entity
oceanEntity camera simTime width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        view =
            Camera.viewMatrix camera

        perspective =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)

        eyePos =
            camera.eye

        sunDir =
            Vec3.normalize (vec3 0.45 0.85 0.35)

        uniforms =
            { u_view = view
            , u_perspective = perspective
            , u_eyePos = eyePos
            , u_sunDir = sunDir
            , u_time = simTime
            }
    in
    WebGL.entityWith
        [ DepthTest.default
        , WebGL.Settings.cullFace WebGL.Settings.back
        ]
        waterVertexShader
        waterFragmentShader
        buildWaterMesh
        uniforms


{-| Sky dome entity.
-}
skyEntity : Camera -> Int -> Int -> WebGL.Entity
skyEntity camera width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        view =
            Camera.viewMatrix camera

        perspective =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)

        eyePos =
            camera.eye

        uniforms =
            { u_view = view
            , u_perspective = perspective
            , u_eyePos = eyePos
            , u_zenith = vec3 0.12 0.42 0.82
            , u_horizon = vec3 0.72 0.86 0.95
            , u_sunDir = Vec3.normalize (vec3 0.45 0.85 0.35)
            }
    in
    WebGL.entityWith
        [ DepthTest.never
        ]
        skyVertexShader
        skyFragmentShader
        buildSkyMesh
        uniforms


buildSkyMesh : WebGL.Mesh { position : Vec3 }
buildSkyMesh =
    let
        r =
            420.0

        rings =
            8

        segs =
            16

        ringAngle i =
            (toFloat i / toFloat rings) * (pi / 2.0)

        segAngle j =
            (toFloat j / toFloat segs) * (2.0 * pi)

        makeVertex i j =
            let
                phi =
                    ringAngle i

                theta =
                    segAngle j

                x =
                    r * cos phi * sin theta

                y =
                    r * sin phi

                z =
                    r * cos phi * cos theta
            in
            { position = vec3 x y z }

        makeQuad i j =
            let
                v00 =
                    makeVertex i j

                v10 =
                    makeVertex (i + 1) j

                v01 =
                    makeVertex i (j + 1)

                v11 =
                    makeVertex (i + 1) (j + 1)
            in
            [ ( v00, v01, v10 ), ( v10, v01, v11 ) ]

        triangles =
            List.range 0 (rings - 1)
                |> List.concatMap
                    (\i ->
                        List.range 0 (segs - 1)
                            |> List.concatMap (\j -> makeQuad i j)
                    )
    in
    WebGL.triangles triangles


{-| Navigation buoy meshes (Port red, Starboard green, Finish gate).
-}
buildBuoyMesh : List Waypoint -> WebGL.Mesh OceanVertex
buildBuoyMesh waypoints =
    let
        makeCylinder pos col radius height =
            let
                segs =
                    10

                step =
                    2.0 * pi / toFloat segs

                p =
                    Vec3.toRecord pos

                makeRing yOffset =
                    List.range 0 segs
                        |> List.map
                            (\s ->
                                let
                                    a =
                                        toFloat s * step

                                    x =
                                        p.x + radius * sin a

                                    z =
                                        p.z + radius * cos a

                                    norm =
                                        vec3 (sin a) 0.0 (cos a)
                                in
                                OceanVertex (vec3 x (p.y + yOffset) z) norm col
                            )

                bottom =
                    makeRing 0.0

                top =
                    makeRing height

                quads b1 b2 t1 t2 =
                    [ ( b1, t1, b2 ), ( b2, t1, t2 ) ]

                foldQuads bList tList =
                    case ( bList, tList ) of
                        ( b1 :: b2 :: bRest, t1 :: t2 :: tRest ) ->
                            quads b1 b2 t1 t2 ++ foldQuads (b2 :: bRest) (t2 :: tRest)

                        _ ->
                            []
            in
            foldQuads bottom top

        buoyTriangles wp =
            let
                col =
                    case wp.kind of
                        PortBuoy ->
                            vec3 0.88 0.12 0.12 -- Bright Port Red

                        StarboardBuoy ->
                            vec3 0.12 0.78 0.22 -- Bright Starboard Green

                        FinishGate ->
                            vec3 0.98 0.82 0.12 -- Gold Finish Line
            in
            makeCylinder wp.position col 1.2 3.8
    in
    WebGL.triangles (List.concatMap buoyTriangles waypoints)


buoyEntity : Camera -> List Waypoint -> Int -> Int -> WebGL.Entity
buoyEntity camera waypoints width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        view =
            Camera.viewMatrix camera

        perspective =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)

        eyePos =
            camera.eye

        sunDir =
            Vec3.normalize (vec3 0.45 0.85 0.35)

        uniforms =
            { u_view = view
            , u_perspective = perspective
            , u_eyePos = eyePos
            , u_sunDir = sunDir
            , u_time = 0.0
            }
    in
    WebGL.entityWith
        [ DepthTest.default
        ]
        buoyVertexShader
        buoyFragmentShader
        (buildBuoyMesh waypoints)
        uniforms


{-| Combat projectile and impact meshes (flying iron cannonballs, water splash plumes, hit spark bursts).
-}
buildCombatMesh : List Cannonball -> List CombatImpact -> WebGL.Mesh OceanVertex
buildCombatMesh balls impacts =
    let
        ironCol =
            vec3 0.14 0.14 0.16

        splashCol =
            vec3 0.92 0.96 1.0

        hitCol =
            vec3 1.0 0.65 0.15

        makeOctahedron center col r =
            let
                p =
                    Vec3.toRecord center

                top =
                    OceanVertex (vec3 p.x (p.y + r) p.z) (vec3 0.0 1.0 0.0) col

                bottom =
                    OceanVertex (vec3 p.x (p.y - r) p.z) (vec3 0.0 -1.0 0.0) col

                v1 =
                    OceanVertex (vec3 (p.x + r) p.y p.z) (vec3 1.0 0.0 0.0) col

                v2 =
                    OceanVertex (vec3 p.x p.y (p.z + r)) (vec3 0.0 0.0 1.0) col

                v3 =
                    OceanVertex (vec3 (p.x - r) p.y p.z) (vec3 -1.0 0.0 0.0) col

                v4 =
                    OceanVertex (vec3 p.x p.y (p.z - r)) (vec3 0.0 0.0 -1.0) col
            in
            [ ( top, v1, v2 )
            , ( top, v2, v3 )
            , ( top, v3, v4 )
            , ( top, v4, v1 )
            , ( bottom, v2, v1 )
            , ( bottom, v3, v2 )
            , ( bottom, v4, v3 )
            , ( bottom, v1, v4 )
            ]

        makeSplashCone center age isHit =
            let
                p =
                    Vec3.toRecord center

                col =
                    if isHit then
                        hitCol

                    else
                        splashCol

                hProgress =
                    clamp 0.0 1.0 (age / 0.65)

                height =
                    if isHit then
                        0.8 + 1.2 * sin (hProgress * pi)

                    else
                        max 0.2 (2.4 * sin (hProgress * pi))

                r =
                    if isHit then
                        0.3 + hProgress * 1.6

                    else
                        0.4 + hProgress * 2.0

                apex =
                    OceanVertex (vec3 p.x (p.y + height) p.z) (vec3 0.0 1.0 0.0) col

                segs =
                    6

                step =
                    2.0 * pi / toFloat segs

                baseVerts =
                    List.range 0 segs
                        |> List.map
                            (\s ->
                                let
                                    a =
                                        toFloat s * step

                                    bx =
                                        p.x + r * sin a

                                    bz =
                                        p.z + r * cos a

                                    norm =
                                        Vec3.normalize (vec3 (sin a) 0.5 (cos a))
                                in
                                OceanVertex (vec3 bx p.y bz) norm col
                            )

                fan list =
                    case list of
                        b1 :: b2 :: rest ->
                            ( apex, b1, b2 ) :: fan (b2 :: rest)

                        _ ->
                            []
            in
            fan baseVerts

        ballTriangles =
            List.concatMap (\b -> makeOctahedron b.position ironCol 0.28) balls

        impactTriangles =
            List.concatMap (\imp -> makeSplashCone imp.position imp.age imp.isHit) impacts
    in
    WebGL.triangles (ballTriangles ++ impactTriangles)


{-| WebGL entity for rendering flying cannonballs and splash/impact VFX.
-}
combatEntity : Camera -> List Cannonball -> List CombatImpact -> Int -> Int -> WebGL.Entity
combatEntity camera balls impacts width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        view =
            Camera.viewMatrix camera

        perspective =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)

        eyePos =
            camera.eye

        sunDir =
            Vec3.normalize (vec3 0.45 0.85 0.35)

        uniforms =
            { u_view = view
            , u_perspective = perspective
            , u_eyePos = eyePos
            , u_sunDir = sunDir
            , u_time = 0.0
            }
    in
    WebGL.entityWith
        [ DepthTest.default
        ]
        buoyVertexShader
        buoyFragmentShader
        (buildCombatMesh balls impacts)
        uniforms


{-| Archipelago islands and lighthouse mesh.
-}
buildIslandMesh : WebGL.Mesh OceanVertex
buildIslandMesh =
    let
        -- Distant archipelago island rocks + lighthouse
        rockCol =
            vec3 0.42 0.44 0.46

        grassCol =
            vec3 0.28 0.52 0.26

        whiteCol =
            vec3 0.92 0.92 0.94

        redCol =
            vec3 0.82 0.15 0.15

        makeMound cx cz rx rz height =
            let
                top =
                    OceanVertex (vec3 cx height cz) (vec3 0.0 1.0 0.0) grassCol

                segs =
                    12

                step =
                    2.0 * pi / toFloat segs

                baseVerts =
                    List.range 0 segs
                        |> List.map
                            (\s ->
                                let
                                    a =
                                        toFloat s * step

                                    x =
                                        cx + rx * sin a

                                    z =
                                        cz + rz * cos a

                                    norm =
                                        Vec3.normalize (vec3 (sin a) 0.3 (cos a))
                                in
                                OceanVertex (vec3 x -1.0 z) norm rockCol
                            )

                fan list =
                    case list of
                        v1 :: v2 :: rest ->
                            ( top, v1, v2 ) :: fan (v2 :: rest)

                        _ ->
                            []
            in
            fan baseVerts

        -- Islands around the bay
        island1 =
            makeMound -180.0 220.0 95.0 110.0 18.0

        island2 =
            makeMound 210.0 320.0 80.0 95.0 15.0

        island3 =
            makeMound 40.0 540.0 120.0 130.0 22.0
    in
    WebGL.triangles (island1 ++ island2 ++ island3)


islandEntity : Camera -> Int -> Int -> WebGL.Entity
islandEntity camera width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        view =
            Camera.viewMatrix camera

        perspective =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)

        eyePos =
            camera.eye

        sunDir =
            Vec3.normalize (vec3 0.45 0.85 0.35)

        uniforms =
            { u_view = view
            , u_perspective = perspective
            , u_eyePos = eyePos
            , u_sunDir = sunDir
            , u_time = 0.0
            }
    in
    WebGL.entityWith
        [ DepthTest.default
        ]
        buoyVertexShader
        buoyFragmentShader
        buildIslandMesh
        uniforms



-- ── Shaders ──────────────────────────────────────────────────────────────────


waterVertexShader : WebGL.Shader OceanVertex Uniforms { v_worldPos : Vec3, v_normal : Vec3, v_viewDir : Vec3 }
waterVertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 uv;

        uniform mat4 u_view;
        uniform mat4 u_perspective;
        uniform vec3 u_eyePos;
        uniform float u_time;

        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_viewDir;

        void main () {
            // Gerstner wave displacement
            float k1 = 2.0 * 3.14159 / 40.0;
            float w1 = k1 * 4.0;
            float wave1 = 0.45 * sin(k1 * (position.z * 0.9 + position.x * 0.4) - w1 * u_time);

            float k2 = 2.0 * 3.14159 / 16.0;
            float w2 = k2 * 3.2;
            float wave2 = 0.20 * sin(k2 * (position.x * 0.8 - position.z * 0.5) - w2 * u_time);

            vec3 displaced = position;
            displaced.y = wave1 + wave2;

            // Wave normal derivation
            float dhDx = 0.45 * k1 * 0.4 * cos(k1 * (position.z * 0.9 + position.x * 0.4) - w1 * u_time)
                       + 0.20 * k2 * 0.8 * cos(k2 * (position.x * 0.8 - position.z * 0.5) - w2 * u_time);
            float dhDz = 0.45 * k1 * 0.9 * cos(k1 * (position.z * 0.9 + position.x * 0.4) - w1 * u_time)
                       - 0.20 * k2 * 0.5 * cos(k2 * (position.x * 0.8 - position.z * 0.5) - w2 * u_time);

            vec3 worldNormal = normalize(vec3(-dhDx, 1.0, -dhDz));

            v_worldPos = displaced;
            v_normal = worldNormal;
            v_viewDir = u_eyePos - displaced;

            gl_Position = u_perspective * u_view * vec4(displaced, 1.0);
        }
    |]


waterFragmentShader : WebGL.Shader {} Uniforms { v_worldPos : Vec3, v_normal : Vec3, v_viewDir : Vec3 }
waterFragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 u_eyePos;
        uniform vec3 u_sunDir;

        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_viewDir;

        void main () {
            vec3 N = normalize(v_normal);
            vec3 V = normalize(v_viewDir);
            vec3 L = normalize(u_sunDir);

            // Deep ocean base and subsurface turquoise
            vec3 deepColor = vec3(0.047, 0.290, 0.431); // #0c4a6e
            vec3 shallowColor = vec3(0.008, 0.518, 0.780); // #0284c7

            // Fresnel reflectance: Schlick approximation
            float NdotV = max(0.0, dot(N, V));
            float fresnel = 0.04 + 0.96 * pow(1.0 - NdotV, 4.0);

            // Specular sun highlight (Blinn-Phong)
            vec3 H = normalize(L + V);
            float NdotH = max(0.0, dot(N, H));
            float spec = pow(NdotH, 64.0) * 1.8;

            // Crest foam detection based on wave height
            float crest = smoothstep(0.42, 0.62, v_worldPos.y);
            vec3 foamColor = vec3(0.94, 0.99, 0.96);

            vec3 waterCol = mix(deepColor, shallowColor, smoothstep(-0.3, 0.4, v_worldPos.y));
            vec3 skyReflect = vec3(0.72, 0.86, 0.95);
            vec3 litColor = mix(waterCol, skyReflect, fresnel) + vec3(spec);
            vec3 finalColor = mix(litColor, foamColor, crest * 0.85);

            // Distance fog fading to horizon
            float dist = length(v_worldPos - u_eyePos);
            float fogFactor = clamp((dist - 120.0) / 320.0, 0.0, 1.0);
            vec3 fogColor = vec3(0.72, 0.86, 0.95);

            gl_FragColor = vec4(mix(finalColor, fogColor, fogFactor), 1.0);
        }
    |]


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
            vec3 worldPos = position + u_eyePos;
            gl_Position = (u_perspective * u_view * vec4(worldPos, 1.0)).xyww;
        }
    |]


skyFragmentShader : WebGL.Shader {} SkyUniforms { v_dir : Vec3 }
skyFragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 u_zenith;
        uniform vec3 u_horizon;
        uniform vec3 u_sunDir;

        varying vec3 v_dir;

        void main () {
            vec3 dir = normalize(v_dir);
            float y = max(0.0, dir.y);

            // Vertical gradient from horizon to zenith
            vec3 sky = mix(u_horizon, u_zenith, pow(y, 0.65));

            // Sun disc highlight
            float sunDot = max(0.0, dot(dir, normalize(u_sunDir)));
            float sunDisc = pow(sunDot, 120.0) * 0.9 + pow(sunDot, 600.0) * 1.5;

            gl_FragColor = vec4(sky + vec3(sunDisc), 1.0);
        }
    |]


buoyVertexShader : WebGL.Shader OceanVertex Uniforms { v_worldPos : Vec3, v_normal : Vec3, v_color : Vec3 }
buoyVertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 uv; // uv.xyz holds rgb vertex color

        uniform mat4 u_view;
        uniform mat4 u_perspective;
        uniform vec3 u_eyePos;

        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;

        void main () {
            v_worldPos = position;
            v_normal = normal;
            v_color = uv;

            gl_Position = u_perspective * u_view * vec4(position, 1.0);
        }
    |]


buoyFragmentShader : WebGL.Shader {} Uniforms { v_worldPos : Vec3, v_normal : Vec3, v_color : Vec3 }
buoyFragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 u_eyePos;
        uniform vec3 u_sunDir;

        varying vec3 v_worldPos;
        varying vec3 v_normal;
        varying vec3 v_color;

        void main () {
            vec3 N = normalize(v_normal);
            vec3 L = normalize(u_sunDir);

            float diff = max(0.25, dot(N, L));
            vec3 lit = v_color * diff;

            // Distance fog
            float dist = length(v_worldPos - u_eyePos);
            float fogFactor = clamp((dist - 120.0) / 320.0, 0.0, 1.0);
            vec3 fogColor = vec3(0.72, 0.86, 0.95);

            gl_FragColor = vec4(mix(lit, fogColor, fogFactor), 1.0);
        }
    |]
