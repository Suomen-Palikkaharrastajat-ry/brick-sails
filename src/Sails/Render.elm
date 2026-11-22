module Sails.Render exposing
    ( RenderBoat
    , renderShipyard
    , renderWorld
    )

{-| Draws the sailing world: sea, sky, blocky islands, buoys, the finish
gate, floating targets, cannonballs and the toy boats.

The boats are real brick models (`Sails.ShipMesh`), drawn with the brick
pipeline's plastic shader and edge shader. Everything in one canvas shares a
single view and projection (`Sails.OceanShader.Ctx`), so the depth buffer is
consistent: the water hides exactly the part of the hull below the waterline.
The props around the course are small procedural meshes.

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.EdgeShader as EdgeShader
import Render.Mesh exposing (Vertex)
import Render.Shader as Shader
import Sails.OceanShader as Ocean exposing (Ctx, PropVertex)
import Sails.Physics as Physics
import Sails.ShipMesh exposing (ShipMesh)
import Sails.Types
    exposing
        ( BuoyKind(..)
        , Cannonball
        , CombatImpact
        , Course
        , SailingState
        , Target
        , Waypoint
        )
import WebGL
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest



-- PUBLIC


{-| A boat to draw: its brick geometry (`Nothing` while its parts are still
loading), its state, and an optional sail colour override (used to tell
multiplayer boats apart).
-}
type alias RenderBoat =
    { mesh : Maybe ShipMesh
    , state : SailingState
    , sailTint : Maybe Vec3
    }


{-| Render the full race scene.
-}
renderWorld :
    { eye : Vec3, target : Vec3 }
    ->
        { course : Course
        , nextIndex : Int
        , boats : List RenderBoat
        , guide : Maybe { from : Vec3, to : Vec3 }
        , targets : List Target
        , balls : List Cannonball
        , impacts : List CombatImpact
        , time : Float
        }
    -> Int
    -> Int
    -> Html msg
renderWorld cam scene width height =
    let
        ctx =
            Ocean.makeCtx cam scene.time width height

        entities =
            [ Ocean.skyEntity ctx
            , Ocean.oceanEntity ctx
            ]
                ++ islandEntities ctx scene.course
                ++ List.concatMap (buoyEntities ctx scene.time scene.nextIndex scene.course.waypoints) scene.course.waypoints
                ++ List.concatMap (targetEntities ctx scene.time) scene.targets
                ++ List.concatMap (boatEntities ctx width height) scene.boats
                ++ guideEntities ctx scene.guide
                ++ List.map (ballEntity ctx) scene.balls
                ++ List.map (impactEntity ctx) scene.impacts
    in
    canvas width height entities


{-| Shipyard preview: one boat on the water, seen from the side, slowly
circling. Shows only the sea while the boat's parts are loading.
-}
renderShipyard : Maybe ShipMesh -> Float -> Int -> Int -> Html msg
renderShipyard mesh time width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        size =
            mesh
                |> Maybe.map (\m -> max m.lengthMeters (m.heightMeters * 0.8))
                |> Maybe.withDefault 5

        -- Pull back on narrow portrait screens so the whole boat fits.
        dist =
            if aspect < 1 then
                2.5 * size / max 0.45 aspect

            else
                2.4 * size

        boatMid =
            mesh |> Maybe.map (\m -> m.heightMeters * 0.32) |> Maybe.withDefault 1.6

        -- On portrait screens the boat picker covers the lower half, so aim
        -- below the boat to lift it into the open sky above the panel.
        lookY =
            if aspect < 1 then
                boatMid - 0.55 * size

            else
                boatMid

        angle =
            time * 0.35

        cam =
            { eye = vec3 (sin angle * dist) (boatMid + dist * 0.3) (cos angle * dist)
            , target = vec3 0 lookY 0
            }

        ctx =
            Ocean.makeCtx cam time width height

        state =
            { position = vec3 0 (Physics.waterElevation 0 0 time) 0
            , heading = 0
            , speed = 0
            , steer = 0
            , sailPower = 1
            , sailAngle = 25
            , heel = 0
            , pitch = 2 * sin (time * 1.7)
            , bumped = False
            }
    in
    canvas width
        height
        ([ Ocean.skyEntity ctx, Ocean.oceanEntity ctx ]
            ++ boatEntities ctx width height { mesh = mesh, state = state, sailTint = Nothing }
        )


canvas : Int -> Int -> List WebGL.Entity -> Html msg
canvas width height entities =
    WebGL.toHtmlWith
        [ WebGL.alpha False
        , WebGL.antialias
        , WebGL.depth 1
        , WebGL.clearColor 0.75 0.88 0.97 1
        ]
        [ Attr.width width
        , Attr.height height
        , Attr.class "w-full h-full block"
        ]
        entities



-- ENTITIES


white : Vec3
white =
    vec3 1 1 1


{-| Draw one brick boat: the plastic surface pass, then the brick edges while
the boat is near enough for them to read as edges rather than a dark smear.
-}
boatEntities : Ctx -> Int -> Int -> RenderBoat -> List WebGL.Entity
boatEntities ctx width height rb =
    case rb.mesh of
        Nothing ->
            []

        Just mesh ->
            let
                s =
                    rb.state

                p =
                    Vec3.toRecord s.position

                world =
                    Mat4.makeTranslate3 p.x p.y p.z
                        |> rotY (degrees s.heading)
                        |> rotZ (degrees -s.heel)
                        |> rotX (degrees -s.pitch)

                model =
                    Mat4.mul world mesh.toBoat

                tint =
                    case rb.sailTint of
                        Just c ->
                            vec4 (Vec3.getX c) (Vec3.getY c) (Vec3.getZ c) 1

                        Nothing ->
                            vec4 1 1 1 0

                distance =
                    Vec3.distance ctx.eye s.position

                surface =
                    WebGL.entityWith [ DepthTest.default ]
                        brickVertexShader
                        Shader.fragmentShader
                        mesh.scene.triangleMesh
                        { modelMatrix = model
                        , viewMatrix = ctx.view
                        , projectionMatrix = ctx.perspective
                        , viewPosition = ctx.eye
                        , lightDirection = Ocean.sunDirection
                        , instanceColor = tint
                        , ambientStrength = 0.62
                        , lightStrength = 0.5
                        , specularStrength = 0.18
                        , specularPower = 24
                        , rimStrength = 0.1
                        , rimPower = 2.5
                        , vibrance = 0.2
                        , metallic = 0
                        , u_ghostMode = 0
                        , u_ghostAlpha = 1
                        }

                edgeWidth =
                    clamp 0 1.2 (1.2 * (26 - distance) / 10)

                edges =
                    if edgeWidth > 0.3 then
                        [ WebGL.entityWith
                            [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
                            , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
                            ]
                            EdgeShader.vertexShader
                            EdgeShader.fragmentShader
                            mesh.scene.lineMesh
                            { modelMatrix = model
                            , viewMatrix = ctx.view
                            , projectionMatrix = ctx.perspective
                            , edgeColor = vec3 0.12 0.13 0.16
                            , viewportWidth = toFloat width
                            , viewportHeight = toFloat height
                            , lineWidth = edgeWidth
                            }
                        ]

                    else
                        []
            in
            surface :: edges


{-| The brick pipeline's vertex shader, with one change: vertices flagged in
`inherit` (the sail bricks, see `Sails.ShipMesh`) take `instanceColor` when
its alpha is 1, which paints another player's sails in their colour. With
alpha 0 every brick keeps its own colour.
-}
brickVertexShader : WebGL.Shader Vertex Shader.Uniforms { vColor : Vec4, vNormalWorld : Vec3, vPositionWorld : Vec3 }
brickVertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec4 color;
        attribute float inherit;

        uniform mat4 modelMatrix;
        uniform mat4 viewMatrix;
        uniform mat4 projectionMatrix;
        uniform vec4 instanceColor;

        varying vec4 vColor;
        varying vec3 vNormalWorld;
        varying vec3 vPositionWorld;

        void main() {
            vec4 worldPosition = modelMatrix * vec4(position, 1.0);
            gl_Position = projectionMatrix * viewMatrix * worldPosition;
            vColor = mix(color, vec4(instanceColor.rgb, color.a), inherit * instanceColor.a);
            vNormalWorld = mat3(modelMatrix) * normal;
            vPositionWorld = worldPosition.xyz;
        }
    |]


ocean : Ctx -> Mat4 -> Vec3 -> WebGL.Mesh PropVertex -> WebGL.Entity
ocean =
    Ocean.propEntity


buoyEntities : Ctx -> Float -> Int -> List Waypoint -> Waypoint -> List WebGL.Entity
buoyEntities ctx time nextIndex all wp =
    let
        p =
            Vec3.toRecord wp.position

        isNext =
            wp.index == nextIndex

        passed =
            wp.index < nextIndex

        bob =
            0.2 * sin (time * 2 + toFloat wp.index) + Physics.seaElevation ctx.sea p.x p.z time

        at =
            Mat4.makeTranslate3 p.x bob p.z

        color =
            if passed then
                vec3 0.55 0.58 0.62

            else
                case wp.kind of
                    PortBuoy ->
                        vec3 0.9 0.15 0.12

                    StarboardBuoy ->
                        vec3 0.1 0.72 0.25

                    FinishGate ->
                        vec3 1 0.8 0.1

        pulse =
            1 + 0.06 * sin (time * 5)

        ring =
            if isNext then
                [ ocean ctx (Mat4.makeTranslate3 p.x 0.35 p.z |> Mat4.scale3 (wp.radius * pulse) 1 (wp.radius * pulse)) (vec3 1 0.92 0.3) ringMesh
                , ocean ctx (at |> rotY (time * 1.5)) (vec3 1 0.85 0.15) beaconMesh
                ]

            else
                []
    in
    case wp.kind of
        FinishGate ->
            let
                approach =
                    all
                        |> List.filter (\w -> w.index == wp.index - 1)
                        |> List.head
                        |> Maybe.map
                            (\prev ->
                                let
                                    a =
                                        Vec3.toRecord prev.position
                                in
                                atan2 (p.x - a.x) (p.z - a.z)
                            )
                        |> Maybe.withDefault 0
            in
            ocean ctx (Mat4.makeTranslate3 p.x 0 p.z |> rotY approach |> Mat4.scale3 wp.radius 1 1) white gateMesh
                :: ring

        _ ->
            ocean ctx at color buoyMesh :: ring


targetEntities : Ctx -> Float -> Target -> List WebGL.Entity
targetEntities ctx time t =
    if t.sunk && t.sinkAge > 1.6 then
        []

    else
        let
            p =
                Vec3.toRecord t.position

            sink =
                if t.sunk then
                    t.sinkAge * 1.6

                else
                    0
        in
        [ ocean ctx
            (Mat4.makeTranslate3 p.x (p.y - sink) p.z
                |> rotY (time * 0.4 + toFloat t.id)
                |> rotZ (sink * 0.5)
            )
            white
            targetMesh
        ]


islandEntities : Ctx -> Course -> List WebGL.Entity
islandEntities ctx course =
    List.indexedMap
        (\i isl ->
            ocean ctx
                (Mat4.makeTranslate3 isl.x -0.6 isl.z |> Mat4.scale3 isl.radius isl.height isl.radius)
                white
                islandMesh
                :: (if i == 0 then
                        [ ocean ctx (Mat4.makeTranslate3 isl.x (isl.height - 0.6) isl.z) white lighthouseMesh ]

                    else
                        []
                   )
        )
        course.islands
        |> List.concat


guideEntities : Ctx -> Maybe { from : Vec3, to : Vec3 } -> List WebGL.Entity
guideEntities ctx guide =
    case guide of
        Just g ->
            let
                a =
                    Vec3.toRecord g.from

                b =
                    Vec3.toRecord g.to

                dx =
                    b.x - a.x

                dz =
                    b.z - a.z

                d =
                    sqrt (dx * dx + dz * dz)
            in
            if d > 14 then
                [ ocean ctx
                    (Mat4.makeTranslate3 (a.x + dx / d * 6.5) 0.45 (a.z + dz / d * 6.5) |> rotY (atan2 dx dz))
                    (vec3 1 0.88 0.2)
                    arrowMesh
                ]

            else
                []

        Nothing ->
            []


ballEntity : Ctx -> Cannonball -> WebGL.Entity
ballEntity ctx ball =
    let
        p =
            Vec3.toRecord ball.position
    in
    ocean ctx (Mat4.makeTranslate3 p.x p.y p.z) white ballMesh


impactEntity : Ctx -> CombatImpact -> WebGL.Entity
impactEntity ctx imp =
    let
        p =
            Vec3.toRecord imp.position

        t =
            clamp 0 1 (imp.age / 0.7)

        h =
            max 0.1 (sin (t * pi)) * 3

        r =
            0.6 + t * 2.2
    in
    ocean ctx
        (Mat4.makeTranslate3 p.x p.y p.z |> Mat4.scale3 r h r)
        (if imp.isHit then
            vec3 1 0.6 0.15

         else
            white
        )
        splashMesh



-- MATRIX HELPERS


rotX : Float -> Mat4 -> Mat4
rotX a m =
    Mat4.rotate a (vec3 1 0 0) m


rotY : Float -> Mat4 -> Mat4
rotY a m =
    Mat4.rotate a (vec3 0 1 0) m


rotZ : Float -> Mat4 -> Mat4
rotZ a m =
    Mat4.rotate a (vec3 0 0 1) m



-- MESH BUILDING


type alias Tri =
    ( PropVertex, PropVertex, PropVertex )


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List Tri
face a b c d col =
    let
        n =
            Vec3.normalize (Vec3.cross (Vec3.sub b a) (Vec3.sub c a))

        v p =
            { position = p, normal = n, color = col }
    in
    [ ( v a, v b, v c ), ( v a, v c, v d ) ]


{-| Axis-aligned box from (x0, y0, z0) to (x1, y1, z1).
-}
box : ( Float, Float, Float ) -> ( Float, Float, Float ) -> Vec3 -> List Tri
box ( x0, y0, z0 ) ( x1, y1, z1 ) col =
    let
        p =
            vec3
    in
    List.concat
        [ face (p x0 y1 z0) (p x0 y1 z1) (p x1 y1 z1) (p x1 y1 z0) col
        , face (p x0 y0 z0) (p x1 y0 z0) (p x1 y0 z1) (p x0 y0 z1) col
        , face (p x0 y0 z1) (p x1 y0 z1) (p x1 y1 z1) (p x0 y1 z1) col
        , face (p x1 y0 z0) (p x0 y0 z0) (p x0 y1 z0) (p x1 y1 z0) col
        , face (p x1 y0 z1) (p x1 y0 z0) (p x1 y1 z0) (p x1 y1 z1) col
        , face (p x0 y0 z0) (p x0 y0 z1) (p x0 y1 z1) (p x0 y1 z0) col
        ]


{-| Upright n-sided prism centred on (cx, cz).
-}
prism : Int -> Float -> ( Float, Float ) -> ( Float, Float ) -> Vec3 -> List Tri
prism n r ( cx, cz ) ( y0, y1 ) col =
    let
        pt i y =
            let
                a =
                    2 * pi * toFloat i / toFloat n
            in
            vec3 (cx + r * sin a) y (cz + r * cos a)

        side i =
            face (pt i y0) (pt (i + 1) y0) (pt (i + 1) y1) (pt i y1) col

        cap i =
            let
                up =
                    { position = vec3 cx y1 cz, normal = vec3 0 1 0, color = col }

                down =
                    { position = vec3 cx y0 cz, normal = vec3 0 -1 0, color = col }

                at y nrm j =
                    { position = pt j y, normal = nrm, color = col }
            in
            [ ( up, at y1 (vec3 0 1 0) i, at y1 (vec3 0 1 0) (i + 1) )
            , ( down, at y0 (vec3 0 -1 0) (i + 1), at y0 (vec3 0 -1 0) i )
            ]
    in
    List.range 0 (n - 1) |> List.concatMap (\i -> side i ++ cap i)


{-| A brick stud on top of a surface at height y.
-}
stud : Float -> Float -> Float -> Vec3 -> List Tri
stud x y z col =
    prism 8 0.15 ( x, z ) ( y, y + 0.1 ) col


transformTris : Mat4 -> List Tri -> List Tri
transformTris m =
    let
        tv v =
            { v | position = Mat4.transform m v.position, normal = Vec3.normalize (Vec3.sub (Mat4.transform m (Vec3.add v.position v.normal)) (Mat4.transform m v.position)) }
    in
    List.map (\( a, b, c ) -> ( tv a, tv b, tv c ))



-- MESHES


darkWood : Vec3
darkWood =
    vec3 0.35 0.22 0.12


black : Vec3
black =
    vec3 0.12 0.12 0.14


buoyMesh : WebGL.Mesh PropVertex
buoyMesh =
    WebGL.triangles
        (prism 8 1.0 ( 0, 0 ) ( -0.6, 0.8 ) white
            ++ box ( -0.4, 0.8, -0.4 ) ( 0.4, 2.6, 0.4 ) white
            ++ box ( -0.42, 1.5, -0.42 ) ( 0.42, 1.9, 0.42 ) (vec3 0.95 0.95 0.95)
            ++ stud 0 2.6 0 white
        )


beaconMesh : WebGL.Mesh PropVertex
beaconMesh =
    WebGL.triangles
        (box ( -0.08, 2.6, -0.08 ) ( 0.08, 8.5, 0.08 ) (vec3 0.9 0.9 0.9)
            ++ box ( 0, 7.0, -0.04 ) ( 1.8, 8.4, 0.04 ) white
        )


ringMesh : WebGL.Mesh PropVertex
ringMesh =
    let
        n =
            40

        pt r i =
            let
                a =
                    2 * pi * toFloat i / toFloat n
            in
            vec3 (r * sin a) 0 (r * cos a)

        seg i =
            if modBy 2 i == 0 then
                face (pt 0.9 i) (pt 0.9 (i + 1)) (pt 1.0 (i + 1)) (pt 1.0 i) white
                    |> List.map (\( a, b, c ) -> ( a, c, b ))
                    |> List.map (\( a, b, c ) -> ( { a | normal = vec3 0 1 0 }, { b | normal = vec3 0 1 0 }, { c | normal = vec3 0 1 0 } ))

            else
                []
    in
    WebGL.triangles (List.range 0 (n - 1) |> List.concatMap seg)


gateMesh : WebGL.Mesh PropVertex
gateMesh =
    let
        -- Scaled in X by the gate radius: posts sit at x = -1 and x = 1.
        post x =
            box ( x - 0.06, -0.5, -0.5 ) ( x + 0.06, 7.0, 0.5 ) white

        checker i =
            let
                x0 =
                    -1 + toFloat i * 0.2
            in
            box ( x0, 6.0, -0.15 )
                ( x0 + 0.2, 7.2, 0.15 )
                (if modBy 2 i == 0 then
                    black

                 else
                    white
                )
    in
    WebGL.triangles
        (post -1 ++ post 1 ++ List.concatMap checker (List.range 0 9))


targetMesh : WebGL.Mesh PropVertex
targetMesh =
    WebGL.triangles
        (box ( -0.8, -0.4, -0.8 ) ( 0.8, 0.5, 0.8 ) (vec3 0.6 0.4 0.2)
            ++ box ( -0.82, 0.05, -0.82 ) ( 0.82, 0.15, 0.82 ) darkWood
            ++ prism 12 0.7 ( 0, 0 ) ( 0.5, 0.62 ) (vec3 0.9 0.12 0.12)
            ++ prism 12 0.47 ( 0, 0 ) ( 0.62, 0.7 ) white
            ++ prism 12 0.22 ( 0, 0 ) ( 0.7, 0.78 ) (vec3 0.9 0.12 0.12)
        )


islandMesh : WebGL.Mesh PropVertex
islandMesh =
    -- Unit island (radius 1, height 1), scaled per island.
    WebGL.triangles
        (prism 12 1.0 ( 0, 0 ) ( -0.5, 0.18 ) (vec3 0.93 0.84 0.58)
            ++ prism 12 0.8 ( 0, 0 ) ( 0.18, 0.55 ) (vec3 0.35 0.68 0.3)
            ++ prism 12 0.5 ( 0.1, -0.05 ) ( 0.55, 1.0 ) (vec3 0.27 0.56 0.24)
            ++ prism 6 0.25 ( -0.35, 0.3 ) ( 0.55, 0.8 ) (vec3 0.55 0.55 0.58)
        )


lighthouseMesh : WebGL.Mesh PropVertex
lighthouseMesh =
    let
        band i =
            box ( -1.2, toFloat i * 1.6, -1.2 )
                ( 1.2, toFloat (i + 1) * 1.6, 1.2 )
                (if modBy 2 i == 0 then
                    vec3 0.95 0.95 0.95

                 else
                    vec3 0.85 0.15 0.15
                )
    in
    WebGL.triangles
        (List.concatMap band (List.range 0 4)
            ++ box ( -1.5, 8.0, -1.5 ) ( 1.5, 8.4, 1.5 ) black
            ++ box ( -0.9, 8.4, -0.9 ) ( 0.9, 9.8, 0.9 ) (vec3 1 0.9 0.4)
            ++ prism 4 1.3 ( 0, 0 ) ( 9.8, 10.6 ) (vec3 0.85 0.15 0.15)
        )


arrowMesh : WebGL.Mesh PropVertex
arrowMesh =
    let
        arm sign =
            box ( -0.25, 0, -1.6 ) ( 0.25, 0.25, 0 ) white
                |> transformTris (Mat4.makeRotate (sign * 0.75) (vec3 0 1 0))
    in
    WebGL.triangles
        (arm 1 ++ arm -1 ++ (arm 1 ++ arm -1 |> transformTris (Mat4.makeTranslate3 0 0 -1.4)))


ballMesh : WebGL.Mesh PropVertex
ballMesh =
    WebGL.triangles (prism 6 0.35 ( 0, 0 ) ( -0.3, 0.3 ) black)


splashMesh : WebGL.Mesh PropVertex
splashMesh =
    -- Unit splash: a short stack of shrinking rings.
    WebGL.triangles
        (prism 8 1.0 ( 0, 0 ) ( 0, 0.35 ) white
            ++ prism 8 0.65 ( 0, 0 ) ( 0.35, 0.7 ) white
            ++ prism 8 0.3 ( 0, 0 ) ( 0.7, 1.0 ) white
        )
