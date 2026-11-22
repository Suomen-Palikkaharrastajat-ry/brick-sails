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
import Render.Scene exposing (Scene)
import Render.Shader as Shader
import Sails.OceanShader as Ocean exposing (Ctx, PropVertex)
import Sails.Physics as Physics
import Sails.Scenery as Scenery
import Sails.ShipMesh exposing (ShipMesh)
import Sails.Types
    exposing
        ( BuoyKind(..)
        , Cannonball
        , CombatImpact
        , Course
        , SailingState
        , Target
        , Town
        , Treasure
        , Waypoint
        )
import Sails.Weather exposing (Conditions)
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
        , weather : Conditions
        , scenery : String -> Maybe ShipMesh
        , treasureFound : List Int
        }
    -> Int
    -> Int
    -> Html msg
renderWorld cam scene width height =
    let
        ctx =
            Ocean.makeCtx cam scene.time width height
                |> Ocean.withWeather scene.weather

        entities =
            [ Ocean.skyEntity ctx
            , Ocean.oceanEntity ctx
            ]
                ++ islandEntities ctx scene.course
                ++ townEntities ctx width height scene.scenery scene.course.towns
                ++ treasureEntities ctx width height scene.scenery scene.time scene.treasureFound scene.course.treasures
                ++ List.concatMap (buoyEntities ctx scene.time scene.nextIndex scene.course.waypoints) scene.course.waypoints
                ++ List.concatMap (targetEntities ctx scene.time) scene.targets
                ++ List.concatMap (boatEntities ctx width height) scene.boats
                ++ guideEntities ctx scene.guide
                ++ List.map (ballEntity ctx) scene.balls
                ++ List.map (impactEntity ctx) scene.impacts
                ++ List.concatMap (sprayEntities ctx scene.time) (List.take 1 scene.boats)
                ++ boltEntities ctx scene.weather
                ++ rainEntities ctx scene.time scene.weather
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
            , sail = 1
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


{-| Draw one brick boat: the hull and rig, and the sails hoisted as far as
the boat's sail setting.
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

                -- Lowered sails fold down onto their foot (the boom).
                hoist =
                    0.12 + 0.88 * clamp 0 1 s.sail

                sailModel =
                    Mat4.mul model
                        (Mat4.makeTranslate3 0 mesh.sailFoot 0
                            |> Mat4.scale3 1 hoist 1
                            |> Mat4.translate3 0 -mesh.sailFoot 0
                        )
            in
            brickPasses ctx width height distance tint model mesh.scene
                ++ brickPasses ctx width height distance tint sailModel mesh.sails


{-| Draw one brick scene: the plastic surface pass, then the brick edges
while it is near enough for them to read as edges rather than a dark smear.
Storm gloom dims the light and lightning flashes it.
-}
brickPasses : Ctx -> Int -> Int -> Float -> Vec4 -> Mat4 -> Scene -> List WebGL.Entity
brickPasses ctx width height distance tint model scene =
    let
        surface =
            WebGL.entityWith [ DepthTest.default ]
                brickVertexShader
                Shader.fragmentShader
                scene.triangleMesh
                { modelMatrix = model
                , viewMatrix = ctx.view
                , projectionMatrix = ctx.perspective
                , viewPosition = ctx.eye
                , lightDirection = Ocean.sunDirection
                , instanceColor = tint
                , ambientStrength = 0.62 - 0.22 * ctx.gloom + 0.5 * ctx.flash
                , lightStrength = 0.5 * (1 - 0.6 * ctx.gloom)
                , specularStrength = 0.18 * (1 - ctx.gloom)
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
                    scene.lineMesh
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


{-| A brick scenery piece standing at a point, its front facing `bearing`.
Nothing is drawn until its parts are in.
-}
sceneryEntities : Ctx -> Int -> Int -> (String -> Maybe ShipMesh) -> Scenery.Piece -> Vec3 -> Float -> List WebGL.Entity
sceneryEntities ctx width height scenery piece at bearing =
    case scenery (Scenery.key piece) of
        Just mesh ->
            let
                p =
                    Vec3.toRecord at

                model =
                    Mat4.makeTranslate3 p.x p.y p.z
                        |> rotY (degrees bearing)
                        |> (\m -> Mat4.mul m mesh.toBoat)
            in
            brickPasses ctx width height (Vec3.distance ctx.eye at) (vec4 1 1 1 0) model mesh.scene

        Nothing ->
            []


{-| Each town: its houses on the grass and its pier from the beach out to
the water's edge.
-}
townEntities : Ctx -> Int -> Int -> (String -> Maybe ShipMesh) -> List Town -> List WebGL.Entity
townEntities ctx width height scenery towns =
    List.concatMap
        (\town ->
            let
                isl =
                    town.island
            in
            sceneryEntities ctx width height scenery Scenery.houses (Physics.shorePoint isl town.bearing 0.6 |> Vec3.setY (Physics.grassHeight isl)) town.bearing
                ++ sceneryEntities ctx width height scenery Scenery.pier (Physics.shorePoint isl town.bearing (1 - 6 / isl.radius) |> Vec3.setY (Physics.beachHeight isl)) town.bearing
        )
        towns


{-| Treasure still to find: the chest, and a gold ring on the water showing
how close to sail to take it aboard.
-}
treasureEntities : Ctx -> Int -> Int -> (String -> Maybe ShipMesh) -> Float -> List Int -> List Treasure -> List WebGL.Entity
treasureEntities ctx width height scenery time found treasures =
    treasures
        |> List.filter (\t -> not (List.member t.id found))
        |> List.concatMap
            (\t ->
                let
                    p =
                        Vec3.toRecord t.position

                    r =
                        Physics.treasureReach * (1 + 0.05 * sin (time * 4))
                in
                ocean ctx (Mat4.makeTranslate3 p.x 0.4 p.z |> Mat4.scale3 r 1 r) (vec3 1 0.82 0.2) ringMesh
                    :: sceneryEntities ctx width height scenery Scenery.chest t.position t.bearing
            )


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



-- WEATHER


{-| Spray thrown up at the bow when the boat drives through a rough sea.
-}
sprayEntities : Ctx -> Float -> RenderBoat -> List WebGL.Entity
sprayEntities ctx time rb =
    let
        s =
            rb.state

        p =
            Vec3.toRecord s.position

        rough =
            clamp 0 1 ((ctx.sea - 1.1) / 1.2) * clamp 0 1 ((s.speed - 2) / 5)

        rad =
            degrees s.heading

        puff i =
            let
                phase =
                    fract (time * 1.4 + toFloat i * 0.5)

                side =
                    if i == 0 then
                        1

                    else
                        -1

                r =
                    (0.4 + phase * 1.4) * rough

                h =
                    max 0.05 (sin (phase * pi)) * 1.6 * rough
            in
            ocean ctx
                (Mat4.makeTranslate3
                    (p.x + sin rad * 2.3 + side * cos rad * (0.6 + phase))
                    (p.y + 0.2)
                    (p.z + cos rad * 2.3 - side * sin rad * (0.6 + phase))
                    |> Mat4.scale3 r h r
                )
                white
                splashMesh
    in
    if rough > 0.05 then
        [ puff 0, puff 1 ]

    else
        []


fract : Float -> Float
fract x =
    x - toFloat (floor x)


{-| A lightning bolt far off on the bearing of the last strike, while its
flash lasts.
-}
boltEntities : Ctx -> Conditions -> List WebGL.Entity
boltEntities ctx weather =
    if weather.flash > 0.25 then
        let
            e =
                Vec3.toRecord ctx.eye

            rad =
                degrees weather.bolt
        in
        [ ocean ctx
            (Mat4.makeTranslate3 (e.x + sin rad * 150) 0 (e.z + cos rad * 150) |> rotY rad)
            (vec3 1 1 1)
            boltMesh
        ]

    else
        []


{-| Rain falling around the camera: two layers of streaks scrolled down at
different speeds, slanted by the wind. The second layer only in heavy rain.
-}
rainEntities : Ctx -> Float -> Conditions -> List WebGL.Entity
rainEntities ctx time weather =
    let
        e =
            Vec3.toRecord ctx.eye

        lean =
            clamp 0 0.5 (weather.wind.speed / 40)

        windRad =
            degrees weather.wind.direction

        layer speed offset =
            let
                fall =
                    fract (time * speed / rainHeight + offset) * rainHeight
            in
            List.map
                (\k ->
                    ocean ctx
                        (Mat4.makeTranslate3 e.x (e.y + rainHeight * (0.5 - toFloat k) - fall) e.z
                            |> rotY windRad
                            |> (\m -> Mat4.mul m (shear lean))
                        )
                        (vec3 0.78 0.84 0.92)
                        rainMesh
                )
                [ 0, 1 ]
    in
    if weather.rain > 0.05 then
        layer 22 0
            ++ (if weather.rain > 0.5 then
                    layer 17 0.5

                else
                    []
               )

    else
        []


{-| Lean the rain streaks downwind: x and z move with height.
-}
shear : Float -> Mat4
shear lean =
    Mat4.fromRecord
        { m11 = 1
        , m21 = 0
        , m31 = 0
        , m41 = 0
        , m12 = 0
        , m22 = 1
        , m32 = -lean
        , m42 = 0
        , m13 = 0
        , m23 = 0
        , m33 = 1
        , m43 = 0
        , m14 = 0
        , m24 = 0
        , m34 = 0
        , m44 = 1
        }


rainHeight : Float
rainHeight =
    30


{-| Rain streaks scattered through a 70 x 30 x 70 m box around the origin,
at fixed pseudo-random places.
-}
rainMesh : WebGL.Mesh PropVertex
rainMesh =
    let
        pick i k =
            fract (sin (toFloat i * 12.9898 + toFloat k * 78.233) * 43758.5453)

        streak i =
            let
                x =
                    (pick i 1 - 0.5) * 70

                y =
                    (pick i 2 - 0.5) * rainHeight

                z =
                    (pick i 3 - 0.5) * 70
            in
            box ( x - 0.025, y, z - 0.025 ) ( x + 0.025, y + 1.1, z + 0.025 ) white
    in
    WebGL.triangles (List.concatMap streak (List.range 0 599))


{-| A jagged bolt from the cloud base to the sea, 80 m tall.
-}
boltMesh : WebGL.Mesh PropVertex
boltMesh =
    let
        pts =
            [ ( 0, 80 ), ( 4, 64 ), ( -2, 50 ), ( 5, 34 ), ( -1, 20 ), ( 3, 8 ), ( 0, 0 ) ]

        seg ( ( x0, y0 ), ( x1, y1 ) ) =
            let
                dx =
                    x1 - x0

                dy =
                    y1 - y0

                len =
                    sqrt (dx * dx + dy * dy)
            in
            box ( -0.6, 0, -0.6 ) ( 0.6, len, 0.6 ) (vec3 0.95 0.95 1)
                |> transformTris
                    (Mat4.makeTranslate3 x0 y0 0
                        |> Mat4.rotate (atan2 -dx dy) (vec3 0 0 1)
                    )
    in
    WebGL.triangles
        (List.concatMap seg (List.map2 Tuple.pair pts (List.drop 1 pts)))
