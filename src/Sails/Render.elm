module Sails.Render exposing
    ( Draw
    , Props
    , RenderBoat
    , ballEntity
    , propSources
    , renderShipyard
    , renderWorld
    , sprayEntities
    )

{-| Draws the sailing world: the tropical sea and sky, palm islands with the
fort's watchtower, the harbour piers, the pirate arch over the final
harbour, coral reefs and a pirate wreck, floating targets, cannonballs, the
colonial towns and the treasure on the islands, the toy boats, and the
weather (spray, rain and lightning).

The boats are real brick models (`Sails.ShipMesh`), drawn with the brick
pipeline's plastic shader and edge shader. The props around the course are
brick models too, from catalog parts (`Game.BrickModels`, shared with Brick
Racer, and `Sails.Scenery` for the palms, the fort, the reefs, the wreck and
the pirate flags), built once (`Props`) and drawn per piece with
`Game.BrickProps`, stretched to the sizes the course and the physics use.
The sea and the sky have the shared brick look (`Game.BrickLook`). Only the
rings round the harbours, the guide arrow, the rain and the lightning are
flat meshes. The towns and the treasure chest (`Sails.Scenery`) are brick
models drawn like the boats.

Everything in one canvas shares a single view and projection
(`Sails.OceanShader.Ctx`), so the depth buffer is consistent: the water hides
exactly the part of the hull below the waterline.

-}

import Dict exposing (Dict)
import Game.BrickLook as BrickLook
import Game.BrickModels as BrickModels
import Game.BrickProps as BrickProps exposing (PropModel)
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
        ( Cannonball
        , CombatImpact
        , Course
        , HarbourKind(..)
        , SailingState
        , Target
        , Town
        , Treasure
        , Waypoint
        )
import Sails.Weather exposing (Conditions)
import Sails.Wreck as Wreck
import WebGL
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest



-- PUBLIC


{-| The prop models built so far, by name (see `propSources`). Pieces whose
model is not built yet are not drawn.
-}
type alias Props =
    Dict String PropModel


{-| Every prop model the course uses, by name: load them with
`Game.BrickProps.refresh`.
-}
propSources : List ( String, String )
propSources =
    [ ( "buoy", BrickModels.buoy )
    , ( "beacon", BrickModels.beacon )
    , ( "floatingTarget", BrickModels.floatingTarget )
    , ( "island", BrickModels.island )
    , ( "fort", Scenery.fort )
    , ( "palmTree", Scenery.palmTree )
    , ( "reef", Scenery.reef )
    , ( "wreck", Scenery.wreck )
    , ( "pirateFlag", Scenery.pirateFlag )
    , ( "cannonball", BrickModels.cannonball )
    , ( "splash", BrickModels.splash )
    , ( "3001.dat", BrickModels.singlePart "3001.dat" )
    , ( "3003.dat", BrickModels.singlePart "3003.dat" )
    ]


{-| A boat to draw: its brick geometry (`Nothing` while its parts are still
loading), its state, and an optional sail colour override (used to tell
multiplayer boats apart).
-}
type alias RenderBoat =
    { mesh : Maybe ShipMesh
    , state : SailingState
    , sailTint : Maybe Vec3
    }


{-| Render the full voyage scene.
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
        , props : Props
        , debris : List Wreck.Piece
        }
    -> Int
    -> Int
    -> Html msg
renderWorld cam scene width height =
    let
        ctx =
            Ocean.makeCtx cam scene.time width height
                |> Ocean.withWeather scene.weather

        pv =
            Ocean.propView ctx width height

        drawProp name model tint =
            case Dict.get name scene.props of
                Just prop ->
                    BrickProps.draw pv model tint (edgeWidthAt ctx (Mat4.transform model (vec3 0 0 0))) prop

                Nothing ->
                    []

        withFit name fit at tint =
            case Dict.get name scene.props of
                Just prop ->
                    let
                        model =
                            Mat4.mul at (fit prop)
                    in
                    BrickProps.draw pv model tint (edgeWidthAt ctx (Mat4.transform at (vec3 0 0 0))) prop

                Nothing ->
                    []

        draw =
            { prop = drawProp, fitted = withFit }

        entities =
            [ Ocean.skyEntity ctx
            , Ocean.oceanEntity ctx
            ]
                ++ islandEntities draw scene.course
                ++ reefEntities draw
                ++ lighthouseBeamEntity ctx scene.time scene.course
                ++ townEntities ctx width height scene.scenery scene.course.towns
                ++ treasureEntities ctx width height scene.scenery scene.time scene.treasureFound scene.course.treasures
                ++ List.concatMap (harbourEntities ctx width height scene.scenery draw scene.time scene.nextIndex scene.course) scene.course.waypoints
                ++ List.concatMap (targetEntities draw scene.time) scene.targets
                ++ List.concatMap (boatEntities ctx width height) scene.boats
                ++ List.concatMap (debrisEntities draw) scene.debris
                ++ guideEntities ctx scene.guide
                ++ List.concatMap (ballEntity draw) scene.balls
                ++ List.concatMap (impactEntity draw) scene.impacts
                ++ List.concatMap (sprayEntities ctx draw scene.time) (List.take 1 scene.boats)
                ++ List.concatMap (wakeEntities ctx draw scene.time) scene.boats
                ++ boltEntities ctx scene.weather
                ++ rainEntities ctx scene.time scene.weather
    in
    canvas width height entities


{-| One loose brick of a wrecked boat (see `Sails.Wreck`), in its colour,
at about the scale of the boats' own bricks.
-}
debrisEntities : Draw -> Wreck.Piece -> List WebGL.Entity
debrisEntities draw piece =
    let
        p =
            Vec3.toRecord piece.position

        u =
            0.035
    in
    draw.prop piece.part
        (Mat4.makeTranslate3 p.x p.y p.z
            |> rotY piece.yaw
            |> rotX piece.tumble
            |> Mat4.translate3 0 (12 * u) 0
            |> Mat4.scale3 u u u
        )
        (tint3 (BrickLook.colour piece.colour))


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
        , WebGL.clearColor 0.62 0.86 0.95 1
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


{-| How to draw a brick prop: `prop` with a full model matrix, `fitted` with
a placement and a fit (see `Game.BrickProps.fitHeight` and friends), each
with a tint for the prop's colour-16 parts.
-}
type alias Draw =
    { prop : String -> Mat4 -> Vec4 -> List WebGL.Entity
    , fitted : String -> (PropModel -> Mat4) -> Mat4 -> Vec4 -> List WebGL.Entity
    }


{-| Brick edge lines only on props near the camera.
-}
edgeWidthAt : Ctx -> Vec3 -> Float
edgeWidthAt ctx at =
    clamp 0 1.2 (1.2 * (30 - Vec3.distance ctx.eye at) / 10)


plain : Vec4
plain =
    vec4 1 1 1 1


tint3 : Vec3 -> Vec4
tint3 c =
    vec4 (Vec3.getX c) (Vec3.getY c) (Vec3.getZ c) 1


{-| A harbour: a pier with a pirate flag at its far end, alongside the
approach from the harbour before (or from the start), a pulsing ring and a
spinning beacon over the next harbour to dock at and a grey ring round the
ones already visited. The final harbour also has the pirate arch across the
approach. The piers are decoration: nothing bumps into them.
-}
harbourEntities : Ctx -> Int -> Int -> (String -> Maybe ShipMesh) -> Draw -> Float -> Int -> Course -> Waypoint -> List WebGL.Entity
harbourEntities ctx width height scenery draw time nextIndex course wp =
    let
        p =
            Vec3.toRecord wp.position

        isNext =
            wp.index == nextIndex

        passed =
            wp.index < nextIndex

        bob =
            0.2 * sin (time * 2 + toFloat wp.index) + Physics.seaElevation ctx.sea p.x p.z time

        pulse =
            1 + 0.06 * sin (time * 5)

        ring =
            if isNext then
                ocean ctx (Mat4.makeTranslate3 p.x 0.35 p.z |> Mat4.scale3 (wp.radius * pulse) 1 (wp.radius * pulse)) (vec3 1 0.92 0.3) ringMesh
                    :: draw.fitted "beacon" (BrickProps.fitHeight 5.9) (Mat4.makeTranslate3 p.x (bob + 2.6) p.z |> rotY (time * 1.5)) plain

            else if passed then
                [ ocean ctx (Mat4.makeTranslate3 p.x 0.35 p.z |> Mat4.scale3 wp.radius 1 wp.radius) (vec3 0.55 0.58 0.62) ringMesh ]

            else
                []

        -- Direction the boat sails in from: the harbour before, or the start.
        approach =
            let
                from =
                    course.waypoints
                        |> List.filter (\w -> w.index == wp.index - 1)
                        |> List.head
                        |> Maybe.map .position
                        |> Maybe.withDefault course.startPosition
                        |> Vec3.toRecord
            in
            atan2 (p.x - from.x) (p.z - from.z)

        ( dx, dz ) =
            ( sin approach, cos approach )

        -- The pier (8 m long) runs along the approach, 4 m to one side of
        -- the harbour's centre, its deck just above the water.
        pierX =
            p.x + 4 * dz - 4 * dx

        pierZ =
            p.z - 4 * dx - 4 * dz

        pier =
            sceneryEntities ctx width height scenery Scenery.pier (vec3 pierX -1.5 pierZ) (approach * 180 / pi)
                ++ draw.fitted "pirateFlag"
                    (BrickProps.fitHeight 3.2)
                    (Mat4.makeTranslate3 (pierX + 6.5 * dx) 1.3 (pierZ + 6.5 * dz) |> rotY approach)
                    plain
    in
    case wp.kind of
        FinalHarbour ->
            let
                frame =
                    Mat4.makeTranslate3 p.x -0.5 p.z |> rotY approach

                -- A pirate flag on top of each post of the arch (whose beam
                -- tops out 7.68 m up in arch space).
                flag x =
                    draw.fitted "pirateFlag" (BrickProps.fitHeight 3.2) (Mat4.mul frame (Mat4.makeTranslate3 x 7.68 0)) plain
            in
            List.concatMap
                (\b -> draw.prop b.part (Mat4.mul frame b.transform) (tint3 (BrickLook.colour b.color)))
                (BrickModels.archBricks
                    { halfWidth = wp.radius
                    , postHeight = 6.5
                    , unit = 0.02
                    , postColor = 15
                    , bannerColors = [ 0, 15 ]
                    }
                )
                ++ flag (negate wp.radius)
                ++ flag wp.radius
                ++ pier
                ++ ring

        Harbour ->
            pier ++ ring


targetEntities : Draw -> Float -> Target -> List WebGL.Entity
targetEntities draw time t =
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
        draw.fitted "floatingTarget"
            (BrickProps.fitWidth 1.6)
            (Mat4.makeTranslate3 p.x (p.y - sink) p.z
                |> rotY (time * 0.4 + toFloat t.id)
                |> rotZ (sink * 0.5)
                |> Mat4.translate3 0 -0.4 0
            )
            plain


{-| Islands of round bricks stretched to each island's radius and height
(the radius the boats bump into), with brick palm trees on the grass (clear
of the towns and the treasure) and the fort's watchtower on the first
island's hill.
The beach and the grass are at `Physics.beachHeight` and
`Physics.grassHeight`, where the towns and the chest stand.
-}
islandEntities : Draw -> Course -> List WebGL.Entity
islandEntities draw course =
    List.indexedMap
        (\i isl ->
            let
                grassY =
                    -0.6 + 0.58 * isl.height

                treeCount =
                    3 + modBy 2 i

                -- Compass bearings from the island's centre of its town's
                -- houses and pier and of buried treasure, kept clear of trees.
                occupied =
                    List.filterMap
                        (\town ->
                            if town.island == isl then
                                Just town.bearing

                            else
                                Nothing
                        )
                        course.towns
                        ++ List.filterMap
                            (\t ->
                                let
                                    tp =
                                        Vec3.toRecord t.position
                                in
                                if (tp.x - isl.x) ^ 2 + (tp.z - isl.z) ^ 2 < isl.radius ^ 2 then
                                    Just (180 / pi * atan2 (tp.x - isl.x) (tp.z - isl.z))

                                else
                                    Nothing
                            )
                            course.treasures

                clear k =
                    let
                        -- The tree's angle `a` below, as a compass bearing.
                        bearing =
                            90 - (120 + 40 * toFloat k + 17 * toFloat i)

                        apart b =
                            abs (toFloat (modBy 360 (round (bearing - b) + 540)) - 180)
                    in
                    List.all (\b -> apart b > 35) occupied

                tree k =
                    let
                        a =
                            degrees (120 + 40 * toFloat k + 17 * toFloat i)

                        tall =
                            modBy 2 (k + i) == 0
                    in
                    draw.fitted "palmTree"
                        (BrickProps.fitHeight
                            (if tall then
                                0.9 * isl.height

                             else
                                0.75 * isl.height
                            )
                        )
                        (Mat4.makeTranslate3 (isl.x + 0.7 * isl.radius * cos a) grassY (isl.z + 0.7 * isl.radius * sin a)
                            |> rotY (a + toFloat k)
                        )
                        plain
            in
            draw.fitted "island"
                (BrickProps.fitBox { width = 2 * isl.radius, height = 1.5 * isl.height, depth = 2 * isl.radius })
                (Mat4.makeTranslate3 isl.x (-0.6 - 0.5 * isl.height) isl.z)
                plain
                ++ List.concatMap tree (List.filter clear (List.range 0 (treeCount - 1)))
                ++ (if i == 0 then
                        draw.fitted "fort"
                            (BrickProps.fitHeight 10.6)
                            (Mat4.makeTranslate3 (isl.x + 0.2 * isl.radius) (-0.6 + 0.94 * isl.height) (isl.z + 0.1 * isl.radius))
                            plain

                    else
                        []
                   )
        )
        course.islands
        |> List.concat


{-| The sweeping torch light from the fort's watchtower on island 0.
-}
lighthouseBeamEntity : Ctx -> Float -> Course -> List WebGL.Entity
lighthouseBeamEntity ctx time course =
    case course.islands of
        isl :: _ ->
            let
                model =
                    Mat4.makeTranslate3 (isl.x + 0.2 * isl.radius) (-0.6 + 0.94 * isl.height + 9.5) (isl.z + 0.1 * isl.radius)
                        |> rotY (time * 0.75)
            in
            [ ocean ctx model (vec3 1.0 0.72 0.32) lighthouseBeamMesh ]

        [] ->
            []


{-| Decoration out on the water, clear of the route and the islands: coral
reefs off the outer islands and a pirate wreck run aground far east of the
loop. Visual only: nothing here is in the physics, so the boats sail through
them as if they were not there.
-}
reefEntities : Draw -> List WebGL.Entity
reefEntities draw =
    List.concatMap
        (\( x, z, yaw ) ->
            draw.fitted "reef" (BrickProps.fitHeight 1.8) (Mat4.makeTranslate3 x -0.9 z |> rotY yaw) plain
        )
        [ ( -112, 250, 0.4 ), ( -92, -42, 2.1 ), ( 205, 140, 4.0 ) ]
        ++ draw.fitted "wreck" (BrickProps.fitHeight 4.5) (Mat4.makeTranslate3 110 -1.1 230 |> rotY 0.6) plain


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


{-| A flying cannonball and its trailing broadside smoke puff when in flight.
-}
ballEntity : Draw -> Cannonball -> List WebGL.Entity
ballEntity draw ball =
    let
        p =
            Vec3.toRecord ball.position

        v =
            Vec3.toRecord ball.velocity

        ballEntities =
            draw.fitted "cannonball" (BrickProps.fitHeight 0.6) (Mat4.makeTranslate3 p.x (p.y - 0.3) p.z) plain

        smokeEntities =
            if ball.age < 0.4 then
                let
                    smokeRadius =
                        0.5 + ball.age * 2.2

                    smokeHeight =
                        0.4 + ball.age * 1.6

                    smokeAlpha =
                        clamp 0.0 1.0 (1.0 - ball.age / 0.4)
                in
                draw.fitted "splash"
                    (BrickProps.fitBox { width = 2 * smokeRadius, height = smokeHeight, depth = 2 * smokeRadius })
                    (Mat4.makeTranslate3 (p.x - v.x * 0.08) (p.y - 0.2) (p.z - v.z * 0.08))
                    (vec4 0.92 0.93 0.96 smokeAlpha)

            else
                []
    in
    ballEntities ++ smokeEntities


impactEntity : Draw -> CombatImpact -> List WebGL.Entity
impactEntity draw imp =
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
    draw.fitted "splash"
        (BrickProps.fitBox { width = 2 * r, height = h, depth = 2 * r })
        (Mat4.makeTranslate3 p.x p.y p.z)
        (if imp.isHit then
            vec4 1 0.6 0.15 1

         else
            vec4 1 1 1 1
        )



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


transformTris : Mat4 -> List Tri -> List Tri
transformTris m =
    let
        tv v =
            { v | position = Mat4.transform m v.position, normal = Vec3.normalize (Vec3.sub (Mat4.transform m (Vec3.add v.position v.normal)) (Mat4.transform m v.position)) }
    in
    List.map (\( a, b, c ) -> ( tv a, tv b, tv c ))



-- MESHES


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


arrowMesh : WebGL.Mesh PropVertex
arrowMesh =
    let
        arm sign =
            box ( -0.25, 0, -1.6 ) ( 0.25, 0.25, 0 ) white
                |> transformTris (Mat4.makeRotate (sign * 0.75) (vec3 0 1 0))
    in
    WebGL.triangles
        (arm 1 ++ arm -1 ++ (arm 1 ++ arm -1 |> transformTris (Mat4.makeTranslate3 0 0 -1.4)))


{-| Sweeping torch beam mesh: 2 triangles extending from z = 0 to z = 90.
-}
lighthouseBeamMesh : WebGL.Mesh PropVertex
lighthouseBeamMesh =
    let
        p =
            vec3
    in
    WebGL.triangles
        (face (p -0.5 -0.4 0) (p 0.5 0.4 0) (p 9 1.8 90) (p -9 -1.8 90) white)



-- WEATHER


{-| Spray thrown up at the bow when the boat drives through a rough sea:
small white brick splashes (the `splash` prop).
-}
sprayEntities : Ctx -> Draw -> Float -> RenderBoat -> List WebGL.Entity
sprayEntities ctx draw time rb =
    let
        s =
            rb.state

        p =
            Vec3.toRecord s.position

        rough =
            clamp 0 1 ((ctx.sea - 1.1) / 1.2) * clamp 0 1 ((s.speed - 2) / 5)

        crestRough =
            if ctx.sea > 1.2 then
                max 0.05 (max rough (clamp 0 1 ((ctx.sea - 1.1) / 1.2) * 0.5))

            else
                rough

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
                    (0.4 + phase * 1.4) * crestRough

                h =
                    max 0.05 (sin (phase * pi)) * 1.6 * crestRough
            in
            draw.fitted "splash"
                (BrickProps.fitBox { width = 2 * r, height = h, depth = 2 * r })
                (Mat4.makeTranslate3
                    (p.x + sin rad * 2.3 + side * cos rad * (0.6 + phase))
                    (p.y + 0.2)
                    (p.z + cos rad * 2.3 - side * sin rad * (0.6 + phase))
                )
                plain

        crestPuff i =
            let
                phase =
                    fract (time * 1.6 + toFloat i * 0.37)

                side =
                    if modBy 2 i == 0 then
                        1

                    else
                        -1

                distAlong =
                    0.5 - toFloat (i // 2) * 1.1

                r =
                    (0.35 + phase * 1.1) * crestRough

                h =
                    max 0.04 (sin (phase * pi)) * 1.3 * crestRough
            in
            draw.fitted "splash"
                (BrickProps.fitBox { width = 2 * r, height = h, depth = 2 * r })
                (Mat4.makeTranslate3
                    (p.x + sin rad * distAlong + side * cos rad * (0.8 + phase * 0.4))
                    (p.y + 0.15)
                    (p.z + cos rad * distAlong - side * sin rad * (0.8 + phase * 0.4))
                )
                plain
    in
    if rough > 0.03 || ctx.sea > 1.2 then
        puff 0 ++ puff 1 ++ crestPuff 0 ++ crestPuff 1 ++ crestPuff 2 ++ crestPuff 3

    else
        []


{-| Dynamic LEGO brick foam wake trailing behind a boat's keel when moving.
Emits flattened foam wash tiers along the boat's stern wake wash.
-}
wakeEntities : Ctx -> Draw -> Float -> RenderBoat -> List WebGL.Entity
wakeEntities ctx draw time rb =
    let
        s =
            rb.state

        p =
            Vec3.toRecord s.position

        speed =
            s.speed
    in
    if speed > 0.4 then
        let
            rad =
                degrees s.heading

            tier i =
                let
                    phase =
                        fract (time * 1.3 + toFloat i * 0.33)

                    dist =
                        1.8 + toFloat i * 1.5 + phase * 0.5

                    spread =
                        0.35 + toFloat i * 0.3

                    w =
                        0.8 + toFloat i * 0.5

                    h =
                        0.06

                    wakePuff side =
                        draw.fitted "splash"
                            (BrickProps.fitBox { width = w, height = h, depth = w })
                            (Mat4.makeTranslate3
                                (p.x - sin rad * dist + side * cos rad * spread)
                                (p.y + 0.05)
                                (p.z - cos rad * dist - side * sin rad * spread)
                            )
                            plain
                in
                wakePuff 1 ++ wakePuff -1
        in
        tier 0 ++ tier 1 ++ tier 2

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
                        (vec3 0.88 0.92 0.98)
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
at fixed pseudo-random places, none right at the camera.
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

                z0 =
                    (pick i 3 - 0.5) * 70

                -- Keep the streaks out of the few metres around the camera,
                -- where one would cross the screen like a pole.
                z =
                    if abs x < 9 && abs z0 < 9 then
                        z0 + 18

                    else
                        z0
            in
            box ( x - 0.012, y, z - 0.012 ) ( x + 0.012, y + 0.8, z + 0.012 ) white
    in
    WebGL.triangles (List.concatMap streak (List.range 0 449))


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
