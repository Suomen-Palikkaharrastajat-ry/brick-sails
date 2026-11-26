module Sails.Render exposing
    ( Draw
    , Props
    , RenderBoat
    , ballEntity
    , ghostBoat
    , propSources
    , renderShipyard
    , renderWorld
    , sprayEntities
    )

{-| Draws the sailing world: the map's sea and sky, its islands dressed for
the biome (palms, pines or leafy trees, rocks, farms) with the fort,
lighthouse or cairn on the hub island, the harbour piers, the pirate arch
over the final harbour, the decoration on the water (coral reefs, a wreck,
sea arches), floating targets, cannonballs, the towns and the treasure on
the islands, the toy boats, and the weather (spray, rain or snow, and
lightning).

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

import Array
import Dict exposing (Dict)
import Game.BrickLook as BrickLook
import Game.BrickModels as BrickModels
import Game.BrickProps as BrickProps exposing (PropModel)
import Game.MapParams exposing (MapParams)
import Game.Precipitation as Precipitation
import Html exposing (Html)
import Html.Attributes as Attr
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Render.EdgeShader as EdgeShader
import Render.Instanced as Instanced
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
        , DecorKind(..)
        , Faction(..)
        , HarbourKind(..)
        , Island
        , IslandRole(..)
        , SailingState
        , StandardState(..)
        , Target
        , Town
        , Treasure
        , Waypoint
        , isAtCitadel
        )
import Sails.Weather as Weather exposing (Conditions)
import Sails.World as World exposing (HubLandmark(..), Tree(..))
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


{-| Every prop model the course uses for the given map, by name: load them with
`Game.BrickProps.refresh`. Only the active map's island palette model is
registered so unused island meshes are not built or uploaded.
-}
propSources : MapParams -> List ( String, String )
propSources map =
    let
        palette =
            World.islandPalette map
    in
    [ ( "buoy", BrickModels.buoy )
    , ( "beacon", BrickModels.beacon )
    , ( "floatingTarget", BrickModels.floatingTarget )
    , ( "fort", Scenery.fort )
    , ( "palmTree", Scenery.palmTree )
    , ( "pineTree", BrickModels.pineTree )
    , ( "leafyTree", BrickModels.leafyTree )
    , ( "rock", BrickModels.rock )
    , ( "lighthouse", BrickModels.lighthouse )
    , ( "barn", BrickModels.barn )
    , ( "hayBale", BrickModels.hayBale )
    , ( "reef", Scenery.reef )
    , ( "wreck", Scenery.wreck )
    , ( "pirateFlag", Scenery.pirateFlag )
    , ( "pirateStandard", Scenery.pirateStandard )
    , ( "blueCoatStandard", Scenery.blueCoatStandard )
    , ( "redCoatStandard", Scenery.redCoatStandard )
    , ( "seabird", Scenery.seabird )
    , ( "cannonball", BrickModels.cannonball )
    , ( "splash", BrickModels.splash )
    , ( "plateRound", BrickModels.singlePart "6141.dat" )
    , ( "plateOpenRound", BrickModels.singlePart "85861.dat" )
    , ( "plate1x1", BrickModels.singlePart "3024.dat" )
    , ( "3001.dat", BrickModels.singlePart "3001.dat" )
    , ( "3003.dat", BrickModels.singlePart "3003.dat" )
    , ( "finishArch", finishArchModel )
    , ( palette.key, World.islandModel palette )
    ]


finishArchUnit : Float
finishArchUnit =
    0.02


{-| The finish harbour arch baked into a single BFC-certified LDraw model so
its 55 bricks render in one draw call.
-}
finishArchModel : String
finishArchModel =
    let
        cleanZero v =
            if v == 0 then
                0

            else
                v

        toLine brick =
            let
                pos =
                    Vec3.toRecord (Mat4.transform brick.transform (vec3 0 0 0))
            in
            String.join " "
                [ "1"
                , String.fromInt brick.color
                , String.fromFloat (cleanZero (pos.x / finishArchUnit))
                , String.fromFloat (cleanZero -(pos.y / finishArchUnit))
                , String.fromFloat (cleanZero -(pos.z / finishArchUnit))
                , "1 0 0 0 1 0 0 0 1"
                , brick.part
                ]

        bricks =
            BrickModels.archBricks
                { halfWidth = Physics.harbourRadius
                , postHeight = 6.5
                , unit = finishArchUnit
                , postColor = 15
                , bannerColors = [ 0, 15 ]
                }
    in
    String.join "\n"
        ([ "0 FILE finish-arch.ldr"
         , "0 Name: Brick Sails finish arch"
         , "0 Author: Brick Sails"
         , "0 BFC CERTIFY CCW"
         ]
            ++ List.map toLine bricks
        )


{-| A boat to draw: its brick geometry (`Nothing` while its parts are still
loading), its state, and an optional sail colour override (used to tell
multiplayer boats apart).
-}
type alias RenderBoat =
    { mesh : Maybe ShipMesh
    , state : SailingState
    , sailTint : Maybe Vec3
    , isGhost : Bool
    }


{-| A ghost boat vessel rendered with ethereal glowing trans-green spectral tint.
-}
ghostBoat : Maybe ShipMesh -> SailingState -> RenderBoat
ghostBoat mesh state =
    { mesh = mesh
    , state = state
    , sailTint = Just (vec3 0.18 0.98 0.52)
    , isGhost = True
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
        , shipMeshOf : Wreck.Source -> Maybe ShipMesh
        , standards : List StandardState
        }
    -> Int
    -> Int
    -> Html msg
renderWorld cam scene width height =
    let
        ctx =
            Ocean.makeCtx cam scene.time width height
                |> Ocean.withCourse scene.course
                |> Ocean.withWeather scene.weather

        pv =
            Ocean.propView ctx width height

        frustum =
            Instanced.frustumFromMatrix (Mat4.mul ctx.perspective ctx.view)

        maxViewDist =
            Ocean.visibility ctx

        inView center radius =
            Vec3.distance ctx.eye center
                <= maxViewDist
                + radius
                && Instanced.sphereInFrustum frustum center radius

        drawProp name model tint =
            case Dict.get name scene.props of
                Just prop ->
                    BrickProps.draw pv model tint (edgeWidthAt ctx (Mat4.transform model (vec3 0 0 0))) prop

                Nothing ->
                    []

        withFitWidth edgeWidth name fit at tint =
            case Dict.get name scene.props of
                Just prop ->
                    BrickProps.draw pv (Mat4.mul at (fit prop)) tint edgeWidth prop

                Nothing ->
                    []

        withFit name fit at tint =
            withFitWidth (edgeWidthAt ctx (Mat4.transform at (vec3 0 0 0))) name fit at tint

        drawNoEdges =
            withFitWidth 0

        draw =
            { prop = drawProp, fitted = withFit }

        entities =
            [ Ocean.skyEntity ctx
            , Ocean.oceanEntity ctx
            ]
                ++ islandEntities inView drawNoEdges draw scene.course
                ++ decorEntities inView ctx width height scene.scenery draw scene.course
                ++ seabirdFlockEntities inView draw scene.time scene.course
                ++ lighthouseBeamEntity ctx scene.time scene.course
                ++ townEntities inView ctx width height scene.scenery draw scene.standards scene.course.towns
                ++ treasureEntities inView ctx width height scene.scenery scene.time scene.treasureFound scene.course.treasures
                ++ List.concatMap (harbourEntities inView ctx width height scene.scenery draw scene.time scene.nextIndex scene.course) scene.course.waypoints
                ++ List.concatMap (targetEntities inView draw scene.time) scene.targets
                ++ List.concatMap (boatEntities ctx width height draw) scene.boats
                ++ List.concatMap (standardAtSeaEntities ctx draw scene.time) scene.standards
                ++ List.concatMap (debrisEntities ctx width height draw scene.shipMeshOf) scene.debris
                ++ List.concatMap (ballEntity draw) scene.balls
                ++ List.concatMap (impactEntity draw) scene.impacts
                ++ List.concatMap (sprayEntities ctx draw scene.time) (List.take 1 scene.boats)
                ++ List.concatMap (wakeEntities ctx draw scene.time) scene.boats
                ++ boltEntities ctx scene.weather
                ++ rainEntities ctx scene.time scene.weather
    in
    canvas width height entities


{-| One loose piece of a wrecked boat (see `Sails.Wreck`): one of the
boat's own parts, drawn as it was built (`Sails.ShipMesh.parts`), or a
generic brick in its colour, at about the scale of the boats' own bricks.
-}
debrisEntities : Ctx -> Int -> Int -> Draw -> (Wreck.Source -> Maybe ShipMesh) -> Wreck.Piece -> List WebGL.Entity
debrisEntities ctx width height draw shipMeshOf piece =
    let
        p =
            Vec3.toRecord piece.position

        u =
            0.035
    in
    case piece.look of
        Wreck.Brick part colour ->
            let
                brickColor =
                    if colour == 20 then
                        vec3 0.18 0.98 0.52

                    else
                        BrickLook.colour colour
            in
            draw.prop part
                (Mat4.makeTranslate3 p.x p.y p.z
                    |> rotY piece.yaw
                    |> rotX piece.tumble
                    |> Mat4.translate3 0 (12 * u) 0
                    |> Mat4.scale3 u u u
                )
                (tint3 brickColor)

        Wreck.ShipPart source slot ->
            case shipMeshOf source of
                Just mesh ->
                    if Array.isEmpty mesh.parts then
                        []

                    else
                        case Array.get (modBy (Array.length mesh.parts) slot) mesh.parts of
                            Just part ->
                                let
                                    pivot =
                                        Vec3.toRecord part.pivot

                                    -- The part turns about its own centre, which
                                    -- is where the piece is; `linear` takes it to
                                    -- boat metres, as the whole boat is drawn.
                                    pose =
                                        Mat4.makeTranslate3 p.x p.y p.z
                                            |> rotY piece.yaw
                                            |> rotX piece.tumble

                                    model =
                                        Mat4.mul pose mesh.linear
                                            |> Mat4.translate3 (negate pivot.x) (negate pivot.y) (negate pivot.z)
                                in
                                brickPasses ctx width height (Vec3.distance ctx.eye piece.position) (vec4 1 1 1 0) model part.scene

                            Nothing ->
                                []

                Nothing ->
                    []


{-| Shipyard preview: one boat on the water of the map's sea colours
(`Ocean.withMap`), seen from the side, slowly circling. Shows only the sea
while the boat's parts are loading.
-}
renderShipyard : MapParams -> Maybe ShipMesh -> Float -> Int -> Int -> Html msg
renderShipyard map mesh time width height =
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
                |> Ocean.withMap map

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
            , carriedStandard = Nothing
            }

        dummyDraw =
            { prop = \_ _ _ -> []
            , fitted = \_ _ _ _ -> []
            }
    in
    canvas width
        height
        ([ Ocean.skyEntity ctx, Ocean.oceanEntity ctx ]
            ++ boatEntities ctx width height dummyDraw { mesh = mesh, state = state, sailTint = Nothing, isGhost = False }
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


{-| Draw one brick boat: the hull and rig, the sails hoisted as far as
the boat's sail setting, and any captured faction standard hoisted banner.
-}
boatEntities : Ctx -> Int -> Int -> Draw -> RenderBoat -> List WebGL.Entity
boatEntities ctx width height draw rb =
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

                hoistedBanner =
                    case s.carriedStandard of
                        Just faction ->
                            let
                                propName =
                                    case faction of
                                        Pirates ->
                                            "pirateStandard"

                                        BlueCoats ->
                                            "blueCoatStandard"

                                        RedCoats ->
                                            "redCoatStandard"

                                bannerModel =
                                    Mat4.mul world
                                        (Mat4.makeTranslate3 0 0.8 -1.2
                                            |> Mat4.scale3 0.6 0.6 0.6
                                        )
                            in
                            draw.fitted propName (BrickProps.fitHeight 1.8) bannerModel plain

                        Nothing ->
                            []
            in
            if rb.isGhost then
                ghostBrickPasses ctx width height distance (vec4 0.18 0.98 0.52 1) model mesh.scene
                    ++ ghostBrickPasses ctx width height distance (vec4 0.18 0.98 0.52 1) sailModel mesh.sails

            else
                brickPasses ctx width height distance tint model mesh.scene
                    ++ brickPasses ctx width height distance tint sailModel mesh.sails
                    ++ hoistedBanner


{-| Draw the phantom ghost vessel: ethereal glowing trans-green spectral tint,
semi-transparent ghost pass, glowing rim, and spectral green edges.
-}
ghostBrickPasses : Ctx -> Int -> Int -> Float -> Vec4 -> Mat4 -> Scene -> List WebGL.Entity
ghostBrickPasses ctx width height distance tint model scene =
    let
        surface =
            WebGL.entityWith [ DepthTest.default, Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
                ghostVertexShader
                Shader.fragmentShader
                scene.triangleMesh
                { modelMatrix = model
                , viewMatrix = ctx.view
                , projectionMatrix = ctx.perspective
                , viewPosition = ctx.eye
                , lightDirection = Ocean.sunDirection
                , instanceColor = tint
                , ambientStrength = 0.85
                , lightStrength = 0.55
                , specularStrength = 0.3
                , specularPower = 16
                , rimStrength = 0.85
                , rimPower = 1.8
                , vibrance = 0.4
                , metallic = 0
                , u_ghostMode = 1.0
                , u_ghostAlpha = 0.75
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
                    , edgeColor = vec3 0.18 0.98 0.52
                    , viewportWidth = toFloat width
                    , viewportHeight = toFloat height
                    , lineWidth = edgeWidth
                    }
                ]

            else
                []
    in
    surface :: edges


{-| Ghost vessel vertex shader: tints all hull and sail bricks with the ethereal
spectral green tint.
-}
ghostVertexShader : WebGL.Shader Vertex Shader.Uniforms { vColor : Vec4, vNormalWorld : Vec3, vPositionWorld : Vec3 }
ghostVertexShader =
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
            vColor = vec4(mix(color.rgb, instanceColor.rgb, 0.8), 0.75);
            vNormalWorld = mat3(modelMatrix) * normal;
            vPositionWorld = worldPosition.xyz;
        }
    |]


{-| Draw one brick scene: the plastic surface pass, then the brick edges
while it is near enough for them to read as edges rather than a dark smear.
Storm gloom dims the light and lightning flashes it.
-}
brickPasses : Ctx -> Int -> Int -> Float -> Vec4 -> Mat4 -> Scene -> List WebGL.Entity
brickPasses ctx width height distance tint model scene =
    let
        surface =
            WebGL.entityWith (BrickProps.surfaceSettings scene.bfcCertified model)
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


{-| A harbour: a pier from the shore of the island it docks on out towards
the harbour, with a pirate flag where it leaves the beach, a red and a
green channel beacon either side of the approach, a pulsing ring and a
spinning beacon over the next harbour to dock at and a grey ring round the
ones already visited. The final harbour also has the pirate arch across the
approach. The piers are decoration: nothing bumps into them.
-}
harbourEntities : (Vec3 -> Float -> Bool) -> Ctx -> Int -> Int -> (String -> Maybe ShipMesh) -> Draw -> Float -> Int -> Course -> Waypoint -> List WebGL.Entity
harbourEntities inView ctx width height scenery draw time nextIndex course wp =
    let
        p =
            Vec3.toRecord wp.position
    in
    if not (inView (vec3 p.x 4 p.z) (wp.radius + 25)) then
        []

    else
        let
            isNext =
                wp.index == nextIndex

            passed =
                wp.index < nextIndex

            bob =
                0.2 * sin (time * 2 + toFloat wp.index) + Physics.seaElevation ctx.sea p.x p.z time

            pulse =
                1 + 0.06 * sin (time * 5)

            ring =
                if (isNext || passed) && inView (vec3 p.x 2.0 p.z) (wp.radius * 1.1 + 4.0) then
                    if isNext then
                        ocean ctx (Mat4.makeTranslate3 p.x 0.35 p.z |> Mat4.scale3 (wp.radius * pulse) 1 (wp.radius * pulse)) (vec3 1 0.92 0.3) ringMesh
                            :: draw.fitted "beacon" (BrickProps.fitHeight 5.9) (Mat4.makeTranslate3 p.x (bob + 2.6) p.z |> rotY (time * 1.5)) plain

                    else
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

            -- The pier runs from the beach of the island the harbour docks on
            -- (the one with the nearest shore) out towards the harbour, like a
            -- town's pier, with a pirate flag where it leaves the beach.
            pier =
                case dockIsland course.islands p of
                    Just isl ->
                        let
                            bearing =
                                180 / pi * atan2 (p.x - isl.x) (p.z - isl.z)

                            edge =
                                Vec3.toRecord (Physics.shorePoint isl bearing 1)

                            flagPos =
                                vec3 edge.x (max 0.48 (Physics.beachHeight isl)) edge.z

                            flagProp =
                                course.towns
                                    |> List.filter (\t -> t.island == isl)
                                    |> List.filterMap .faction
                                    |> List.head
                                    |> Maybe.map
                                        (\faction ->
                                            case faction of
                                                Pirates ->
                                                    "pirateStandard"

                                                BlueCoats ->
                                                    "blueCoatStandard"

                                                RedCoats ->
                                                    "redCoatStandard"
                                        )
                                    |> Maybe.withDefault "pirateFlag"

                            flagEntities =
                                if inView (Vec3.add flagPos (vec3 0 1.6 0)) 2.5 then
                                    draw.fitted flagProp
                                        (BrickProps.fitHeight 3.2)
                                        (Mat4.makeTranslate flagPos |> rotY (degrees bearing))
                                        plain

                                else
                                    []
                        in
                        pierInView inView ctx width height scenery isl bearing ++ flagEntities

                    Nothing ->
                        []

            -- The channel beacons stand 12 m out along the approach, 6 m to
            -- either side of it, in the open water off the shore.
            portX =
                p.x - 12 * dx + 6 * dz

            portZ =
                p.z - 12 * dz - 6 * dx

            starboardX =
                p.x - 12 * dx - 6 * dz

            starboardZ =
                p.z - 12 * dz + 6 * dx

            channelBeacons =
                (if inView (vec3 portX 0.35 portZ) 1.5 then
                    [ ocean ctx (Mat4.makeTranslate3 portX 0.35 portZ |> Mat4.scale3 0.8 1 0.8) (vec3 0.95 0.15 0.15) ringMesh ]

                 else
                    []
                )
                    ++ (if inView (vec3 starboardX 0.35 starboardZ) 1.5 then
                            [ ocean ctx (Mat4.makeTranslate3 starboardX 0.35 starboardZ |> Mat4.scale3 0.8 1 0.8) (vec3 0.15 0.9 0.2) ringMesh ]

                        else
                            []
                       )
        in
        case wp.kind of
            FinalHarbour ->
                let
                    archEntities =
                        if inView (vec3 p.x 4.5 p.z) (wp.radius + 5.0) then
                            let
                                frame =
                                    Mat4.makeTranslate3 p.x -0.5 p.z |> rotY approach

                                -- A pirate flag on top of each post of the arch (whose beam
                                -- tops out 7.68 m up in arch space).
                                flag x =
                                    draw.fitted "pirateFlag" (BrickProps.fitHeight 3.2) (Mat4.mul frame (Mat4.makeTranslate3 x 7.68 0)) plain

                                arch =
                                    if wp.radius == Physics.harbourRadius then
                                        draw.prop "finishArch" (Mat4.mul frame (Mat4.makeScale3 finishArchUnit finishArchUnit finishArchUnit)) plain

                                    else
                                        List.concatMap
                                            (\b -> draw.prop b.part (Mat4.mul frame b.transform) (tint3 (BrickLook.colour b.color)))
                                            (BrickModels.archBricks
                                                { halfWidth = wp.radius
                                                , postHeight = 6.5
                                                , unit = finishArchUnit
                                                , postColor = 15
                                                , bannerColors = [ 0, 15 ]
                                                }
                                            )
                            in
                            arch ++ flag (negate wp.radius) ++ flag wp.radius

                        else
                            []
                in
                archEntities ++ pier ++ ring ++ channelBeacons

            Harbour ->
                pier ++ ring ++ channelBeacons


{-| The island a harbour at `p` docks at: the one whose shore is nearest.
-}
dockIsland : List Island -> { x : Float, y : Float, z : Float } -> Maybe Island
dockIsland islands p =
    islands
        |> List.sortBy (\isl -> sqrt ((p.x - isl.x) ^ 2 + (p.z - isl.z) ^ 2) - isl.radius)
        |> List.head


targetEntities : (Vec3 -> Float -> Bool) -> Draw -> Float -> Target -> List WebGL.Entity
targetEntities inView draw time t =
    if t.sunk && t.sinkAge > 1.6 then
        []

    else
        let
            p =
                Vec3.toRecord t.position
        in
        if not (inView (vec3 p.x (p.y + 0.5) p.z) 2.5) then
            []

        else
            let
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


{-| Islands of round bricks in the map's colours (`Sails.World.islandPalette`)
stretched to each island's radius and height (the radius the boats bump
into), dressed for the biome (`Sails.World.dressing`): trees on the grass
(clear of the towns, the harbour piers and the treasure), rocks, a barn and
hay on farm islands, and the fort, lighthouse or cairn on the hub. Skerries
are bare rock.
The beach and the grass are at `Physics.beachHeight` and
`Physics.grassHeight`, where the towns and the chest stand.
-}
islandEntities :
    (Vec3 -> Float -> Bool)
    -> (String -> (PropModel -> Mat4) -> Mat4 -> Vec4 -> List WebGL.Entity)
    -> Draw
    -> Course
    -> List WebGL.Entity
islandEntities inView drawNoEdges draw course =
    let
        look =
            World.dressing course.map

        palette =
            World.islandPalette course.map

        treeProp =
            case look.tree of
                Palm ->
                    Just "palmTree"

                Pine ->
                    Just "pineTree"

                Leafy ->
                    Just "leafyTree"

                NoTree ->
                    Nothing
    in
    List.indexedMap
        (\i isl ->
            if not (inView (vec3 isl.x 6 isl.z) (max 28 (isl.radius + 14))) then
                []

            else
                let
                    grassY =
                        -0.6 + 0.58 * isl.height

                    treeCount =
                        case isl.role of
                            Skerry ->
                                0

                            TreasureIsle ->
                                2

                            _ ->
                                if look.landmark == Cairn then
                                    1 + modBy 2 i

                                else
                                    3 + modBy 2 i

                    -- Compass bearings from the island's centre of its town's
                    -- houses and pier, of the piers of harbours docking at it and
                    -- of buried treasure, kept clear of trees.
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
                            ++ List.filterMap
                                (\wp ->
                                    let
                                        wpp =
                                            Vec3.toRecord wp.position
                                    in
                                    if dockIsland course.islands wpp == Just isl then
                                        Just (180 / pi * atan2 (wpp.x - isl.x) (wpp.z - isl.z))

                                    else
                                        Nothing
                                )
                                course.waypoints

                    -- The angle `a` of slot k (as used below), as a compass bearing.
                    slotAngle k =
                        120 + 40 * toFloat k + 17 * toFloat i

                    clear k =
                        let
                            bearing =
                                90 - slotAngle k

                            apart b =
                                abs (toFloat (modBy 360 (round (bearing - b) + 540)) - 180)
                        in
                        List.all (\b -> apart b > 48) occupied

                    grassPoint k share =
                        let
                            a =
                                degrees (slotAngle k)
                        in
                        vec3 (isl.x + share * isl.radius * cos a) grassY (isl.z + share * isl.radius * sin a)

                    onGrass k share =
                        let
                            a =
                                degrees (slotAngle k)
                        in
                        Mat4.makeTranslate (grassPoint k share)
                            |> rotY (a + toFloat k)

                    tree prop k =
                        let
                            tall =
                                modBy 2 (k + i) == 0

                            height =
                                min 9
                                    (if tall then
                                        0.9 * isl.height

                                     else
                                        0.75 * isl.height
                                    )
                        in
                        if inView (Vec3.add (grassPoint k 0.7) (vec3 0 (0.5 * height) 0)) 6.5 then
                            draw.fitted prop
                                (BrickProps.fitHeight height)
                                (onGrass k 0.7)
                                plain

                        else
                            []

                    trees =
                        case treeProp of
                            Just prop ->
                                List.concatMap (tree prop) (List.filter clear (List.range 0 (treeCount - 1)))

                            Nothing ->
                                []

                    -- Slots 5 and 6 (past the trees) for rocks and farm buildings.
                    extras =
                        case isl.role of
                            HarbourIsle ->
                                (if look.rocks && clear 5 && inView (Vec3.add (grassPoint 5 0.55) (vec3 0 1.2 0)) 3.5 then
                                    draw.fitted "rock" (BrickProps.fitHeight 2.4) (onGrass 5 0.55) plain

                                 else
                                    []
                                )
                                    ++ (if look.farm && clear 6 then
                                            (if inView (Vec3.add (grassPoint 6 0.45) (vec3 0 2.25 0)) 5.5 then
                                                draw.fitted "barn" (BrickProps.fitHeight 4.5) (onGrass 6 0.45) plain

                                             else
                                                []
                                            )
                                                ++ (if inView (Vec3.add (grassPoint 6 0.75) (vec3 0 0.6 0)) 2.5 then
                                                        draw.fitted "hayBale" (BrickProps.fitHeight 1.2) (onGrass 6 0.75) plain

                                                    else
                                                        []
                                                   )

                                        else
                                            []
                                       )

                            _ ->
                                []

                    landmark =
                        if isl.role == Hub then
                            let
                                pos =
                                    landmarkPos isl

                                at =
                                    Mat4.makeTranslate pos
                            in
                            case look.landmark of
                                Fort ->
                                    if inView (Vec3.add pos (vec3 0 5.3 0)) 8.0 then
                                        draw.fitted "fort" (BrickProps.fitHeight 10.6) at plain

                                    else
                                        []

                                Lighthouse ->
                                    if inView (Vec3.add pos (vec3 0 5.3 0)) 8.0 then
                                        draw.fitted "lighthouse" (BrickProps.fitHeight 10.6) at plain

                                    else
                                        []

                                Cairn ->
                                    if inView (Vec3.add pos (vec3 0 3.0 0)) 5.0 then
                                        List.concatMap
                                            (\k ->
                                                draw.fitted "rock"
                                                    (BrickProps.fitHeight (2.4 - 0.5 * toFloat k))
                                                    (Mat4.mul at (Mat4.makeTranslate3 0 (1.9 * toFloat k) 0) |> rotY (toFloat k))
                                                    plain
                                            )
                                            [ 0, 1, 2 ]

                                    else
                                        []

                        else
                            []

                    islandBase =
                        if inView (vec3 isl.x 1.5 isl.z) 27 then
                            drawNoEdges palette.key
                                (\prop ->
                                    let
                                        h =
                                            max 0.001 (Vec3.getY (BrickProps.size prop))
                                    in
                                    BrickProps.fitHeight (0.035 * h) prop
                                )
                                (Mat4.makeTranslate3 isl.x -0.84 isl.z)
                                plain

                        else
                            []
                in
                islandBase
                    ++ trees
                    ++ extras
                    ++ landmark
        )
        course.islands
        |> List.concat


{-| The hub island of a course, where the landmark, its beam and the
seabirds are.
-}
hubOf : Course -> Maybe Island
hubOf course =
    course.islands
        |> List.filter (\isl -> isl.role == Hub)
        |> List.head


{-| Where the hub's landmark stands, on its hill.
-}
landmarkPos : Island -> Vec3
landmarkPos isl =
    vec3 (isl.x + 0.2 * isl.radius) (-0.6 + 0.94 * isl.height) (isl.z + 0.1 * isl.radius)


{-| The sweeping light from the fort's watchtower or the lighthouse on the
hub island (a cairn has none).
-}
lighthouseBeamEntity : Ctx -> Float -> Course -> List WebGL.Entity
lighthouseBeamEntity ctx time course =
    case ( hubOf course, (World.dressing course.map).landmark ) of
        ( Just _, Cairn ) ->
            []

        ( Just isl, _ ) ->
            let
                model =
                    Mat4.makeTranslate3 (isl.x + 0.2 * isl.radius) (-0.6 + 0.94 * isl.height + 9.5) (isl.z + 0.1 * isl.radius)
                        |> rotY (time * 0.75)
            in
            [ ocean ctx model (vec3 1.0 0.72 0.32) lighthouseBeamMesh ]

        ( Nothing, _ ) ->
            []


{-| Decoration out on the water, clear of the route and the islands
(`Course.decor`): coral reefs, a wreck run aground and rock sea arches.
Only the reefs are in the physics (deep keels ground on them, see
`Physics.checkReefGrounding`); the boats sail through the rest as if they
were not there.
-}
decorEntities : (Vec3 -> Float -> Bool) -> Ctx -> Int -> Int -> (String -> Maybe ShipMesh) -> Draw -> Course -> List WebGL.Entity
decorEntities inView ctx width height scenery draw course =
    List.concatMap
        (\d ->
            case d.kind of
                Reef ->
                    if inView (vec3 d.x 0.0 d.z) 3.5 then
                        draw.fitted "reef" (BrickProps.fitHeight 1.8) (Mat4.makeTranslate3 d.x -0.9 d.z |> rotY d.yaw) plain

                    else
                        []

                Wreck ->
                    if inView (vec3 d.x 1.2 d.z) 5.0 then
                        draw.fitted "wreck" (BrickProps.fitHeight 4.5) (Mat4.makeTranslate3 d.x -1.1 d.z |> rotY d.yaw) plain

                    else
                        []

                SeaArch ->
                    if inView (vec3 d.x 2.1 d.z) 6.0 then
                        sceneryEntities ctx width height scenery Scenery.seaArch (vec3 d.x -1.0 d.z) (d.yaw * 180 / pi)

                    else
                        []
        )
        course.decor


{-| A flock of tiny brick gulls wheeling on thermal updrafts high above the hub island.
-}
seabirdFlockEntities : (Vec3 -> Float -> Bool) -> Draw -> Float -> Course -> List WebGL.Entity
seabirdFlockEntities inView draw time course =
    case hubOf course of
        Just isl ->
            let
                centerX =
                    isl.x + 0.2 * isl.radius

                centerZ =
                    isl.z + 0.1 * isl.radius

                baseY =
                    isl.height + 14.0
            in
            if not (inView (vec3 centerX baseY centerZ) 24.0) then
                []

            else
                let
                    bird i =
                        let
                            phase =
                                toFloat i * (2.0 * pi / 5.0)

                            orbitRadius =
                                18.0 + 3.0 * sin (toFloat i * 1.7)

                            ang =
                                time * 0.45 + phase

                            x =
                                centerX + orbitRadius * cos ang

                            z =
                                centerZ + orbitRadius * sin ang

                            y =
                                baseY + 2.0 * sin (time * 1.2 + toFloat i)

                            yaw =
                                -ang + (pi / 2.0)

                            model =
                                Mat4.makeTranslate3 x y z
                                    |> rotY yaw
                        in
                        if inView (vec3 x y z) 1.5 then
                            draw.fitted "seabird"
                                (BrickProps.fitBox { width = 1.2, height = 0.4, depth = 0.8 })
                                model
                                plain

                        else
                            []
                in
                List.concatMap bird (List.range 0 4)

        Nothing ->
            []


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


{-| Render a shore pier if its bounding sphere is inside the view frustum and
visibility range. Shared by `townEntities` and `harbourEntities`.
-}
pierInView : (Vec3 -> Float -> Bool) -> Ctx -> Int -> Int -> (String -> Maybe ShipMesh) -> Island -> Float -> List WebGL.Entity
pierInView inView ctx width height scenery isl bearing =
    let
        pierPos =
            Physics.shorePoint isl bearing (1 - 5 / isl.radius) |> Vec3.setY -1.4

        rad =
            degrees bearing

        center =
            Vec3.add pierPos (vec3 (5 * sin rad) 0.3 (5 * cos rad))
    in
    if inView center 8.0 then
        sceneryEntities ctx width height scenery Scenery.pier pierPos bearing

    else
        []


{-| Each town: its houses on the grass, its pier from the beach out to
the water's edge, and the faction standard at the end of the pier on the
island shore (drawn only if still safe at the citadel).
-}
townEntities : (Vec3 -> Float -> Bool) -> Ctx -> Int -> Int -> (String -> Maybe ShipMesh) -> Draw -> List StandardState -> List Town -> List WebGL.Entity
townEntities inView ctx width height scenery draw standards towns =
    List.concatMap
        (\town ->
            let
                isl =
                    town.island

                edge =
                    Vec3.toRecord (Physics.shorePoint isl town.bearing 1)

                housesPos =
                    Physics.shorePoint isl town.bearing 0.6 |> Vec3.setY (Physics.grassHeight isl)

                housesEntities =
                    if inView (Vec3.add housesPos (vec3 0 3.5 0)) 9.5 then
                        sceneryEntities ctx width height scenery Scenery.houses housesPos town.bearing

                    else
                        []

                standardProp =
                    case town.faction of
                        Just f ->
                            if List.any (isAtCitadel f) standards then
                                case f of
                                    Pirates ->
                                        Just "pirateStandard"

                                    BlueCoats ->
                                        Just "blueCoatStandard"

                                    RedCoats ->
                                        Just "redCoatStandard"

                            else
                                Nothing

                        Nothing ->
                            Nothing

                standardEntities =
                    case standardProp of
                        Just prop ->
                            let
                                stdY =
                                    Physics.beachHeight isl
                            in
                            if inView (vec3 edge.x (stdY + 1.6) edge.z) 2.5 then
                                draw.fitted prop
                                    (BrickProps.fitHeight 3.2)
                                    (Mat4.makeTranslate3 edge.x stdY edge.z |> rotY (degrees town.bearing))
                                    plain

                            else
                                []

                        Nothing ->
                            []
            in
            housesEntities
                ++ pierInView inView ctx width height scenery isl town.bearing
                ++ standardEntities
        )
        towns


{-| A faction standard lost at sea: bobbing in the waves with a golden beacon glow ring.
-}
standardAtSeaEntities : Ctx -> Draw -> Float -> StandardState -> List WebGL.Entity
standardAtSeaEntities ctx draw time std =
    case std of
        AtSea faction pos age ->
            let
                p =
                    Vec3.toRecord pos

                bob =
                    Physics.seaElevation ctx.sea p.x p.z time + 0.3 * sin (age * 3.5)

                pulse =
                    4.5 * (1 + 0.08 * sin (age * 5))

                ring =
                    ocean ctx (Mat4.makeTranslate3 p.x 0.35 p.z |> Mat4.scale3 pulse 1 pulse) (vec3 1 0.84 0.15) ringMesh

                propName =
                    case faction of
                        Pirates ->
                            "pirateStandard"

                        BlueCoats ->
                            "blueCoatStandard"

                        RedCoats ->
                            "redCoatStandard"

                propMatrix =
                    Mat4.makeTranslate3 p.x (bob + 1.2) p.z
                        |> rotY (age * 1.8)
                        |> rotX (degrees (6 * sin (age * 2.5)))
                        |> rotZ (degrees (5 * cos (age * 2.0)))
            in
            ring :: draw.fitted propName (BrickProps.fitHeight 2.4) propMatrix plain

        _ ->
            []


{-| Treasure still to find: the chest, and a gold ring on the water showing
how close to sail to take it aboard.
-}
treasureEntities : (Vec3 -> Float -> Bool) -> Ctx -> Int -> Int -> (String -> Maybe ShipMesh) -> Float -> List Int -> List Treasure -> List WebGL.Entity
treasureEntities inView ctx width height scenery time found treasures =
    treasures
        |> List.filter (\t -> not (List.member t.id found))
        |> List.concatMap
            (\t ->
                if inView (Vec3.add t.position (vec3 0 1.5 0)) (Physics.treasureReach + 2.0) then
                    let
                        p =
                            Vec3.toRecord t.position

                        r =
                            Physics.treasureReach * (1 + 0.05 * sin (time * 4))
                    in
                    ocean ctx (Mat4.makeTranslate3 p.x 0.4 p.z |> Mat4.scale3 r 1 r) (vec3 1 0.82 0.2) ringMesh
                        :: sceneryEntities ctx width height scenery Scenery.chest t.position t.bearing

                else
                    []
            )


goalArrowEntities : Ctx -> Draw -> Float -> List RenderBoat -> Maybe { from : Vec3, to : Vec3 } -> List StandardState -> List Target -> List WebGL.Entity
goalArrowEntities ctx draw time boats guide standards targets =
    case List.head boats of
        Just playerBoat ->
            let
                p =
                    Vec3.toRecord playerBoat.state.position

                bob =
                    sin (time * 3.2) * 0.03

                drawIndicator ( goalX, goalZ ) mesh radius =
                    let
                        dx =
                            goalX - p.x

                        dz =
                            goalZ - p.z

                        dist =
                            sqrt (dx * dx + dz * dz)
                    in
                    if dist > 8.0 then
                        let
                            angle =
                                atan2 dx dz

                            ax =
                                p.x + sin angle * radius

                            az =
                                p.z + cos angle * radius

                            mat =
                                Mat4.makeTranslate3 ax (0.12 + bob) az
                                    |> Mat4.mul (Mat4.makeRotate angle (vec3 0 1 0))
                        in
                        [ ocean ctx mat (vec3 1 1 1) mesh ]

                    else
                        []

                -- 1. Next Waypoint / Harbour (Emerald Green Portal, r = 2.1)
                harbourArrow =
                    case guide of
                        Just g ->
                            let
                                b =
                                    Vec3.toRecord g.to
                            in
                            drawIndicator ( b.x, b.z ) harbourIndicatorMesh 2.1

                        Nothing ->
                            []

                -- 2. Nearest Lost Standard at Sea (Golden Star, r = 2.3)
                nearestStandard =
                    standards
                        |> List.filterMap
                            (\s ->
                                case s of
                                    AtSea _ spos _ ->
                                        Just ( Physics.horizontalDistance playerBoat.state.position spos, spos )

                                    _ ->
                                        Nothing
                            )
                        |> List.sortBy Tuple.first
                        |> List.head

                standardArrow =
                    case nearestStandard of
                        Just ( d, spos ) ->
                            if d < 350 then
                                let
                                    sr =
                                        Vec3.toRecord spos
                                in
                                drawIndicator ( sr.x, sr.z ) standardIndicatorMesh 2.3

                            else
                                []

                        Nothing ->
                            []

                -- 3. Nearest Target / Enemy (Crimson Crosshair, r = 2.5)
                nearestTarget =
                    targets
                        |> List.map (\t -> ( Physics.horizontalDistance playerBoat.state.position t.position, t.position ))
                        |> List.sortBy Tuple.first
                        |> List.head

                targetArrow =
                    case nearestTarget of
                        Just ( d, tpos ) ->
                            if d < 180 then
                                let
                                    tr =
                                        Vec3.toRecord tpos
                                in
                                drawIndicator ( tr.x, tr.z ) targetIndicatorMesh 2.5

                            else
                                []

                        Nothing ->
                            []

                -- 4. Ghost Ship (Spectral Cyan Wings, r = 2.7)
                ghostArrow =
                    case List.filter .isGhost (List.drop 1 boats) |> List.head of
                        Just ghost ->
                            let
                                d =
                                    Physics.horizontalDistance playerBoat.state.position ghost.state.position
                            in
                            if d < 400 && d > 8 then
                                let
                                    gr =
                                        Vec3.toRecord ghost.state.position
                                in
                                drawIndicator ( gr.x, gr.z ) ghostIndicatorMesh 2.7

                            else
                                []

                        Nothing ->
                            []
            in
            harbourArrow ++ standardArrow ++ targetArrow ++ ghostArrow

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
                    phase =
                        ball.age / 0.4

                    smokeRadius =
                        0.3 + phase * 0.6

                    smokeAlpha =
                        clamp 0.0 1.0 (1.0 - phase)
                in
                draw.fitted "plateOpenRound"
                    (BrickProps.fitWidth smokeRadius)
                    (Mat4.makeTranslate3 (p.x - v.x * 0.06) (p.y - 0.1) (p.z - v.z * 0.06))
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
            clamp 0 1 (imp.age / 0.6)

        splashR =
            0.5 + t * 1.8

        splashH =
            0.1 + sin (t * pi) * 1.2

        alpha =
            1.0 - t

        puff ringI =
            let
                angle =
                    toFloat ringI * (2 * pi / 6)

                px =
                    p.x + cos angle * splashR

                pz =
                    p.z + sin angle * splashR
            in
            draw.fitted "plateRound"
                (BrickProps.fitWidth 0.5)
                (Mat4.makeTranslate3 px (p.y + splashH) pz)
                (if imp.isHit then
                    vec4 1.0 0.6 0.1 alpha

                 else
                    vec4 0.9 0.96 1.0 alpha
                )
    in
    List.concatMap puff (List.range 0 5)



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


{-| Navigational indicator chevron arm pointing forward along +Z.
-}
chevronArm : Float -> Vec3 -> List Tri
chevronArm sign col =
    box ( -0.06, 0.0, -0.55 ) ( 0.06, 0.12, 0.0 ) col
        |> transformTris
            (Mat4.makeTranslate3 0 0 0.75
                |> Mat4.mul (Mat4.makeRotate (sign * 0.65) (vec3 0 1 0))
            )


chevronSideFin : Float -> Vec3 -> List Tri
chevronSideFin sign col =
    box ( -0.05, 0.0, -0.4 ) ( 0.05, 0.1, 0.0 ) col
        |> transformTris (Mat4.makeTranslate3 (sign * 0.33) 0 0.31)


badgeBase : Vec3 -> List Tri
badgeBase col =
    box ( -0.22, 0.0, -0.25 ) ( 0.22, 0.08, 0.2 ) (Vec3.scale 0.55 col)


indicatorMesh : Vec3 -> List Tri -> WebGL.Mesh PropVertex
indicatorMesh col iconTris =
    let
        chevron =
            chevronArm 1 col
                ++ chevronArm -1 col
                ++ chevronSideFin 1 (Vec3.scale 0.85 col)
                ++ chevronSideFin -1 (Vec3.scale 0.85 col)
                ++ badgeBase col
                ++ iconTris
    in
    WebGL.triangles chevron


{-| Harbour / Waypoint indicator: Emerald green with port gateway beacon icon.
-}
harbourIndicatorMesh : WebGL.Mesh PropVertex
harbourIndicatorMesh =
    let
        postX =
            0.11

        icon =
            List.concat
                [ box ( -postX - 0.03, 0.08, -0.15 ) ( -postX + 0.03, 0.18, 0.12 ) white
                , box ( postX - 0.03, 0.08, -0.15 ) ( postX + 0.03, 0.18, 0.12 ) white
                , box ( -postX - 0.04, 0.16, -0.04 ) ( postX + 0.04, 0.21, 0.04 ) white
                , box ( -0.03, 0.21, -0.03 ) ( 0.03, 0.26, 0.03 ) (vec3 1.0 0.95 0.4)
                ]
    in
    indicatorMesh (vec3 0.15 0.95 0.35) icon


{-| Lost Standard / Treasure indicator: Radiant gold with 8-point compass star icon.
-}
standardIndicatorMesh : WebGL.Mesh PropVertex
standardIndicatorMesh =
    let
        arm sign =
            box ( -0.03, 0.08, -0.16 ) ( 0.03, 0.17, 0.16 ) white
                |> transformTris (Mat4.makeRotate (sign * (pi / 4.0)) (vec3 0 1 0))

        icon =
            List.concat
                [ arm 0
                , arm 1
                , box ( -0.05, 0.08, -0.05 ) ( 0.05, 0.19, 0.05 ) (vec3 1.0 0.95 0.4)
                ]
    in
    indicatorMesh (vec3 1.0 0.84 0.12) icon


{-| Target / Enemy ship indicator: Fiery crimson red with crosshair bullseye icon.
-}
targetIndicatorMesh : WebGL.Mesh PropVertex
targetIndicatorMesh =
    let
        icon =
            List.concat
                [ box ( -0.04, 0.08, -0.04 ) ( 0.04, 0.18, 0.04 ) white
                , box ( -0.16, 0.08, -0.025 ) ( 0.16, 0.16, 0.025 ) white
                , box ( -0.025, 0.08, -0.16 ) ( 0.025, 0.16, 0.16 ) white
                , box ( -0.14, 0.08, 0.11 ) ( -0.11, 0.15, 0.14 ) (vec3 1.0 0.4 0.4)
                , box ( 0.11, 0.08, 0.11 ) ( 0.14, 0.15, 0.14 ) (vec3 1.0 0.4 0.4)
                , box ( -0.14, 0.08, -0.14 ) ( -0.11, 0.15, -0.11 ) (vec3 1.0 0.4 0.4)
                , box ( 0.11, 0.08, -0.14 ) ( 0.14, 0.15, -0.11 ) (vec3 1.0 0.4 0.4)
                ]
    in
    indicatorMesh (vec3 0.95 0.2 0.2) icon


{-| Ghost Vessel indicator: Spectral cyan with phantom twin sail icon.
-}
ghostIndicatorMesh : WebGL.Mesh PropVertex
ghostIndicatorMesh =
    let
        sail sign =
            box ( -0.025, 0.08, -0.14 ) ( 0.025, 0.19, 0.0 ) white
                |> transformTris
                    (Mat4.makeTranslate3 (sign * 0.08) 0 0.0
                        |> Mat4.mul (Mat4.makeRotate (sign * 0.35) (vec3 0 1 0))
                    )

        icon =
            List.concat
                [ sail 1
                , sail -1
                , box ( -0.02, 0.08, -0.15 ) ( 0.02, 0.22, 0.12 ) (vec3 0.6 1.0 0.9)
                ]
    in
    indicatorMesh (vec3 0.18 0.95 0.85) icon



-- WEATHER


{-| Spray thrown up at the bow when the boat drives through a rough sea:
authentic LEGO 1x1 round plates.
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
                    fract (time * 1.8 + toFloat i * 0.5)

                side =
                    if i == 0 then
                        1

                    else
                        -1

                distAlong =
                    2.0 + phase * 0.4

                sprayH =
                    0.05 + sin (phase * pi) * 0.35 * crestRough
            in
            draw.fitted "plateRound"
                (BrickProps.fitWidth 0.35)
                (Mat4.makeTranslate3
                    (p.x + sin rad * distAlong + side * cos rad * (0.6 + phase * 0.3))
                    (p.y + sprayH)
                    (p.z + cos rad * distAlong - side * sin rad * (0.6 + phase * 0.3))
                )
                (vec4 1.0 1.0 1.0 (1.0 - phase * 0.5))
    in
    if rough > 0.03 || ctx.sea > 1.2 then
        puff 0 ++ puff 1

    else
        []


{-| Dynamic LEGO brick foam wake trailing behind a boat when moving:
authentic 1x1 round plates and open-stud plates floating on the water surface.
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
                        fract (time * 1.6 + toFloat i * 0.33)

                    dist =
                        1.4 + toFloat i * 1.2 + phase * 0.6

                    spread =
                        0.4 + toFloat i * 0.35

                    propName =
                        if modBy 2 i == 0 then
                            "plateRound"

                        else
                            "plateOpenRound"

                    wakePuff side =
                        let
                            wx =
                                p.x - sin rad * dist + side * cos rad * spread

                            wz =
                                p.z - cos rad * dist - side * sin rad * spread
                        in
                        draw.fitted propName
                            (BrickProps.fitWidth 0.42)
                            (Mat4.makeTranslate3 wx 0.03 wz)
                            (vec4 0.9 0.96 1.0 (1.0 - phase * 0.6))
                in
                wakePuff 1 ++ wakePuff -1
        in
        tier 0 ++ tier 1 ++ tier 2 ++ tier 3

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


{-| Rain falling around the camera: the streaks of `Game.Precipitation`,
drawn with the ocean shader.
-}
rainEntities : Ctx -> Float -> Conditions -> List WebGL.Entity
rainEntities ctx time weather =
    let
        field =
            { eye = ctx.eye
            , time = time
            , windSpeed = weather.wind.speed
            , windDirection = weather.wind.direction
            }
    in
    case weather.precip of
        Weather.RainFall ->
            Precipitation.transforms weather.rain field
                |> List.map (\m -> ocean ctx m (vec3 0.88 0.92 0.98) rainMesh)

        -- Snow: white flakes drifting down slowly.
        Weather.SnowFall ->
            Precipitation.transforms weather.rain { field | time = time * 0.25, windSpeed = 0.5 * weather.wind.speed }
                |> List.map (\m -> ocean ctx m (vec3 1 1 1) snowMesh)


rainHeight : Float
rainHeight =
    Precipitation.streakHeight


{-| Rain streaks scattered through a 70 x 30 x 70 m box around the origin,
at fixed pseudo-random places, none right at the camera.
-}
rainMesh : WebGL.Mesh PropVertex
rainMesh =
    fallMesh 0.012 0.8


{-| Snowflakes: the rain's places, as small cubes instead of long streaks.
-}
snowMesh : WebGL.Mesh PropVertex
snowMesh =
    fallMesh 0.06 0.12


fallMesh : Float -> Float -> WebGL.Mesh PropVertex
fallMesh halfWidth length =
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
            box ( x - halfWidth, y, z - halfWidth ) ( x + halfWidth, y + length, z + halfWidth ) white
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
