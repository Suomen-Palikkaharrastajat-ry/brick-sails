module NpcScaleTest exposing (suite)

{-| Patrol ships are drawn 1:1 with the player's bricks: an NPC model drawn
as a pirate (`Sails.ShipMesh.build`, then `Sails.Render` with
`Sails.Pirates.shipScale` as the boat's `scale`) has the same world-space
bounding box as the same model built and drawn as the player's own boat
(scale 1), and the same length and beam as the player's build on the
matching starter hull. Its collision circle fits that drawn hull. Parts are
stand-in boxes of the parts' real footprints, so no catalog is needed.
-}

import Dict
import Expect
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Ingest as Ingest
import Sails.NpcShips as NpcShips
import Sails.Pirates as Pirates exposing (ShipKind(..))
import Sails.ShipMesh as ShipMesh exposing (ShipMesh)
import Sails.StarterBases as StarterBases
import Test exposing (Test, describe, test)


{-| An axis-aligned box from (x0, y0, z0) to (x1, y1, z1), in LDU, as six quads.
-}
box : ( Float, Float, Float ) -> ( Float, Float, Float ) -> List LDrawLine
box ( x0, y0, z0 ) ( x1, y1, z1 ) =
    let
        pt ( x, y, z ) =
            String.join " " (List.map String.fromFloat [ x, y, z ])

        quad a b c d =
            "4 16 " ++ String.join " " (List.map pt [ a, b, c, d ])
    in
    Parser.parseFile
        (String.join "\n"
            [ quad ( x0, y0, z0 ) ( x1, y0, z0 ) ( x1, y0, z1 ) ( x0, y0, z1 )
            , quad ( x0, y1, z0 ) ( x1, y1, z0 ) ( x1, y1, z1 ) ( x0, y1, z1 )
            , quad ( x0, y0, z0 ) ( x1, y0, z0 ) ( x1, y1, z0 ) ( x0, y1, z0 )
            , quad ( x0, y0, z1 ) ( x1, y0, z1 ) ( x1, y1, z1 ) ( x0, y1, z1 )
            , quad ( x0, y0, z0 ) ( x0, y0, z1 ) ( x0, y1, z1 ) ( x0, y1, z0 )
            , quad ( x1, y0, z0 ) ( x1, y0, z1 ) ( x1, y1, z1 ) ( x1, y1, z0 )
            ]
        )


{-| A part's stand-in box: its footprint in studs along LDraw X and Z and its
height in LDU, hanging below its origin (LDraw -Y up).
-}
footprint : String -> ( ( Float, Float, Float ), ( Float, Float, Float ) )
footprint part =
    let
        brick w d h =
            ( ( -10 * w, 0, -10 * d ), ( 10 * w, h, 10 * d ) )
    in
    case part of
        "3001.dat" ->
            brick 4 2 24

        "3002.dat" ->
            brick 3 2 24

        "3003.dat" ->
            brick 2 2 24

        "3004.dat" ->
            brick 2 1 24

        "3005.dat" ->
            brick 1 1 24

        "3010.dat" ->
            brick 4 1 24

        "3622.dat" ->
            brick 3 1 24

        "3660b.dat" ->
            brick 2 2 24

        "87087.dat" ->
            brick 1 1 24

        "3062b.dat" ->
            brick 1 1 24

        "43888.dat" ->
            brick 1 1 144

        "87580.dat" ->
            brick 2 2 8

        "3020.dat" ->
            brick 4 2 8

        "3021.dat" ->
            brick 3 2 8

        "3031.dat" ->
            brick 4 4 8

        "3710.dat" ->
            brick 4 1 8

        "15573.dat" ->
            brick 2 1 8

        "3069b.dat" ->
            brick 2 1 8

        "3957a.dat" ->
            ( ( -4, -96, -4 ), ( 4, 0, 4 ) )

        "2335.dat" ->
            ( ( 0, 0, -2 ), ( 40, 32, 2 ) )

        _ ->
            brick 1 1 8


{-| Stand-in parts for every part a model places.
-}
cacheFor : Document -> PartCache
cacheFor doc =
    Document.flattenedPlacements doc
        |> List.map (\p -> ( p.part, Loaded (footprint p.part |> (\( a, b ) -> box a b)) ))
        |> Dict.fromList


meshOf : Document -> ShipMesh
meshOf doc =
    ShipMesh.build (Ingest.sailRule (Document.flattenedPlacements doc)) doc (cacheFor doc)


{-| World-space bounding box (min, max) of a model drawn at `scale` the way
`Sails.Render` draws a boat: `scale3 scale` on the boat's pose, then the
mesh's `toBoat`, for every corner of every part.
-}
worldBox : Float -> Document -> ( Vec3, Vec3 )
worldBox scale doc =
    let
        mesh =
            meshOf doc

        pose =
            Mat4.makeTranslate3 0 0 0

        model =
            Mat4.mul (Mat4.scale3 scale scale scale pose) mesh.toBoat

        corners : Mat4 -> ( ( Float, Float, Float ), ( Float, Float, Float ) ) -> List Vec3
        corners m ( ( x0, y0, z0 ), ( x1, y1, z1 ) ) =
            [ vec3 x0 y0 z0, vec3 x1 y0 z0, vec3 x0 y1 z0, vec3 x1 y1 z0, vec3 x0 y0 z1, vec3 x1 y0 z1, vec3 x0 y1 z1, vec3 x1 y1 z1 ]
                |> List.map (Mat4.transform m)

        points =
            Document.flattenedPlacements doc
                |> List.concatMap (\p -> corners (Mat4.mul model p.matrix) (footprint p.part))

        fold f =
            List.foldl
                (\v acc -> vec3 (f (Vec3.getX v) (Vec3.getX acc)) (f (Vec3.getY v) (Vec3.getY acc)) (f (Vec3.getZ v) (Vec3.getZ acc)))
                (List.head points |> Maybe.withDefault (vec3 0 0 0))
                points
    in
    ( fold min, fold max )


{-| Size of a world box along x (beam), y (height) and z (length), metres.
-}
sizeOf : ( Vec3, Vec3 ) -> { x : Float, y : Float, z : Float }
sizeOf ( lo, hi ) =
    Vec3.sub hi lo |> Vec3.toRecord


modelOf : ShipKind -> Document
modelOf kind =
    case kind of
        Sloop ->
            Document.parse NpcShips.sloop

        _ ->
            Document.parse NpcShips.frigate


starterHullOf : ShipKind -> Document
starterHullOf kind =
    let
        file =
            case kind of
                Sloop ->
                    "sloop-hull.mpd"

                _ ->
                    "frigate-hull.mpd"
    in
    StarterBases.all
        |> List.filter (\s -> s.file == file)
        |> List.head
        |> Maybe.map (.text >> Document.parse)
        |> Maybe.withDefault (Document.parse "")


near : Float -> Float -> Float -> Expect.Expectation
near tolerance expected actual =
    Expect.within (Expect.Absolute tolerance) expected actual


kindTests : ShipKind -> String -> Float -> List Test
kindTests kind label lengthMeters =
    [ test (label ++ ": drawn as a pirate, the same world box as the player's build of the same model") <|
        \_ ->
            let
                ( playerLo, playerHi ) =
                    worldBox 1 (modelOf kind)

                ( npcLo, npcHi ) =
                    worldBox (Pirates.shipScale kind) (modelOf kind)

                coords v =
                    let
                        r =
                            Vec3.toRecord v
                    in
                    [ r.x, r.y, r.z ]
            in
            List.map2 (-) (coords npcLo ++ coords npcHi) (coords playerLo ++ coords playerHi)
                |> List.map abs
                |> List.maximum
                |> Maybe.withDefault 1
                |> Expect.atMost 1.0e-6
    , test (label ++ ": as long and broad as the player's build on its starter hull (length at 0.035 m per LDU)") <|
        \_ ->
            let
                npc =
                    sizeOf (worldBox (Pirates.shipScale kind) (modelOf kind))

                player =
                    sizeOf (worldBox 1 (starterHullOf kind))
            in
            Expect.all
                [ \_ -> near 1.0e-6 player.z npc.z
                , \_ -> near 1.0e-6 player.x npc.x
                , \_ -> near 1.0e-6 lengthMeters npc.z
                , \_ -> near 1.0e-6 (Pirates.shipScale kind * (meshOf (modelOf kind)).lengthMeters) (meshOf (starterHullOf kind)).lengthMeters
                ]
                ()
    , test (label ++ ": its collision circle fits the hull as drawn") <|
        \_ ->
            let
                drawn =
                    sizeOf (worldBox (Pirates.shipScale kind) (modelOf kind))

                r =
                    Pirates.collisionRadius kind
            in
            Expect.all
                [ \_ -> Expect.greaterThan (0.5 * drawn.x) r
                , \_ -> Expect.lessThan (0.5 * drawn.z) r
                , \_ -> near 0.1 (0.24 * drawn.z) r
                ]
                ()
    ]


suite : Test
suite =
    describe "NPC ships at the player's scale"
        (kindTests Frigate "frigate" (280 * ShipMesh.studScale)
            ++ kindTests Corvette "corvette" (280 * ShipMesh.studScale)
            ++ kindTests Sloop "sloop" (240 * ShipMesh.studScale)
        )
