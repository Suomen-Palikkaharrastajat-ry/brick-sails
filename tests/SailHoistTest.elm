module SailHoistTest exposing (suite)

{-| Lowering the sails (`Sails.ShipMesh.sailHoist`): the sails come down to
where they are attached, the deck or mast foot under them, and stand lower
the less sail is set. Parts are stand-in boxes so no catalog is needed.
-}

import Dict
import Expect
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Sails.Ingest as Ingest
import Sails.NpcShips as NpcShips
import Sails.ShipMesh as ShipMesh exposing (ShipMesh)
import Test exposing (Test, describe, test)


{-| An axis-aligned box from (x0, y0, z0) to (x1, y1, z1), in the part's
LDraw units (-Y up), as six quads.
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


{-| Stand-in parts: a 2 x 4 brick and plate, a 1 x 3 brick, an antenna mast
(4 bricks tall, its origin at its foot) and a flag hung from its origin.
-}
cache : PartCache
cache =
    Dict.fromList
        [ ( "3001.dat", Loaded (box ( -20, 0, -40 ) ( 20, 24, 40 )) )
        , ( "3020.dat", Loaded (box ( -20, 0, -40 ) ( 20, 8, 40 )) )
        , ( "3622.dat", Loaded (box ( -30, 0, -10 ) ( 30, 24, 10 )) )
        , ( "3957a.dat", Loaded (box ( -4, -96, -4 ) ( 4, 0, 4 )) )
        , ( "2335.dat", Loaded (box ( 0, 0, -2 ) ( 40, 32, 2 )) )
        ]


{-| A one-brick hull with a plate deck (its top at 8 LDU, Y-up), a mast on
the deck and the given rig.
-}
ship : List String -> Document
ship rig =
    Document.parse
        (String.join "\n"
            ([ "0 FILE test.ldr"
             , "1 70 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
             , "1 70 0 -8 0 1 0 0 0 1 0 0 0 1 3020.dat"
             , "1 0 0 -8 0 1 0 0 0 1 0 0 0 1 3957a.dat"
             ]
                ++ rig
            )
        )


meshOf : Document -> ShipMesh
meshOf doc =
    ShipMesh.build (Ingest.sailRule (Document.flattenedPlacements doc)) doc cache


{-| Lowest and highest point (Y-up LDU) of the sails as drawn at a sail
setting.
-}
drawnSails : ShipMesh -> Float -> ( Float, Float )
drawnSails mesh sail =
    let
        ys =
            mesh.sails.surface
                |> List.concatMap (\( a, b, c ) -> [ a, b, c ])
                |> List.map (\v -> Vec3.getY (Mat4.transform (ShipMesh.sailHoist mesh sail) v.position))
    in
    ( List.minimum ys |> Maybe.withDefault 0, List.maximum ys |> Maybe.withDefault 0 )


deckTop : Float
deckTop =
    8


lowersToAttachment : String -> ShipMesh -> Float -> Test
lowersToAttachment name mesh foot =
    test name <|
        \_ ->
            let
                ( fullLow, fullTop ) =
                    drawnSails mesh 1

                ( reefLow, reefTop ) =
                    drawnSails mesh 0.65

                ( downLow, downTop ) =
                    drawnSails mesh 0
            in
            Expect.all
                [ \_ -> Expect.within (Expect.Absolute 0.01) foot mesh.sailFoot
                , \_ -> Expect.within (Expect.Absolute 0.01) foot downLow
                , \_ -> Expect.within (Expect.Absolute 0.01) mesh.sailLow fullLow
                , \_ -> Expect.within (Expect.Absolute 0.01) mesh.sailTop fullTop
                , \_ -> Expect.lessThan fullTop reefTop
                , \_ -> Expect.lessThan reefTop downTop
                , \_ -> Expect.atMost fullLow reefLow
                , \_ -> Expect.atMost reefLow downLow
                , \_ -> Expect.atMost (foot + 0.12 * (fullTop - foot) + 0.01) downTop
                , \_ -> Expect.atLeast (downTop - downLow - 0.01) (fullTop - fullLow)
                ]
                ()


suite : Test
suite =
    describe "Lowering the sails"
        [ lowersToAttachment "white 1 x 3 sails stacked on the deck fold down onto the deck"
            (meshOf (ship [ "1 15 30 -32 0 1 0 0 0 1 0 0 0 1 3622.dat", "1 15 30 -56 0 1 0 0 0 1 0 0 0 1 3622.dat" ]))
            deckTop
        , lowersToAttachment "a flag hung at the mast top comes down to the deck"
            (meshOf (ship [ "1 15 0 -104 0 1 0 0 0 1 0 0 0 1 2335.dat" ]))
            deckTop
        , test "the frigate's sails hanging from yards come down below the lowest yard, to the mast foot" <|
            \_ ->
                let
                    doc =
                        Document.parse NpcShips.frigate

                    boxes =
                        Document.flattenedPlacements doc
                            |> List.map (\p -> ( p.part, Loaded (box ( -10, 0, -10 ) ( 10, 24, 10 )) ))
                            |> Dict.fromList

                    mesh =
                        ShipMesh.build (Ingest.sailRule (Document.flattenedPlacements doc)) doc boxes

                    ( downLow, downTop ) =
                        drawnSails mesh 0
                in
                Expect.all
                    [ \_ -> Expect.lessThan mesh.sailLow mesh.sailFoot
                    , \_ -> Expect.within (Expect.Absolute 0.01) mesh.sailFoot downLow
                    , \_ -> Expect.atMost (mesh.sailFoot + 0.12 * (mesh.sailTop - mesh.sailFoot) + 0.01) downTop
                    ]
                    ()
        ]
