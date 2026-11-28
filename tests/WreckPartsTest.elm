module WreckPartsTest exposing (suite)

{-| A model's wreck geometry (`Sails.ShipMesh.parts`) holds only the parts a
wreck can show (`Sails.Wreck.shownParts`), in piece order, so piece number
`index` is still the part `Sails.Wreck.partSlot` picks for it.
-}

import Array
import Dict
import Expect
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import Math.Vector3 as Vec3
import Sails.ShipMesh as ShipMesh
import Sails.Wreck as Wreck
import Test exposing (Test, describe, test)


{-| A row of `n` bricks, brick `i` at x = 40 i LDU.
-}
row : Int -> Document
row n =
    List.range 0 (n - 1)
        |> List.map (\i -> "1 4 " ++ String.fromInt (40 * i) ++ " 0 0 1 0 0 0 1 0 0 0 1 3001.dat")
        |> String.join "\n"
        |> Document.parse


cache : PartCache
cache =
    Dict.fromList [ ( "3001.dat", Loaded (Parser.parseFile "4 16 -10 0 -10 10 0 -10 10 24 10 -10 24 10") ) ]


{-| Which brick of the row each built part is, from its centre.
-}
bricksOf : ShipMesh.ShipMesh -> List Int
bricksOf mesh =
    Array.toList mesh.parts
        |> List.map (\part -> round (Vec3.getX part.pivot / 40))


{-| The bricks the wreck pieces of an `n`-brick model show, by `partSlot`.
-}
expectedBricks : Int -> List Int
expectedBricks n =
    List.filterMap (Wreck.partSlot n) (List.range 0 (Wreck.pieceCount - 1))


suite : Test
suite =
    describe "Wreck part meshes"
        [ test "shownParts picks the parts partSlot picks, in piece order" <|
            \_ ->
                List.map (\n -> Wreck.shownParts (List.range 0 (n - 1))) [ 0, 1, 7, 20, 21, 57, 300 ]
                    |> Expect.equal (List.map expectedBricks [ 0, 1, 7, 20, 21, 57, 300 ])
        , test "a big model builds only the parts its wreck can show" <|
            \_ ->
                let
                    mesh =
                        ShipMesh.build (\_ -> False) (row 57) cache
                in
                Expect.all
                    [ \_ -> Expect.equal Wreck.pieceCount (Array.length mesh.parts)
                    , \_ -> Expect.equal (expectedBricks 57) (bricksOf mesh)
                    ]
                    ()
        , test "a small model still builds every one of its parts" <|
            \_ ->
                ShipMesh.build (\_ -> False) (row 7) cache
                    |> bricksOf
                    |> Expect.equal (List.range 0 6)
        , test "every wreck piece of a big model still gets its part, with geometry" <|
            \_ ->
                let
                    mesh =
                        ShipMesh.build (\_ -> False) (row 57) cache
                in
                List.range 0 (Wreck.pieceCount - 1)
                    |> List.map (\index -> Array.get index mesh.parts |> Maybe.map (\part -> not (List.isEmpty part.scene.surface)))
                    |> Expect.equal (List.repeat Wreck.pieceCount (Just True))
        , test "a smashed scenery model builds only the parts its wreck can show" <|
            \_ ->
                ShipMesh.buildScenery 0.035 (row 40) cache
                    |> ShipMesh.withParts (row 40) cache
                    |> bricksOf
                    |> Expect.equal (expectedBricks 40)
        ]
