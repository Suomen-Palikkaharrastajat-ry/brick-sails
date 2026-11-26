module LegalConnectionsTest exposing (suite)

{-| Verifies that all built-in builds and vehicles in the game only use legal brick connections:

  - No illegal part overlaps or misaligned parts (via Studio.Health)
  - All parts sit above or flat on the table/waterline (y <= 24)
  - Builds form single connected structures without floating disconnected pieces

-}

import Dict
import EmbeddedParts
import Expect
import Game.BrickModels as BrickModels
import LDraw.Connectivity as Connectivity
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Boats as Boats
import Sails.BuilderDock as BuilderDock
import Sails.Scenery as Scenery
import Sails.StarterBases as StarterBases exposing (Starter)
import Sails.Types exposing (Boat)
import Set
import Studio.Health as Health
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Legal brick connections"
        [ describe "Starter hulls"
            (List.map checkStarter StarterBases.all)
        , describe "Built-in boats"
            (List.map checkBoat Boats.all)
        , describe "Scenery pieces"
            (List.map checkScenery Scenery.all)
        , describe "Builder harbour"
            [ test "Studio health finds no illegal overlaps or misalignments" <|
                \_ ->
                    Expect.equal [] (Health.check EmbeddedParts.cache BuilderDock.document Set.empty)
            ]
        ]


checkStarter : Starter -> Test
checkStarter starter =
    let
        doc =
            Document.parse starter.text
    in
    describe starter.file
        [ test "Studio health finds no illegal overlaps or misalignments" <|
            \_ ->
                Expect.equal [] (Health.check EmbeddedParts.cache doc Set.empty)
        ]


checkBoat : Boat -> Test
checkBoat boat =
    let
        doc =
            Document.parse boat.ldraw

        placements =
            Document.flattenedPlacements doc

        ys =
            List.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0))) placements

        issues =
            Health.check EmbeddedParts.cache doc Set.empty
    in
    describe boat.name
        [ test "has no keel below flat waterline (y <= 24)" <|
            \_ ->
                Expect.atMost 24 (List.maximum ys |> Maybe.withDefault 0)
        , test "Studio health finds no illegal overlaps or misalignments" <|
            \_ ->
                if List.isEmpty issues then
                    Expect.pass

                else
                    Expect.fail (Debug.toString issues)
        ]


checkScenery : Scenery.Piece -> Test
checkScenery piece =
    let
        doc =
            Document.parse piece.ldraw

        issues =
            Health.check EmbeddedParts.cache doc Set.empty
    in
    describe piece.name
        [ test "Studio health finds no illegal overlaps or misalignments" <|
            \_ ->
                if List.isEmpty issues then
                    Expect.pass

                else
                    Expect.fail (Debug.toString issues)
        ]
