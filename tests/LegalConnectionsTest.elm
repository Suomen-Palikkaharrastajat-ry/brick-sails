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
import Sails.NpcShips as NpcShips
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
        , describe "NPC ships"
            (List.map checkNpcShip NpcShips.all)
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


{-| An NPC ship is checked three times, since `Health.check` only looks at a
document's main submodel (and skips the hull reference in it): the main model
(rig, sails, cannons), the grouped hull submodel on its own, and the whole
ship flattened into one model, so rig parts are checked against the hull.
The flattened ship must also be one piece: every part joined to the rest by
studs, or (a flag) by its clips on a mast.
-}
checkNpcShip : ( String, String ) -> Test
checkNpcShip ( name, ldraw ) =
    let
        hullText =
            ldraw
                |> String.split "0 NOFILE"
                |> List.drop 1
                |> String.join "0 NOFILE"
                |> String.trim

        healthOf text =
            Health.check EmbeddedParts.cache (Document.parse text) Set.empty

        flatShip =
            Document.fromLines (Document.flattenedLines (Document.parse ldraw))

        ys =
            Document.flattenedPlacements (Document.parse ldraw)
                |> List.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)))
    in
    describe name
        [ test "has no keel below flat waterline (y <= 24)" <|
            \_ ->
                Expect.atMost 24 (List.maximum ys |> Maybe.withDefault 0)
        , test "the hull is one grouped submodel with parts in it" <|
            \_ ->
                Expect.greaterThan 10 (List.length (Document.flattenedPlacements (Document.parse hullText)))
        , test "Studio health finds no illegal overlaps or misalignments on the rig" <|
            \_ ->
                Expect.equal [] (healthOf ldraw)
        , test "Studio health finds no illegal overlaps or misalignments on the hull" <|
            \_ ->
                Expect.equal [] (healthOf hullText)
        , test "Studio health finds no illegal overlaps or misalignments on the whole ship" <|
            \_ ->
                Expect.equal [] (Health.check EmbeddedParts.cache flatShip Set.empty)
        , test "every part is joined to the ship: no floating parts" <|
            \_ ->
                Expect.equal [] (looseParts flatShip)
        ]


{-| The parts of a single-model document that are not joined to its largest
piece. Two parts are joined when an anti-stud of one is on a stud of the
other, or a clip of one (a flag's) lies inside the other's box (a mast).
-}
looseParts : Document -> List String
looseParts doc =
    let
        placements =
            Document.mainPlacements doc

        connOf pl =
            Connectivity.knownCatalogPart pl.part

        points kind pl =
            case connOf pl of
                Just conn ->
                    conn.connections
                        |> List.filter (\c -> kind c.connType)
                        |> List.map (Connectivity.transformPoint pl.matrix pl.id)

                Nothing ->
                    []

        isClip t =
            case t of
                Connectivity.ClipMale _ ->
                    True

                _ ->
                    False

        studs =
            List.concatMap (points ((==) Connectivity.Stud)) placements

        inBox point pl =
            let
                local =
                    Mat4.transform (Connectivity.invertRigid pl.matrix) point
            in
            connOf pl
                |> Maybe.map
                    (List.any
                        (\b ->
                            (Vec3.getX local >= Vec3.getX b.min - 0.5)
                                && (Vec3.getX local <= Vec3.getX b.max + 0.5)
                                && (Vec3.getY local >= Vec3.getY b.min - 0.5)
                                && (Vec3.getY local <= Vec3.getY b.max + 0.5)
                                && (Vec3.getZ local >= Vec3.getZ b.min - 0.5)
                                && (Vec3.getZ local <= Vec3.getZ b.max + 0.5)
                        )
                        << .boundingBoxes
                    )
                |> Maybe.withDefault False

        edges =
            placements
                |> List.concatMap
                    (\pl ->
                        (points ((==) Connectivity.AntiStud) pl
                            |> List.concatMap
                                (\a ->
                                    studs
                                        |> List.filter (\s -> s.placementId /= pl.id && Vec3.distance a.worldPosition s.worldPosition <= 0.5)
                                        |> List.map (\s -> ( pl.id, s.placementId ))
                                )
                        )
                            ++ (points isClip pl
                                    |> List.concatMap
                                        (\c ->
                                            placements
                                                |> List.filter (\o -> o.id /= pl.id && inBox c.worldPosition o)
                                                |> List.map (\o -> ( pl.id, o.id ))
                                        )
                               )
                    )

        adjacency =
            List.foldl
                (\( a, b ) d ->
                    d
                        |> Dict.update a (\v -> Just (b :: Maybe.withDefault [] v))
                        |> Dict.update b (\v -> Just (a :: Maybe.withDefault [] v))
                )
                Dict.empty
                edges

        visit queue seen =
            case queue of
                [] ->
                    seen

                x :: rest ->
                    if Set.member x seen then
                        visit rest seen

                    else
                        visit (rest ++ Maybe.withDefault [] (Dict.get x adjacency)) (Set.insert x seen)

        largest =
            placements
                |> List.map (\pl -> visit [ pl.id ] Set.empty)
                |> List.sortBy (\c -> negate (Set.size c))
                |> List.head
                |> Maybe.withDefault Set.empty
    in
    placements
        |> List.filter (\pl -> not (Set.member pl.id largest))
        |> List.map (\pl -> pl.part ++ " at " ++ Debug.toString (Vec3.toRecord (Mat4.transform pl.matrix (vec3 0 0 0))))
