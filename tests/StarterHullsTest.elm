module StarterHullsTest exposing (suite)

{-| Checks of the starter hulls in `starters/sails/` (embedded in
`Sails.StarterBases` by `node starters/embed-starters.mjs`):

  - each file is one grouped hull: the main model holds a single reference to
    the hull submodel, and the part count in its header is the real count,
  - `Sails.Ingest.validateShip` accepts it as a boat (under `maxShipParts`,
    enough hull to float) and `Sails.Analyze` only notes the missing sail,
  - Brick Studio's `Studio.Health` finds no misaligned or overlapping parts,
    and every part is joined to the rest of the hull by studs,
  - the hull has a flat bottom and a flat deck of plates on top (the four small
    hulls: deck plates at y = -8; the NPC hulls: free plate studs for the rig).

The frigate and sloop hulls (`frigate-hull.mpd`, `sloop-hull.mpd`) are the
hulls of the computer's ships in `Sails.NpcShips`. For each, the NPC ship's
hull submodel has the same name and the same parts as the starter's, so the
NPC ship is found as a build on that hull (`Sails.Editor.baseOf`), and every
part of its rig (masts, yards, sails, flags, cannons) is one the player can
add in the ship editor, in one of its colours.

-}

import Dict exposing (Dict)
import EmbeddedParts
import Expect
import LDraw.Connectivity as Connectivity exposing (ConnectionType(..))
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Analyze as Analyze exposing (Seaworthiness(..))
import Sails.Editor as Editor
import Sails.Ingest as Ingest
import Sails.NpcShips as NpcShips
import Sails.StarterBases as StarterBases exposing (Starter)
import Set exposing (Set)
import Studio.Health as Health
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Starter hulls"
        (test "there are six starter hulls" (\_ -> Expect.equal 6 (List.length StarterBases.all))
            :: List.map starterTest StarterBases.all
            ++ List.map npcHullTest npcHulls
        )


{-| The starter hulls that are the hulls of NPC ships: starter file, NPC ship
name and LDraw text.
-}
npcHulls : List ( String, String, String )
npcHulls =
    [ ( "frigate-hull.mpd", NpcShips.frigateName, NpcShips.frigate )
    , ( "sloop-hull.mpd", NpcShips.sloopName, NpcShips.sloop )
    ]


isNpcHull : Starter -> Bool
isNpcHull starter =
    List.any (\( file, _, _ ) -> file == starter.file) npcHulls


starterTest : Starter -> Test
starterTest starter =
    let
        doc =
            Document.parse starter.text

        hull =
            hullDocument starter.text

        flat =
            Document.flattenedPlacements doc
    in
    describe starter.file
        [ test "is one grouped submodel" <|
            \_ ->
                Expect.all
                    [ \d -> Expect.equal 1 (List.length (Document.mainPlacements d))
                    , \d -> Expect.equal 2 (List.length d.submodels)
                    , \d -> Expect.equal True (List.all (\p -> Document.isSubmodelReference p d) (Document.mainPlacements d))
                    ]
                    doc
        , test "header part count matches and stays within budget" <|
            \_ ->
                Expect.all
                    [ \n -> Expect.equal (headerPartCount starter.text) (Just n)
                    , \n -> Expect.atMost 80 n
                    , \n -> Expect.atMost Ingest.maxShipParts n
                    ]
                    (List.length flat)
        , test "the sails loader accepts it as a boat" <|
            \_ ->
                case Ingest.validateShip doc of
                    Ok spec ->
                        Expect.all
                            [ \s -> Expect.atLeast 2 s.hullPartCount
                            , \s -> Expect.equal 0 s.sailPartCount
                            , \s ->
                                case (Analyze.analyzeShip s).seaworthiness of
                                    MinorAdvisory _ ->
                                        Expect.pass

                                    other ->
                                        Expect.fail ("Expected only the no-sail advisory, got " ++ Debug.toString other)
                            ]
                            spec

                    Err err ->
                        Expect.fail err
        , test "has a flat bottom below the hull and a plate deck on top" <|
            \_ ->
                let
                    ys =
                        List.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0))) flat
                in
                Expect.all
                    [ \_ -> Expect.atMost 24 (List.maximum ys |> Maybe.withDefault 0)
                    , \_ ->
                        if isNpcHull starter then
                            -- the waist / deck plates keep studs free for the masts and cannons
                            Expect.atLeast 8 (freePlateStuds hull)

                        else
                            Expect.equal (Just -8) (List.filter (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)) < 0) flat |> List.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0))) |> List.maximum)
                    ]
                    ()
        , test "Studio health finds nothing to tidy" <|
            \_ -> Expect.equal [] (Health.check EmbeddedParts.cache hull Set.empty)
        , test "every part is joined to the hull by studs" <|
            \_ -> Expect.equal 1 (studComponents hull)
        ]


{-| An NPC ship and its starter hull: the same hull submodel, and a rig the
player can build in the editor.
-}
npcHullTest : ( String, String, String ) -> Test
npcHullTest ( file, shipName, shipText ) =
    let
        ship =
            Document.parse shipText

        base =
            List.filter (\b -> b.id ++ ".mpd" == file) Editor.bases |> List.head

        hullParts doc name =
            Document.placementsOf name doc
                |> List.map (\p -> ( p.part, p.color, Mat4.toRecord p.matrix ))

        rig =
            Document.mainPlacements ship
                |> List.filter (\p -> not (Document.isSubmodelReference p ship))
    in
    describe (file ++ " and " ++ shipName)
        [ test "the starter hull is in the shipyard" <|
            \_ -> Expect.equal (Just (String.replace ".mpd" "" file)) (Maybe.map .id base)
        , test "the NPC ship's hull submodel is the starter's, part for part" <|
            \_ ->
                case base of
                    Just b ->
                        Expect.all
                            [ \_ -> Expect.equal (hullParts b.document b.submodel) (hullParts ship b.submodel)
                            , \_ -> Expect.equal b.partCount (List.length (hullParts ship b.submodel))
                            ]
                            ()

                    Nothing ->
                        Expect.fail ("no editor base for " ++ file)
        , test "the NPC ship is a build on the starter hull" <|
            \_ -> Expect.equal (Maybe.map .id base) (Editor.baseOf ship |> Maybe.map .id)
        , test "every rig part is in the editor's parts list and palette" <|
            \_ ->
                rig
                    |> List.filter
                        (\p ->
                            not (List.member p.part (List.map .file Editor.parts))
                                || not (List.member p.color (List.map .code Editor.palette))
                        )
                    |> List.map (\p -> p.part ++ " colour " ++ String.fromInt p.color)
                    |> Expect.equal []
        ]


{-| Studs on the hull's plates that no other hull part covers: room to build
the rig on.
-}
freePlateStuds : Document -> Int
freePlateStuds doc =
    let
        placements =
            Document.mainPlacements doc

        points kind pl =
            case Connectivity.knownCatalogPart pl.part of
                Just conn ->
                    conn.connections
                        |> List.filter (\c -> c.connType == kind)
                        |> List.map (Connectivity.transformPoint pl.matrix pl.id)

                Nothing ->
                    []

        antiStuds =
            List.concatMap (points AntiStud) placements
    in
    placements
        |> List.filter (\pl -> List.member pl.part plateFiles)
        |> List.concatMap (points Stud)
        |> List.filter (\s -> not (List.any (\a -> Vec3.distance a.worldPosition s.worldPosition <= 0.5) antiStuds))
        |> List.length


plateFiles : List String
plateFiles =
    [ "3020.dat", "3021.dat", "3031.dat", "3034.dat", "3710.dat", "3795.dat" ]


{-| The hull submodel on its own, as a single-model document (`Studio.Health`
only checks the main model).
-}
hullDocument : String -> Document
hullDocument text =
    let
        afterMain =
            String.lines text
                |> dropThrough
                |> dropThrough
    in
    Document.parse (String.join "\n" afterMain)


{-| Drop lines up to and including the next `0 FILE` line.
-}
dropThrough : List String -> List String
dropThrough lines =
    case lines of
        first :: rest ->
            if String.startsWith "0 FILE" (String.trim first) then
                rest

            else
                dropThrough rest

        [] ->
            []


headerPartCount : String -> Maybe Int
headerPartCount text =
    String.lines text
        |> List.filterMap
            (\l ->
                if String.startsWith "0 // Parts: " l then
                    String.dropLeft 12 l |> String.words |> List.head |> Maybe.andThen String.toInt

                else
                    Nothing
            )
        |> List.head


{-| Number of groups of parts joined by stud / anti-stud pairs.
-}
studComponents : Document -> Int
studComponents doc =
    let
        placements =
            Document.mainPlacements doc

        points kind pl =
            case Connectivity.knownCatalogPart pl.part of
                Just conn ->
                    conn.connections
                        |> List.filter (\c -> c.connType == kind)
                        |> List.map (Connectivity.transformPoint pl.matrix pl.id)

                Nothing ->
                    []

        studs =
            List.concatMap (points Stud) placements

        edges =
            placements
                |> List.concatMap
                    (\pl ->
                        points AntiStud pl
                            |> List.concatMap
                                (\a ->
                                    studs
                                        |> List.filter
                                            (\s ->
                                                s.placementId
                                                    /= pl.id
                                                    && Vec3.distance a.worldPosition s.worldPosition
                                                    <= 0.5
                                            )
                                        |> List.map (\s -> ( pl.id, s.placementId ))
                                )
                    )

        adjacency : Dict Int (List Int)
        adjacency =
            List.foldl
                (\( a, b ) d ->
                    d
                        |> Dict.update a (\v -> Just (b :: Maybe.withDefault [] v))
                        |> Dict.update b (\v -> Just (a :: Maybe.withDefault [] v))
                )
                Dict.empty
                edges

        visit : List Int -> Set Int -> Set Int
        visit queue seen =
            case queue of
                [] ->
                    seen

                x :: rest ->
                    if Set.member x seen then
                        visit rest seen

                    else
                        visit (rest ++ Maybe.withDefault [] (Dict.get x adjacency)) (Set.insert x seen)

        count remaining seen n =
            case remaining of
                [] ->
                    n

                pl :: rest ->
                    if Set.member pl.id seen then
                        count rest seen n

                    else
                        count rest (visit [ pl.id ] seen) (n + 1)
    in
    count placements Set.empty 0
