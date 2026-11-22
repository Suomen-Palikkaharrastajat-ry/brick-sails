module StarterHullsTest exposing (suite)

{-| Checks of the starter hulls in `starters/sails/` (embedded in
`StarterBases` by `node starters/embed-starters.mjs`):

  - each file is one grouped hull: the main model holds a single reference to
    the hull submodel, and the part count in its header is the real count,
  - `Sails.Ingest.validateShip` accepts it as a boat (under `maxShipParts`,
    enough hull to float) and `Sails.Analyze` only notes the missing sail,
  - Brick Studio's `Studio.Health` finds no misaligned or overlapping parts,
    and every part is joined to the rest of the hull by studs,
  - the hull has a keel below the waterline and a flat deck of plates on top.

-}

import Dict exposing (Dict)
import EmbeddedParts
import Expect
import LDraw.Connectivity as Connectivity exposing (ConnectionType(..))
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Analyze as Analyze exposing (Seaworthiness(..))
import Sails.Ingest as Ingest
import Set exposing (Set)
import StarterBases exposing (Starter)
import Studio.Health as Health
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Starter hulls"
        (test "there are four starter hulls" (\_ -> Expect.equal 4 (List.length StarterBases.all))
            :: List.map starterTest StarterBases.all
        )


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
        , test "has a keel below the hull and a plate deck on top" <|
            \_ ->
                let
                    ys =
                        List.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0))) flat
                in
                Expect.all
                    [ \_ -> Expect.atLeast 48 (List.maximum ys |> Maybe.withDefault 0)
                    , \_ -> Expect.equal (Just -8) (List.filter (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)) < 0) flat |> List.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0))) |> List.maximum)
                    ]
                    ()
        , test "Studio health finds nothing to tidy" <|
            \_ -> Expect.equal [] (Health.check EmbeddedParts.cache hull Set.empty)
        , test "every part is joined to the hull by studs" <|
            \_ -> Expect.equal 1 (studComponents hull)
        ]


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
