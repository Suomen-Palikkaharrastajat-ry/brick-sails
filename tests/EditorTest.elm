module EditorTest exposing (suite)

{-| Checks of the ship editor's rules (`Sails.Editor`):

  - the palette has 8 different colours and the parts list stays short,
  - every part is one the editor can put on studs, and `validateShip` accepts
    every starter hull with any of them added where the editor drops it,
  - the masts, sails and cannon of the list are read as such by `Sails.Ingest`,
  - a hull with a mast, a sail and more parts is a seaworthy ship, survives a
    build link round trip and is found on its hull again,
  - the edits that would split the hull are refused by `baseIntact`.

-}

import Expect
import LDraw.Connectivity as Connectivity
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Analyze as Analyze exposing (Seaworthiness(..))
import Sails.Editor as Editor exposing (Base)
import Sails.Ingest as Ingest
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ship editor"
        [ test "the palette has 8 different colours" <|
            \_ ->
                Expect.all
                    [ \codes -> Expect.equal 8 (List.length codes)
                    , \codes -> Expect.equal 8 (Set.size (Set.fromList codes))
                    ]
                    (List.map .code Editor.palette)
        , test "the default colour is in the palette" <|
            \_ -> Expect.equal True (List.member Editor.defaultColour (List.map .code Editor.palette))
        , test "the parts list is short and has no duplicates" <|
            \_ ->
                let
                    files =
                        List.map .file Editor.parts
                in
                Expect.all
                    [ \fs -> Expect.lessThan 100 (List.length fs)
                    , \fs -> Expect.equal (List.length fs) (Set.size (Set.fromList fs))
                    ]
                    files
        , test "every part has an offline connectivity entry" <|
            \_ ->
                Editor.parts
                    |> List.filter (\p -> Connectivity.knownCatalogPart p.file == Nothing)
                    |> List.map .file
                    |> Expect.equal []
        , test "there are six hulls, each found again in its own document" <|
            \_ ->
                Expect.all
                    [ \bs -> Expect.equal 6 (List.length bs)
                    , \bs -> Expect.equal (List.map .id bs) (List.filterMap (\b -> Editor.baseOf b.document |> Maybe.map .id) bs)
                    ]
                    Editor.bases
        , test "the first ship is the narrow racer with a mast and a sail, and it sails" <|
            \_ ->
                case Editor.firstShip of
                    Just ( base, doc ) ->
                        let
                            plain =
                                Ingest.ingestDocument "test" base.document

                            rigged =
                                Ingest.ingestDocument "test" doc
                        in
                        Expect.all
                            [ \_ -> Expect.equal "narrow-racer" base.id
                            , \_ -> Expect.equal (Just base.id) (Editor.baseOf doc |> Maybe.map .id)
                            , \_ -> Expect.equal (plain.mastPartCount + 1) rigged.mastPartCount
                            , \_ -> Expect.equal (plain.sailPartCount + 1) rigged.sailPartCount
                            , \_ -> Expect.ok (Ingest.validateShip doc)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "no first ship"
        , describe "the rigging is read as rigging"
            (Editor.bases |> List.take 1 |> List.map riggingTest)
        , describe "every part added to every hull still makes a ship"
            (Editor.bases |> List.map partsOnBaseTest)
        , describe "a hull with added parts" (List.map builtShipTest Editor.bases)
        , describe "the hull cannot be split" (List.map guardTest Editor.bases)
        ]


riggingTest : Base -> Test
riggingTest base =
    let
        spec file =
            Ingest.ingestDocument "test" (addPart file base.document)

        plain =
            Ingest.ingestDocument "test" base.document
    in
    describe base.id
        [ test "the antenna is a mast" <|
            \_ -> Expect.equal (plain.mastPartCount + 1) (spec "3957a.dat").mastPartCount
        , test "the flag is a sail" <|
            \_ -> Expect.equal (plain.sailPartCount + 1) (spec "2335.dat").sailPartCount
        , test "a sail added with a mast selected hangs on the mast" <|
            \_ ->
                let
                    withMast =
                        addPart "3957a.dat" base.document

                    mast =
                        Document.mainPlacements withMast |> List.reverse |> List.head

                    sailMatrix =
                        mast |> Maybe.map (\m -> Editor.dropMatrix withMast (Set.singleton m.id) "2335.dat")

                    position matrix =
                        Mat4.transform matrix (vec3 0 0 0)
                in
                case ( mast, sailMatrix ) of
                    ( Just m, Just s ) ->
                        Expect.all
                            [ \_ -> Expect.within (Expect.Absolute 0.01) (Vec3.getX (position m.matrix)) (Vec3.getX (position s))
                            , \_ -> Expect.within (Expect.Absolute 0.01) (Vec3.getZ (position m.matrix)) (Vec3.getZ (position s))

                            -- the flag's clips (4 and 36 below its origin) are on the antenna (88 high)
                            , \_ -> Expect.atLeast (Vec3.getY (position m.matrix) - 88) (Vec3.getY (position s))
                            , \_ -> Expect.atMost (Vec3.getY (position m.matrix) - 36) (Vec3.getY (position s))
                            ]
                            ()

                    _ ->
                        Expect.fail "no mast"
        , test "the bar with round plate is a cannon" <|
            \_ -> Expect.equal (plain.cannonCount + 1) (spec "32828.dat").cannonCount
        ]


partsOnBaseTest : Base -> Test
partsOnBaseTest base =
    test base.id <|
        \_ ->
            Editor.parts
                |> List.filterMap
                    (\p ->
                        case Ingest.validateShip (addPart p.file base.document) of
                            Ok _ ->
                                Nothing

                            Err err ->
                                Just (p.file ++ ": " ++ err)
                    )
                |> Expect.equal []


builtShipTest : Base -> Test
builtShipTest base =
    let
        built =
            List.foldl addPart base.document [ "3001.dat", "3957a.dat", "2335.dat", "2335.dat", "32828.dat", "3829c01.dat", "3069b.dat" ]

        roundTrip =
            Ingest.decodeShipInput Nothing (Ingest.encodeShipLink built)
                |> Result.map .document
    in
    describe base.id
        [ test "is a seaworthy ship with mast and sails" <|
            \_ ->
                case Ingest.validateShip built of
                    Ok spec ->
                        Expect.all
                            [ \s -> Expect.equal Seaworthy (Analyze.analyzeShip s).seaworthiness
                            , \s -> Expect.equal 2 s.sailPartCount
                            , \s -> Expect.atLeast 1 s.mastPartCount
                            ]
                            spec

                    Err err ->
                        Expect.fail err
        , test "adds the parts" <|
            \_ -> Expect.equal (base.partCount + 7) (List.length (Document.flattenedPlacements built))
        , test "survives a build link round trip with its hull" <|
            \_ ->
                case roundTrip of
                    Ok doc ->
                        Expect.all
                            [ \d -> Expect.equal (Just base.id) (Editor.baseOf d |> Maybe.map .id)
                            , \d -> Expect.equal (List.length (Document.flattenedPlacements built)) (List.length (Document.flattenedPlacements d))
                            , \d -> Expect.ok (Ingest.validateShip d)
                            ]
                            doc

                    Err err ->
                        Expect.fail err
        ]


guardTest : Base -> Test
guardTest base =
    let
        doc =
            addPart "3001.dat" base.document

        baseRef =
            Document.mainPlacements doc |> List.filter (Editor.isBasePlacement base) |> List.head

        exploded =
            baseRef |> Maybe.map (\r -> Document.explodeSubmodel r.id doc)

        removed =
            baseRef |> Maybe.map (\r -> Document.removePlacement r.id doc)

        duplicated =
            baseRef |> Maybe.map (\r -> Document.addPlacement (Document.mainSubmodel doc).name { part = r.part, color = r.color, matrix = r.matrix } doc |> Tuple.first)
    in
    describe base.id
        [ test "a build on the hull keeps it intact" <|
            \_ -> Expect.equal True (Editor.baseIntact base doc)
        , test "ungrouping the hull breaks it" <|
            \_ -> Expect.equal (Just False) (Maybe.map (Editor.baseIntact base) exploded)
        , test "deleting the hull breaks it" <|
            \_ -> Expect.equal (Just False) (Maybe.map (Editor.baseIntact base) removed)
        , test "a second copy of the hull breaks it" <|
            \_ -> Expect.equal (Just False) (Maybe.map (Editor.baseIntact base) duplicated)
        ]


addPart : String -> Document -> Document
addPart file doc =
    Document.addPlacement (Document.mainSubmodel doc).name { part = file, color = 70, matrix = Editor.dropMatrix doc Set.empty file } doc
        |> Tuple.first
