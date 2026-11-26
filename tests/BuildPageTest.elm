module BuildPageTest exposing (suite)

{-| The ship editor screen (`Pages.Build`) keeps the hull in one piece: with
the hull selected, the keys and buttons that would ungroup, delete, cut,
duplicate or regroup it leave the build as it was, while parts added from the
dock can still be turned, copied, pasted and deleted. The palette row keeps
its order while the same card is tapped again, and the full catalog opens
and closes from its button.
-}

import Expect
import I18n exposing (Language(..))
import LDraw.Document as Document exposing (Document)
import Pages.Build as Build exposing (BuilderTab(..), Msg(..))
import Sails.Analyze as Analyze exposing (Seaworthiness(..))
import Sails.Editor as Editor exposing (Base)
import Sails.Ingest as Ingest
import Studio.Core as Core
import Test exposing (Test, describe, test)
import Viewport


suite : Test
suite =
    describe "Ship editor screen" (paletteTests :: List.map baseTest Editor.bases)


{-| The palette row and the full catalog, on the first starter hull.
-}
paletteTests : Test
paletteTests =
    case Editor.bases of
        base :: _ ->
            let
                opened =
                    open base

                firstCard =
                    List.head opened.recentParts
            in
            describe "palette and catalog"
                [ test "tapping the same card again keeps the palette order" <|
                    \_ ->
                        let
                            tapped =
                                opened |> step (AddPart "3039.dat") |> step (AddPart "3039.dat") |> step (AddPart "3039.dat")
                        in
                        Expect.all
                            [ \m -> Expect.equal opened.recentParts m.recentParts
                            , \m -> Expect.equal "3039.dat" m.activePart
                            , \m -> Expect.equal (partCount base.document + 3) (partCount (Build.document m))
                            ]
                            tapped
                , test "the tapped card moves to the front once the player moves on" <|
                    \_ ->
                        let
                            movedOn =
                                opened |> step (AddPart "3039.dat") |> step PaletteLeft
                        in
                        Expect.equal (Just "3039.dat") (List.head movedOn.recentParts)
                , test "the palette keeps its first card until then" <|
                    \_ ->
                        Expect.equal firstCard (List.head (step (AddPart "3039.dat") opened).recentParts)
                , test "the catalog button opens the catalog and closes it again" <|
                    \_ ->
                        Expect.equal [ True, False ]
                            [ (step ToggleCatalog opened).catalogOpen
                            , (opened |> step ToggleCatalog |> step ToggleCatalog).catalogOpen
                            ]
                , test "adding a part from the catalog closes it" <|
                    \_ ->
                        Expect.equal False (opened |> step ToggleCatalog |> step (AddPart "2335.dat")).catalogOpen
                , test "top toolbar drawer tabs toggle BasesTab, ImportBuildTab and ReviewTab" <|
                    \_ ->
                        Expect.equal
                            [ BasesTab, BuildBricksTab, ImportBuildTab, ReviewTab, BuildBricksTab ]
                            [ (step (SelectTab BasesTab) opened).activeTab
                            , (opened |> step (SelectTab BasesTab) |> step (SelectTab BasesTab)).activeTab
                            , (step (SelectTab ImportBuildTab) opened).activeTab
                            , (opened |> step (SelectTab ImportBuildTab) |> step (SelectTab ReviewTab)).activeTab
                            , (opened |> step (SelectTab ReviewTab) |> step (SelectTab ReviewTab)).activeTab
                            ]
                , test "evaluation AddPartList helper rigs a bare starter hull with mast and sail to seaworthy" <|
                    \_ ->
                        let
                            rigged =
                                opened
                                    |> step (SelectTab ReviewTab)
                                    |> step (AddPartList [ "3957a.dat", "2335.dat" ])

                            spec =
                                Ingest.ingestDocument "Rigged" (Build.document rigged)
                        in
                        Expect.all
                            [ \_ -> Expect.equal BuildBricksTab rigged.activeTab
                            , \_ -> Expect.equal 1 spec.mastPartCount
                            , \_ -> Expect.equal 1 spec.sailPartCount
                            , \_ -> Expect.equal Seaworthy (Analyze.analyzeShip spec).seaworthiness
                            ]
                            ()
                ]

        [] ->
            test "has starter hulls" (\_ -> Expect.fail "no starter hulls")


open : Base -> Build.Model
open base =
    Build.init
        { ldrawBase = ""
        , ldrawFallbackBase = ""
        , supportsPointerEvents = True
        , language = En
        , width = 390
        , height = 844
        }
        base
        base.document
        |> Tuple.first


baseTest : Base -> Test
baseTest base =
    let
        opened =
            open base

        baseId =
            Document.mainPlacements base.document |> List.map .id |> List.head |> Maybe.withDefault -1

        withBaseSelected =
            step (StudioMsg (Core.SelectOnly baseId)) opened

        attempts =
            [ ( "Ctrl+Shift+G ungroups", key "G" True True )
            , ( "Delete", key "Delete" False False )
            , ( "Backspace", key "Backspace" False False )
            , ( "Ctrl+X cuts", key "x" False True )
            , ( "Ctrl+D duplicates", key "d" False True )
            , ( "Ctrl+G regroups", key "g" False True )
            , ( "the delete button", DeleteSelected )
            ]
    in
    describe base.id
        (List.map
            (\( label, msg ) ->
                test (label ++ " leaves the hull in one piece") <|
                    \_ ->
                        let
                            after =
                                Build.document (step msg withBaseSelected)
                        in
                        Expect.all
                            [ \d -> Expect.equal True (Editor.baseIntact base d)
                            , \d -> Expect.equal (partCount base.document) (partCount d)
                            ]
                            after
            )
            attempts
            ++ [ test "the studio alone would ungroup it (so the checks above mean something)" <|
                    \_ ->
                        case key "G" True True of
                            StudioMsg studioMsg ->
                                let
                                    ( studio, _, _ ) =
                                        Core.update studioMsg withBaseSelected.studio
                                in
                                Expect.equal False (Editor.baseIntact base (Core.getDocument studio))

                            _ ->
                                Expect.fail "not a studio message"
               , test "added parts can be turned and deleted" <|
                    \_ ->
                        let
                            added =
                                opened |> step (AddPart "3001.dat") |> step (AddPart "3039.dat")

                            turned =
                                step (Turn TurnLeft) added

                            deleted =
                                step DeleteSelected turned
                        in
                        Expect.all
                            [ \_ -> Expect.equal (partCount base.document + 2) (partCount (Build.document added))
                            , \_ -> Expect.notEqual (Build.document added |> placementMatrices) (Build.document turned |> placementMatrices)
                            , \_ -> Expect.equal (partCount base.document + 1) (partCount (Build.document deleted))
                            , \_ -> Expect.equal True (Editor.baseIntact base (Build.document deleted))
                            ]
                            ()
               , test "an added part can be copied and pasted" <|
                    \_ ->
                        let
                            pasted =
                                opened |> step (AddPart "3001.dat") |> step CopySelected |> step PasteSelected
                        in
                        Expect.all
                            [ \d -> Expect.equal (partCount base.document + 2) (partCount d)
                            , \d -> Expect.equal True (Editor.baseIntact base d)
                            ]
                            (Build.document pasted)
               , test "pasting a copy of the hull leaves the hull in one piece" <|
                    \_ ->
                        let
                            pasted =
                                withBaseSelected |> step (key "c" False True) |> step PasteSelected
                        in
                        Expect.all
                            [ \d -> Expect.equal (partCount base.document) (partCount d)
                            , \d -> Expect.equal True (Editor.baseIntact base d)
                            ]
                            (Build.document pasted)
               , test "picking a colour recolours the selected part" <|
                    \_ ->
                        let
                            added =
                                opened |> step (AddPart "3001.dat") |> step (PickColour 14)

                            colours =
                                Document.mainPlacements (Build.document added) |> List.map .color
                        in
                        Expect.equal True (List.member 14 colours)
               ]
        )


step : Msg -> Build.Model -> Build.Model
step msg model =
    let
        ( next, _, _ ) =
            Build.update msg model
    in
    next


key : String -> Bool -> Bool -> Msg
key k shift ctrl =
    StudioMsg
        (Core.ViewportMsg
            (Viewport.KeyPressed
                { key = k
                , shift = shift
                , ctrl = ctrl
                , meta = False
                , alt = False
                , repeat = False
                , targetTag = "BODY"
                , targetEditable = False
                , targetRole = ""
                }
            )
        )


partCount : Document -> Int
partCount doc =
    List.length (Document.flattenedPlacements doc)


placementMatrices : Document -> List String
placementMatrices doc =
    Document.mainPlacements doc |> List.map (\p -> Debug.toString p.matrix)
