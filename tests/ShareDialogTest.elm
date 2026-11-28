module ShareDialogTest exposing (suite)

{-| The ship editor's share dialog, its modal frame and the hulls drawer's
ways to open a shared build (`Pages.Build`):

  - a modal dialog closes with Escape (`modalKeyDecoder`), its close (X) button
    and a tap on the shade behind it, which covers the whole editor;
  - the share dialog shows the QR code from the start, with the build's link
    in a read-only field, the copy buttons and the BrickLink Studio download;
  - the hulls drawer has no "Choose Starter Hull" title any more, lists the
    saved builds, then the "Ready-made hulls" subtitle, then the starter
    hulls, and opens a build pasted from the clipboard (the paste button sits
    before the camera button) or into its text field.

-}

import Expect
import Html.Attributes as Attr
import I18n
import Json.Decode as Decode
import LDraw.Document as Document exposing (Document)
import Pages.Build as Build exposing (BuilderTab(..), Modal(..), Msg(..))
import Sails.Editor as Editor exposing (Base)
import Sails.I18n as T
import Sails.Ingest as Ingest
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


open : Base -> Build.Model
open base =
    Build.init
        { ldrawBase = ""
        , ldrawFallbackBase = ""
        , supportsPointerEvents = True
        , language = I18n.En
        , width = 390
        , height = 844
        }
        base
        base.document
        |> Tuple.first


step : Msg -> Build.Model -> Build.Model
step msg model =
    let
        ( next, _, _ ) =
            Build.update msg model
    in
    next


testId : String -> Selector.Selector
testId id =
    Selector.attribute (Attr.attribute "data-testid" id)


viewOf : Build.Model -> Query.Single Msg
viewOf model =
    Query.fromHtml (Build.view model)


suite : Test
suite =
    case Editor.bases of
        first :: second :: _ ->
            let
                opened =
                    open first

                shared =
                    step (SelectTab ShareTab) opened

                hulls =
                    step (SelectTab BasesTab) opened

                secondLink =
                    Ingest.encodeShipLink second.document
            in
            describe "Share dialog and hulls drawer"
                [ modalTests opened shared
                , shareDialogTests opened shared
                , hullsDrawerTests hulls
                , clipboardTests first second hulls secondLink
                ]

        _ ->
            test "has two starter hulls" (\_ -> Expect.fail "fewer than two starter hulls")


modalTests : Build.Model -> Build.Model -> Test
modalTests opened shared =
    describe "modal frame"
        [ test "the share tab opens a modal, the other tabs do not" <|
            \_ ->
                Expect.equal
                    [ Just ShareModal, Nothing, Nothing, Nothing ]
                    [ Build.topModal shared
                    , Build.topModal opened
                    , Build.topModal (step (SelectTab BasesTab) opened)
                    , Build.topModal (step (SelectTab ReviewTab) opened)
                    ]
        , test "the Escape key closes the modal" <|
            \_ ->
                case Decode.decodeString Build.modalKeyDecoder """{"key":"Escape","code":"Escape"}""" of
                    Ok msg ->
                        step msg shared
                            |> Expect.all
                                [ \m -> Expect.equal Nothing (Build.topModal m)
                                , \m -> Expect.equal BuildBricksTab m.activeTab
                                ]

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "other keys leave the modal open" <|
            \_ ->
                Decode.decodeString Build.modalKeyDecoder """{"key":"Enter"}"""
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "the close (X) button closes it" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "share-dialog" ]
                    |> Query.find [ testId "modal-close" ]
                    |> Event.simulate Event.click
                    |> Event.expect CloseModal
        , test "the close button is labelled" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "modal-close" ]
                    |> Query.has [ Selector.attribute (Attr.title (T.closeDialog I18n.En)) ]
        , test "a tap on the shade closes it" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "modal-backdrop" ]
                    |> Event.simulate Event.click
                    |> Event.expect CloseModal
        , test "the shade covers the whole editor above the top bar" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "modal-layer" ]
                    |> Query.has
                        [ Selector.style "position" "fixed"
                        , Selector.style "inset" "0"
                        , Selector.style "z-index" "60"
                        ]
        , test "closing it puts the editor back" <|
            \_ ->
                step CloseModal shared
                    |> viewOf
                    |> Query.findAll [ testId "modal-layer" ]
                    |> Query.count (Expect.equal 0)
        , test "CloseModal does nothing without a modal" <|
            \_ ->
                (step (SelectTab BasesTab) opened |> step CloseModal).activeTab
                    |> Expect.equal BasesTab
        ]


shareDialogTests : Build.Model -> Build.Model -> Test
shareDialogTests opened shared =
    describe "share dialog"
        [ test "the top bar has a Share tab instead of Open build" <|
            \_ ->
                viewOf opened
                    |> Query.find [ Selector.attribute (Attr.attribute "data-share-toggle" "") ]
                    |> Query.has [ Selector.attribute (Attr.title (T.shareTab I18n.En)) ]
        , test "the QR code is shown from the start" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "share-dialog" ]
                    |> Query.has [ testId "share-qr-code" ]
        , test "there is no QR toggle button any more" <|
            \_ ->
                viewOf shared
                    |> Query.findAll [ testId "share-qr-toggle" ]
                    |> Query.count (Expect.equal 0)
        , test "the read-only field holds the build's link" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "share-code" ]
                    |> Query.has
                        [ Selector.attribute (Attr.readonly True)
                        , Selector.attribute (Attr.value (Maybe.withDefault "" (Maybe.map Tuple.first shared.shareQr)))
                        ]
        , test "the link is the build's own" <|
            \_ ->
                Maybe.map Tuple.first shared.shareQr
                    |> Maybe.map (String.endsWith (Ingest.encodeShipLink (Build.document opened)))
                    |> Expect.equal (Just True)
        , test "the copy icon button copies the link" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "share-copy-icon" ]
                    |> Event.simulate Event.click
                    |> Event.expect CopyBuildCode
        , test "the Copy button copies the link" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "share-copy" ]
                    |> Expect.all
                        [ Query.has [ Selector.text (T.copyButton I18n.En) ]
                        , Event.simulate Event.click >> Event.expect CopyBuildCode
                        ]
        , test "copying says so" <|
            \_ ->
                step CopyBuildCode shared
                    |> viewOf
                    |> Query.find [ testId "share-copied" ]
                    |> Query.has [ Selector.text (T.shareCopied I18n.En) ]
        , test "the .io button downloads the build" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "share-download-io" ]
                    |> Expect.all
                        [ Query.has [ Selector.text (T.shareDownloadIo I18n.En) ]
                        , Event.simulate Event.click >> Event.expect DownloadIo
                        ]
        , test "the old paste field is not in the share dialog" <|
            \_ ->
                viewOf shared
                    |> Query.find [ testId "share-dialog" ]
                    |> Query.findAll [ testId "paste-input" ]
                    |> Query.count (Expect.equal 0)
        , test "a build too big for a QR code shows the too-large note" <|
            \_ ->
                let
                    ( big, _ ) =
                        Build.init
                            { ldrawBase = ""
                            , ldrawFallbackBase = ""
                            , supportsPointerEvents = True
                            , language = I18n.En
                            , width = 390
                            , height = 844
                            }
                            opened.base
                            bigBuild
                in
                step (SelectTab ShareTab) big
                    |> viewOf
                    |> Query.find [ testId "share-dialog" ]
                    |> Expect.all
                        [ Query.has [ testId "share-qr-too-large" ]
                        , Query.hasNot [ testId "share-qr-code" ]
                        ]
        ]


hullsDrawerTests : Build.Model -> Test
hullsDrawerTests hulls =
    describe "hulls drawer"
        [ test "the Choose Starter Hull title is gone" <|
            \_ ->
                viewOf hulls
                    |> Query.find [ testId "bases-drawer" ]
                    |> Query.hasNot [ Selector.text "Choose Starter Hull" ]
        , test "the Finnish title is gone too, and the subtitle is there" <|
            \_ ->
                Build.setLanguage I18n.Fi hulls
                    |> viewOf
                    |> Query.find [ testId "bases-drawer" ]
                    |> Expect.all
                        [ Query.hasNot [ Selector.text "Valitse aloitusrunko" ]
                        , Query.has [ Selector.text "Valmiit runkopohjat" ]
                        ]
        , test "saved builds come first, then the Ready-made hulls subtitle, then the hulls" <|
            \_ ->
                viewOf hulls
                    |> Query.find [ testId "bases-drawer" ]
                    |> Query.findAll [ Selector.attribute (Attr.attribute "data-hulls-section" "") ]
                    |> Expect.all
                        [ Query.count (Expect.equal 3)
                        , Query.index 0 >> Query.has [ testId "own-builds", Selector.text (T.buildHistoryHeading I18n.En) ]
                        , Query.index 1 >> Query.has [ testId "ready-made-hulls-heading", Selector.text (T.readyMadeHulls I18n.En) ]
                        , Query.index 2 >> Query.has [ testId "base-hulls" ]
                        ]
        , test "the starter hull cards are under the subtitle" <|
            \_ ->
                viewOf hulls
                    |> Query.find [ testId "base-hulls" ]
                    |> Query.findAll [ Selector.tag "button" ]
                    |> Query.count (Expect.equal (List.length Editor.bases))
        , test "the paste button sits right before the camera button" <|
            \_ ->
                viewOf hulls
                    |> Query.find [ testId "hulls-actions" ]
                    |> Query.findAll [ Selector.tag "button" ]
                    |> Expect.all
                        [ Query.count (Expect.equal 3)
                        , Query.index 1 >> Query.has [ testId "paste-clipboard", Selector.attribute (Attr.title (T.pasteFromClipboard I18n.En)) ]
                        , Query.index 2 >> Query.has [ testId "scan-qr" ]
                        ]
        , test "the paste button reads the clipboard" <|
            \_ ->
                viewOf hulls
                    |> Query.find [ testId "paste-clipboard" ]
                    |> Event.simulate Event.click
                    |> Event.expect PasteFromClipboard
        , test "the drawer keeps a field for a pasted link or LDraw text" <|
            \_ ->
                viewOf hulls
                    |> Query.find [ testId "paste-input" ]
                    |> Event.simulate (Event.input "#b/abc")
                    |> Event.expect (PasteTextChanged "#b/abc")
        ]


clipboardTests : Base -> Base -> Build.Model -> String -> Test
clipboardTests first second hulls secondLink =
    describe "opening a build from the clipboard"
        [ test "a ship link on the clipboard opens as the build, like the text field" <|
            \_ ->
                let
                    fromClipboard =
                        step (ClipboardPasted secondLink) hulls

                    fromField =
                        hulls |> step (PasteTextChanged secondLink) |> step SubmitPaste
                in
                Expect.all
                    [ \m -> Expect.equal second.id m.base.id
                    , \m -> Expect.equal secondLink (Ingest.encodeShipLink (Build.document m))
                    , \m -> Expect.equal BuildBricksTab m.activeTab
                    , \m -> Expect.equal Nothing m.pasteError
                    , \m -> Expect.equal (Ingest.encodeShipLink (Build.document fromField)) (Ingest.encodeShipLink (Build.document m))
                    ]
                    fromClipboard
        , test "LDraw text on the clipboard opens too" <|
            \_ ->
                step (ClipboardPasted "1 4 0 -24 0 1 0 0 0 1 0 0 0 1 3001.dat") hulls
                    |> Build.document
                    |> Document.flattenedPlacements
                    |> List.length
                    |> Expect.equal 1
        , test "clipboard text that is not a build keeps the build and shows the error" <|
            \_ ->
                let
                    pasted =
                        step (ClipboardPasted "hello there") hulls
                in
                Expect.all
                    [ \m -> Expect.equal first.id m.base.id
                    , \m -> Expect.equal BasesTab m.activeTab
                    , \m -> Expect.equal "hello there" m.pasteText
                    , \m -> Expect.notEqual Nothing m.pasteError
                    , \m -> viewOf m |> Query.has [ testId "paste-error" ]
                    ]
                    pasted
        , test "an empty clipboard says so" <|
            \_ ->
                (step (ClipboardPasted "  ") hulls).pasteError
                    |> Expect.equal (Just (T.clipboardEmpty I18n.En))
        , test "a refused clipboard permission says so" <|
            \_ ->
                (step (ClipboardFailed "denied") hulls).pasteError
                    |> Expect.equal (Just (T.clipboardDenied I18n.En))
        , test "a browser without the clipboard API says so" <|
            \_ ->
                (step (ClipboardFailed "unsupported") hulls).pasteError
                    |> Expect.equal (Just (T.clipboardUnavailable I18n.En))
        ]


{-| 1000 bricks at scattered places: far too long a link for a QR code (as in
`QrShareTest`).
-}
bigBuild : Document
bigBuild =
    List.range 0 999
        |> List.map
            (\i ->
                String.join " "
                    [ "1"
                    , String.fromInt (modBy 70 (i * 7))
                    , String.fromInt (modBy 997 (i * 131) * 20)
                    , String.fromInt (modBy 89 (i * 17) * -24)
                    , String.fromInt (modBy 991 (i * 37) * 20)
                    , "1 0 0 0 1 0 0 0 1"
                    , [ "3001.dat", "3004.dat", "3023.dat", "3069b.dat", "3039.dat" ] |> List.drop (modBy 5 i) |> List.head |> Maybe.withDefault "3001.dat"
                    ]
            )
        |> String.join "\n"
        |> Document.parse
