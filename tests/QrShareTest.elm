module QrShareTest exposing (suite)

{-| Moving a build between devices by QR code (`Sails.QrShare`, `Pages.Build`):
a link is drawn as a QR code only while it fits a code a phone can read
(`maxQrLength`), and a scanned code opens in the ship editor only when it is
a ship build link: `#b/...` alone or in a page link, not a Brick Racer or
Brick Play link, a room invitation or other text.
-}

import Expect
import I18n
import LDraw.Document as Document exposing (Document)
import Pages.Build as Build exposing (BuilderTab(..), Msg(..))
import QRCode
import Sails.Editor as Editor exposing (Base)
import Sails.I18n as T
import Sails.Ingest as Ingest
import Sails.QrShare as QrShare exposing (QrCode(..), ScanRejection(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Build by QR code"
        [ sizeGuardTests
        , shareUrlTests
        , scanTests
        , editorTests
        ]


sizeGuardTests : Test
sizeGuardTests =
    describe "QR size guard"
        [ test "a link of exactly maxQrLength characters fits a version 25 code" <|
            \_ ->
                case QrShare.qrCode (linkOfLength QrShare.maxQrLength) of
                    QrReady code ->
                        QRCode.version code |> Expect.atMost 25

                    TooLong n ->
                        Expect.fail ("refused at " ++ String.fromInt n)
        , test "one character more is refused with its length" <|
            \_ ->
                QrShare.qrCode (linkOfLength (QrShare.maxQrLength + 1))
                    |> Expect.equal (TooLong (QrShare.maxQrLength + 1))
        , test "a short link gives a small code" <|
            \_ ->
                case QrShare.qrCode "https://ohoi.palikkaharrastajat.fi/#b/abc" of
                    QrReady code ->
                        QRCode.version code |> Expect.atMost 5

                    TooLong _ ->
                        Expect.fail "short link refused"
        , describe "every starter hull fits a QR code"
            (List.map
                (\base ->
                    test base.id <|
                        \_ ->
                            case QrShare.qrCode (QrShare.shareUrl QrShare.defaultShareBase (Ingest.encodeShipLink base.document)) of
                                QrReady _ ->
                                    Expect.pass

                                TooLong n ->
                                    Expect.fail ("link of " ++ String.fromInt n ++ " characters")
                )
                Editor.bases
            )
        , test "a big build is too long for a QR code" <|
            \_ ->
                case QrShare.qrCode (QrShare.shareUrl QrShare.defaultShareBase (Ingest.encodeShipLink bigBuild)) of
                    TooLong n ->
                        n |> Expect.greaterThan QrShare.maxQrLength

                    QrReady _ ->
                        Expect.fail "a 1000-part scattered build should not fit"
        ]


shareUrlTests : Test
shareUrlTests =
    describe "shareUrl"
        [ test "puts the hash on the page" <|
            \_ ->
                QrShare.shareUrl "http://localhost:5177/" "#b/abc"
                    |> Expect.equal "http://localhost:5177/#b/abc"
        , test "drops the page's old hash" <|
            \_ ->
                QrShare.shareUrl "https://ohoi.palikkaharrastajat.fi/#map=x&b/old" "#b/new"
                    |> Expect.equal "https://ohoi.palikkaharrastajat.fi/#b/new"
        , test "an empty page is the public site" <|
            \_ ->
                QrShare.shareUrl "" "#b/abc"
                    |> Expect.equal "https://ohoi.palikkaharrastajat.fi/#b/abc"
        , test "a starter hull's shared link scans back to its build hash" <|
            \_ ->
                case Editor.bases of
                    base :: _ ->
                        let
                            hash =
                                Ingest.encodeShipLink base.document
                        in
                        QrShare.parseScanned (QrShare.shareUrl QrShare.defaultShareBase hash)
                            |> Expect.equal (Ok hash)

                    [] ->
                        Expect.fail "no starter hulls"
        ]


scanTests : Test
scanTests =
    describe "parseScanned"
        [ describe "accepts ship build links"
            (List.map
                (\( raw, expected ) -> test raw <| \_ -> QrShare.parseScanned raw |> Expect.equal (Ok expected))
                [ ( "#b/abc_-09", "#b/abc_-09" )
                , ( "  #b/abc  ", "#b/abc" )
                , ( "b/abc", "#b/abc" )
                , ( "https://ohoi.palikkaharrastajat.fi/#b/abc", "#b/abc" )
                , ( "https://ohoi.palikkaharrastajat.fi/#map=boreal-winter-snow-42&b/abc", "#b/abc" )
                , ( "HTTPS://OHOI.PALIKKAHARRASTAJAT.FI/#b/abc", "#b/abc" )
                , ( "http://localhost:5177/?lang=en#b/abc", "#b/abc" )
                , ( "https://ohoi.palikkaharrastajat.fi/#model=1%204%200&parts=3001", "#model=1%204%200&parts=3001" )
                ]
            )
        , describe "rejects Brick Racer links"
            (List.map
                (\raw -> test raw <| \_ -> QrShare.parseScanned raw |> Expect.equal (Err RacerLink))
                [ "https://pienajot.palikkaharrastajat.fi/#map=boreal-winter-snow-42&b/abc"
                , "https://pienajot.palikkaharrastajat.fi/#b/abc"
                , "http://pienajot.palikkaharrastajat.fi:443/#room=ABCD"
                ]
            )
        , describe "rejects Brick Play links"
            (List.map
                (\raw -> test raw <| \_ -> QrShare.parseScanned raw |> Expect.equal (Err PlayLink))
                [ "https://rakenna.palikkaharrastajat.fi/#b/abc"
                , "https://rakenna.palikkaharrastajat.fi/#/play/ROOM1/b/abc"
                , "#/play/ROOM1/b/abc"
                , "https://example.org/#/watch/ROOM1"
                , "#/grid/ROOM1"
                ]
            )
        , describe "rejects room invitations"
            (List.map
                (\raw -> test raw <| \_ -> QrShare.parseScanned raw |> Expect.equal (Err RoomLink))
                [ "https://ohoi.palikkaharrastajat.fi/#room=ABCD"
                , "#room=ABCD"
                ]
            )
        , describe "rejects other text"
            (List.map
                (\raw -> test ("\"" ++ raw ++ "\"") <| \_ -> QrShare.parseScanned raw |> Expect.equal (Err NotABuildLink))
                [ ""
                , "   "
                , "hello sailor"
                , "https://example.org/"
                , "https://ohoi.palikkaharrastajat.fi/#map=boreal-winter-snow-42"
                , "#b/"
                , "#b/abc def!"
                , "#model="
                , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                ]
            )
        ]


editorTests : Test
editorTests =
    case Editor.bases of
        first :: second :: _ ->
            let
                opened =
                    open first

                secondLink =
                    QrShare.shareUrl QrShare.defaultShareBase (Ingest.encodeShipLink second.document)
            in
            describe "ship editor"
                [ test "a scanned ship link opens as the build, on its hull" <|
                    \_ ->
                        opened
                            |> step (SelectTab BasesTab)
                            |> step (ScannedCode secondLink)
                            |> Expect.all
                                [ \m -> Expect.equal second.id m.base.id
                                , \m -> Expect.equal (Ingest.encodeShipLink second.document) (Ingest.encodeShipLink (Build.document m))
                                , \m -> Expect.equal (Just (T.qrScanOpened I18n.En)) m.notice
                                , \m -> Expect.equal BuildBricksTab m.activeTab
                                ]
                , test "a scanned racer link leaves the build and says why" <|
                    \_ ->
                        opened
                            |> step (ScannedCode ("https://pienajot.palikkaharrastajat.fi/" ++ Ingest.encodeShipLink second.document))
                            |> Expect.all
                                [ \m -> Expect.equal first.id m.base.id
                                , \m -> Expect.equal (Just (T.qrScanRacerLink I18n.En)) m.notice
                                ]
                , test "a scanned build that is not a boat leaves the build and says why" <|
                    \_ ->
                        opened
                            |> step (ScannedCode (Ingest.encodeShipLink oneBrick))
                            |> Expect.all
                                [ \m -> Expect.equal (Ingest.encodeShipLink first.document) (Ingest.encodeShipLink (Build.document m))
                                , \m -> Expect.equal (Just (T.qrScanNotAShip I18n.En)) m.notice
                                ]
                , test "a damaged build link is not a ship" <|
                    \_ ->
                        opened
                            |> step (ScannedCode "#b/AAAA")
                            |> .notice
                            |> Expect.equal (Just (T.qrScanNotAShip I18n.En))
                , test "Share as QR shows the build's link as a QR code" <|
                    \_ ->
                        case (opened |> step (SelectTab ImportBuildTab) |> step ToggleShareQr).shareQr of
                            Just ( link, QrReady _ ) ->
                                Expect.equal (QrShare.shareUrl QrShare.defaultShareBase (Ingest.encodeShipLink first.document)) link

                            _ ->
                                Expect.fail "no QR code"
                , test "the QR code uses the app's own address" <|
                    \_ ->
                        (Build.setShareBase "http://localhost:5177/" opened |> step (SelectTab ImportBuildTab) |> step ToggleShareQr).shareQr
                            |> Maybe.map Tuple.first
                            |> Maybe.map (String.startsWith "http://localhost:5177/#b/")
                            |> Expect.equal (Just True)
                , test "closing the drawer hides the QR code" <|
                    \_ ->
                        (opened |> step (SelectTab ImportBuildTab) |> step ToggleShareQr |> step (SelectTab ImportBuildTab)).shareQr
                            |> Expect.equal Nothing
                ]

        _ ->
            test "has two starter hulls" (\_ -> Expect.fail "fewer than two starter hulls")


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


{-| A link-like text of `n` characters, lower case as a real build link is
(byte mode).
-}
linkOfLength : Int -> String
linkOfLength n =
    let
        prefix =
            "https://ohoi.palikkaharrastajat.fi/#b/"
    in
    prefix ++ String.repeat (n - String.length prefix) "q"


oneBrick : Document
oneBrick =
    Document.parse "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"


{-| 1000 bricks of five kinds at scattered places (its link is about 1800
characters; starter hulls are 110 to 370).
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
