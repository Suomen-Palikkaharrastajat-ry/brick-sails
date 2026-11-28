module Sails.QrShare exposing
    ( QrCode(..)
    , ScanRejection(..)
    , defaultShareBase
    , maxQrLength
    , parseScanned
    , qrCode
    , shareUrl
    )

{-| Moving a build from one device to another by QR code: the desktop shows
the build link as a QR code (`qrCode`), the phone scans it with the camera of
the shared runtime (`qr-camera-scanner.js`) and opens it in the ship editor
(`parseScanned`).


# Showing a build

The QR code holds the page link with the build, `https://.../#b/<base64url>`
(`shareUrl`), so the phone's own camera app opens it too. It is drawn with
`pablohirafuji/elm-qrcode` at error correction level Low, the level that holds
the most: the code is read off a screen, not a worn sticker.

A big build makes a dense code that a phone camera cannot read off a screen,
or no code at all. A link longer than `maxQrLength` characters is not drawn
(`TooLong`); the player copies the link instead.


# Reading a scanned code

The scanner passes on `#b/...` when the code holds one directly, and the
whole text otherwise (a link with other parts, `#map=...&b/...`, or anything
else). `parseScanned` takes either and gives the hash to decode, or why the
code is not a ship build:

  - a Brick Racer (`pienajot.`) or Brick Play (`rakenna.`) link, by its host;
  - a Brick Play room route (`#/play/...`, `#/watch/...`, `#/grid/...`);
  - a room invitation (`room=`);
  - anything that is not a `b/...` or `model=...` build link.

The hash then goes through the editor's own decoder (`Sails.Ingest`), like a
pasted link.

-}

import QRCode


{-| The longest link drawn as a QR code: 1273 characters, the byte-mode
capacity of a version 25 code (117 × 117 modules) at error correction level
Low (ISO/IEC 18004, the table of data capacities). The format goes up to
version 40 (2953 bytes at Low), but codes past version 25 are too dense to
read reliably with a phone camera pointed at a screen.
-}
maxQrLength : Int
maxQrLength =
    1273


{-| The page the link opens when the app does not know its own address.
-}
defaultShareBase : String
defaultShareBase =
    "https://ohoi.palikkaharrastajat.fi/"


{-| The QR code of a link, or why there is none: the link is longer than
`maxQrLength` (its length is given), or the encoder refused it.
-}
type QrCode
    = QrReady QRCode.QRCode
    | TooLong Int


{-| The page link of a build hash (`#b/...`, from `Sails.Ingest.encodeShipLink`)
on the page at `base` (any old hash of `base` is dropped). An empty base is
the public site.
-}
shareUrl : String -> String -> String
shareUrl base hash =
    let
        page =
            case String.split "#" (String.trim base) of
                first :: _ ->
                    first

                [] ->
                    ""
    in
    (if String.isEmpty page then
        defaultShareBase

     else
        page
    )
        ++ hash


{-| The QR code of `link`, unless it is too long to scan.
-}
qrCode : String -> QrCode
qrCode link =
    let
        length =
            String.length link
    in
    if length > maxQrLength then
        TooLong length

    else
        case QRCode.fromStringWith QRCode.Low link of
            Ok code ->
                QrReady code

            Err _ ->
                TooLong length


{-| Why a scanned code is not opened as a ship build.
-}
type ScanRejection
    = RacerLink
    | PlayLink
    | RoomLink
    | NotABuildLink


{-| The build hash in a scanned code, `#b/...` or `#model=...` (without any
`map=` part), or why there is none.
-}
parseScanned : String -> Result ScanRejection String
parseScanned raw =
    let
        trimmed =
            String.trim raw

        lower =
            String.toLower trimmed
    in
    case hostOf lower of
        Just host ->
            if String.startsWith "pienajot." host then
                Err RacerLink

            else if String.startsWith "rakenna." host then
                Err PlayLink

            else
                case String.indexes "#" trimmed of
                    i :: _ ->
                        fromFragment (String.dropLeft (i + 1) trimmed)

                    [] ->
                        Err NotABuildLink

        Nothing ->
            if String.startsWith "#" trimmed then
                fromFragment (String.dropLeft 1 trimmed)

            else
                fromFragment trimmed


{-| The host of a `http://` or `https://` link, lower case and without its
port.
-}
hostOf : String -> Maybe String
hostOf lower =
    let
        rest =
            if String.startsWith "https://" lower then
                Just (String.dropLeft 8 lower)

            else if String.startsWith "http://" lower then
                Just (String.dropLeft 7 lower)

            else
                Nothing

        cutAt sep s =
            String.split sep s |> List.head |> Maybe.withDefault s
    in
    rest |> Maybe.map (cutAt "/" >> cutAt "#" >> cutAt "?" >> cutAt ":")


fromFragment : String -> Result ScanRejection String
fromFragment fragment =
    let
        segments =
            String.split "&" fragment |> List.map String.trim

        lowerSegments =
            List.map String.toLower segments

        build =
            List.filter (\seg -> not (String.startsWith "map=" (String.toLower seg))) segments
    in
    if List.any (\seg -> String.startsWith "/play/" seg || String.startsWith "/watch/" seg || String.startsWith "/grid/" seg) lowerSegments then
        Err PlayLink

    else if List.any (String.startsWith "room=") lowerSegments then
        Err RoomLink

    else
        case List.filter (\seg -> String.startsWith "b/" seg && isBase64Url (String.dropLeft 2 seg)) build of
            binary :: _ ->
                Ok ("#" ++ binary)

            [] ->
                if List.any (\seg -> String.startsWith "model=" seg && String.length seg > 6) build then
                    Ok ("#" ++ String.join "&" build)

                else
                    Err NotABuildLink


isBase64Url : String -> Bool
isBase64Url s =
    not (String.isEmpty s) && String.all (\c -> Char.isAlphaNum c || c == '-' || c == '_') s
