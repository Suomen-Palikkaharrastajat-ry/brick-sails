module Sails.Ingest exposing
    ( DecodedShip
    , decodeShipInput
    , hasShipPayload
    , ingestDocument
    , ingestFromPlacements
    , isSailPart
    , maxShipParts
    , validateShip
    )

{-| Load ships built in the brick building apps and check that they float.

Accepts the same inputs as the other brick games of the family, decoded with
the same vendor code:

  - binary `#b/<base64url>` build links (bytes decoded in JS by
    `url-hash-codec.js`, or the base64 text decoded here), read by
    `Studio.HashPayload`,
  - text `#model=...&parts=...` links, read by `Studio.UrlPayload`,
  - raw LDraw `.ldr` / `.mpd` text from a file or a paste.

`ingestDocument` then sorts the parts into hull, mast, sail and fittings, and
`validateShip` turns that into a short error when the build is not a boat.

-}

import Base64
import Flate
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Analyze as Analyze exposing (Seaworthiness(..))
import Sails.Types exposing (ShipSpec, lduToMeters)
import Studio.HashPayload as HashPayload
import Studio.UrlPayload as UrlPayload


{-| A decoded build and a title for it.
-}
type alias DecodedShip =
    { document : Document
    , title : String
    }


{-| Builds with more parts than this are refused: they would be slow to load
and to draw on a phone.
-}
maxShipParts : Int
maxShipParts =
    600


{-| Whether a URL or hash carries a build (`#b/...` or `#model=...`).
-}
hasShipPayload : String -> Bool
hasShipPayload raw =
    let
        fragment =
            HashPayload.extractFragment (String.trim raw)
    in
    (HashPayload.parseDirectBase64 raw /= Nothing)
        || String.startsWith "model=" fragment
        || String.contains "&model=" fragment


{-| Decode a build from bytes already inflated by JS (if any) or from a URL,
a hash or LDraw text. Errors are short sentences to show the player.
-}
decodeShipInput : Maybe (List Int) -> String -> Result String DecodedShip
decodeShipInput maybeBytes rawInput =
    let
        trimmed =
            String.trim rawInput
    in
    case maybeBytes |> Maybe.andThen HashPayload.decodeBytes of
        Just payload ->
            Ok (named payload.document)

        Nothing ->
            if String.isEmpty trimmed then
                Err "Paste a build link first."

            else if isRawLdrText trimmed then
                Ok (named (Document.parse trimmed))

            else
                case HashPayload.parseDirectBase64 trimmed of
                    Just b64 ->
                        case decodeBase64Payload b64 of
                            Just doc ->
                                Ok (named doc)

                            Nothing ->
                                Err "That build link is damaged and could not be read."

                    Nothing ->
                        case (UrlPayload.decodeHash (HashPayload.extractFragment trimmed)).modelText of
                            Just text ->
                                Ok (named (Document.parse text))

                            Nothing ->
                                Err "That is not a build link. Copy the link from the building app."


named : Document -> DecodedShip
named doc =
    { document = doc, title = modelTitle doc }


{-| Check that a decoded build can sail: it has bricks, is not too big for a
phone, and has enough hull to float. Returns the ship's specification.
-}
validateShip : Document -> Result String ShipSpec
validateShip doc =
    let
        placements =
            Document.flattenedPlacements doc

        count =
            List.length placements
    in
    if count == 0 then
        Err "That build has no bricks."

    else if count > maxShipParts then
        Err ("That build is too big to sail here (" ++ String.fromInt count ++ " parts, at most " ++ String.fromInt maxShipParts ++ ").")

    else
        let
            spec =
                ingestDocument (modelTitle doc) doc
        in
        case (Analyze.analyzeShip spec).seaworthiness of
            Unseaworthy reason ->
                Err ("Not a boat: " ++ reason)

            _ ->
                Ok spec


isRawLdrText : String -> Bool
isRawLdrText input =
    String.lines input
        |> List.any
            (\line ->
                let
                    t =
                        String.trim line
                in
                String.startsWith "1 " t || String.startsWith "0 FILE" t || String.startsWith "0 Name:" t
            )


decodeBase64Payload : String -> Maybe Document
decodeBase64Payload b64 =
    Base64.toBytes (HashPayload.base64UrlToStandard b64)
        |> Maybe.andThen Flate.inflate
        |> Maybe.andThen HashPayload.decode
        |> Maybe.map .document


modelTitle : Document -> String
modelTitle doc =
    let
        main =
            Document.mainSubmodel doc

        fromMeta entry =
            case entry of
                Document.Raw raw ->
                    let
                        t =
                            String.trim raw
                    in
                    if String.startsWith "0 Name:" t then
                        Just (String.trim (String.dropLeft 7 t))

                    else
                        Nothing

                Document.PlacementEntry _ ->
                    Nothing

        title =
            List.filterMap fromMeta main.entries
                |> List.head
                |> Maybe.withDefault main.name
    in
    if String.endsWith ".ldr" (String.toLower title) || String.endsWith ".mpd" (String.toLower title) then
        String.dropRight 4 title

    else
        title


type PartCategory
    = Hull
    | Mast
    | Sail
    | RudderKeel
    | DeckFitting
    | Cannon


classifyPart : String -> Float -> PartCategory
classifyPart rawName yPos =
    let
        p =
            String.toLower rawName
    in
    if String.contains "32828" p || String.contains "cannon" p || String.contains "shooting" p then
        Cannon

    else if String.contains "antenna" p || String.contains "3957" p || String.contains "bar" p || String.contains "pole" p || String.contains "axle" p then
        Mast

    else if String.contains "flag" p || String.contains "2335" p || String.contains "sail" p || String.contains "cloth" p || String.contains "wedge" p then
        Sail

    else if String.contains "rudder" p || String.contains "fin" p || yPos > 10.0 then
        RudderKeel

    else if String.contains "slope" p || String.contains "boat" p || String.contains "3660" p || String.contains "3003" p || String.contains "brick" p then
        Hull

    else
        DeckFitting


{-| Whether a part file is a sail (flags, cloth, sails and wedge plates).
Used to give another player's copy of a loaded build its player colour.
-}
isSailPart : String -> Bool
isSailPart part =
    classifyPart part 0 == Sail


{-| Ingest an LDraw document into a `ShipSpec`.
-}
ingestDocument : String -> Document -> ShipSpec
ingestDocument name doc =
    let
        partPlacements =
            Document.flattenedPlacements doc
                |> List.map (\p -> ( p.part, Mat4.transform p.matrix (Vec3.vec3 0 0 0) ))
    in
    ingestFromPlacements name partPlacements


{-| Pure derivation of `ShipSpec` from part names and (x, y, z) positions in LDU coordinates.
-}
ingestFromPlacements : String -> List ( String, Vec3 ) -> ShipSpec
ingestFromPlacements name placements =
    if List.isEmpty placements then
        defaultFallbackSpec name

    else
        let
            positions =
                List.map (Tuple.second >> Vec3.toRecord) placements

            xs =
                List.map .x positions

            ys =
                List.map .y positions

            zs =
                List.map .z positions

            minX =
                List.minimum xs |> Maybe.withDefault -40.0

            maxX =
                List.maximum xs |> Maybe.withDefault 40.0

            minY =
                List.minimum ys |> Maybe.withDefault -120.0

            maxY =
                List.maximum ys |> Maybe.withDefault 0.0

            minZ =
                List.minimum zs |> Maybe.withDefault -20.0

            maxZ =
                List.maximum zs |> Maybe.withDefault 80.0

            -- Convert LDU bounding dimensions to meters
            beamMeters =
                max 1.2 ((maxX - minX + 20.0) * lduToMeters)

            mastHeightMeters =
                max 2.0 ((maxY - minY) * lduToMeters)

            lengthMeters =
                max 2.0 ((maxZ - minZ + 20.0) * lduToMeters)

            draftMeters =
                max 0.4 (beamMeters * 0.35)

            totalCount =
                List.length placements

            categorized =
                placements
                    |> List.map (\( part, pos ) -> ( classifyPart part (Vec3.toRecord pos).y, Vec3.toRecord pos ))

            hullCount =
                categorized |> List.filter (\( cat, _ ) -> cat == Hull || cat == DeckFitting) |> List.length

            sailCount =
                categorized |> List.filter (\( cat, _ ) -> cat == Sail) |> List.length

            mastCount =
                categorized |> List.filter (\( cat, _ ) -> cat == Mast) |> List.length

            cannonCount =
                categorized |> List.filter (\( cat, _ ) -> cat == Cannon) |> List.length

            -- Center of buoyancy is centered in the hull volume
            midX =
                (minX + maxX) / 2.0 * lduToMeters

            waterlineY =
                0.0

            midZ =
                (minZ + maxZ) / 2.0 * lduToMeters

            centerOfBuoyancy =
                vec3 midX (waterlineY - draftMeters * 0.5) midZ

            -- Sail Center of Effort (CE): average position of sail parts, or fallback to 60% mast height
            sailPositions =
                categorized
                    |> List.filterMap
                        (\( cat, pos ) ->
                            if cat == Sail || (cat == Mast && pos.y < (minY + maxY) / 2.0) then
                                Just pos

                            else
                                Nothing
                        )

            centerOfEffort =
                if List.isEmpty sailPositions then
                    vec3 midX (waterlineY + mastHeightMeters * 0.55) midZ

                else
                    let
                        avgY =
                            (List.sum (List.map .y sailPositions) / toFloat (List.length sailPositions)) * lduToMeters

                        avgZ =
                            (List.sum (List.map .z sailPositions) / toFloat (List.length sailPositions)) * lduToMeters
                    in
                    vec3 midX -avgY avgZ

            -- Center of lateral resistance (CLR) in the underwater hull/keel
            centerOfLateralResistance =
                vec3 midX (waterlineY - draftMeters * 0.6) midZ

            -- Total displacement mass estimated from volume and part count (e.g. 350-1200 kg)
            -- Cannons add concentrated low-deck ballast weight
            mass =
                max 250.0 (toFloat totalCount * 45.0 + lengthMeters * beamMeters * 80.0 + toFloat cannonCount * 35.0)

            -- Sail area: flags/wedge plates provide ~1.5 - 3.5 m² per piece in scale
            sailAreaSqMeters =
                max 4.0 (toFloat (max 1 sailCount) * 2.8 + mastHeightMeters * 0.8)

            -- Metacentric height GM = KB + BM - KG
            -- Beam gives transverse metacentric radius BM ~ B^2 / (12 * Draft)
            bm =
                (beamMeters * beamMeters) / (12.0 * max 0.2 draftMeters)

            gm =
                max 0.6 (bm * 0.85 + 0.3)
        in
        { name = name
        , mass = mass
        , lengthMeters = lengthMeters
        , beamMeters = beamMeters
        , draftMeters = draftMeters
        , mastHeightMeters = mastHeightMeters
        , sailAreaSqMeters = sailAreaSqMeters
        , centerOfBuoyancy = centerOfBuoyancy
        , centerOfEffort = centerOfEffort
        , centerOfLateralResistance = centerOfLateralResistance
        , metacentricHeight = gm
        , hullPartCount = hullCount
        , sailPartCount = sailCount
        , mastPartCount = mastCount
        , cannonCount = cannonCount
        , totalPartCount = totalCount
        }


defaultFallbackSpec : String -> ShipSpec
defaultFallbackSpec name =
    { name = name
    , mass = 450.0
    , lengthMeters = 3.6
    , beamMeters = 1.6
    , draftMeters = 0.5
    , mastHeightMeters = 4.8
    , sailAreaSqMeters = 6.5
    , centerOfBuoyancy = vec3 0.0 -0.2 0.0
    , centerOfEffort = vec3 0.0 2.4 0.0
    , centerOfLateralResistance = vec3 0.0 -0.3 0.0
    , metacentricHeight = 1.2
    , hullPartCount = 5
    , sailPartCount = 1
    , mastPartCount = 1
    , cannonCount = 2
    , totalPartCount = 8
    }
