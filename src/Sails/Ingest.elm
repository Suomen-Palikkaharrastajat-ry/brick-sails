module Sails.Ingest exposing
    ( DecodedShip
    , SharedShip(..)
    , decodeSharedShip
    , decodeShipInput
    , encodeMapHash
    , encodeShipLink
    , hasShipPayload
    , hashWithoutMap
    , ingestDocument
    , ingestFromPlacements
    , isCannonPart
    , isMastPart
    , isSailPart
    , mapFromHash
    , maxShipParts
    , sailRule
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

A link may also name the map to sail on: `#map=<code>` on its own, or in
front of a build, `#map=<code>&b/...` (`mapFromHash`, `encodeMapHash`). The
map part is left out before a build is decoded (`hashWithoutMap`).

`ingestDocument` then sorts the parts into hull, mast, sail and fittings, and
`validateShip` turns that into a short error when the build is not a boat.

-}

import Base64
import Flate
import Game.MapParams as MapParams exposing (MapParams)
import LDraw.Document as Document exposing (Document, Placement)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Analyze as Analyze exposing (Seaworthiness(..))
import Sails.Boats as Boats
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
                        case (UrlPayload.decodeHash (hashWithoutMap trimmed)).modelText of
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


{-| What another player shared as their ship in a multiplayer room: the name
of a built-in boat, or a build (a build link or LDraw text).
-}
type SharedShip
    = SharedBoat String
    | SharedBuild DecodedShip
    | SharedUnusable String


{-| Read a ship another player shared. A build goes through the same checks
as a loaded one (`validateShip`, at most `maxShipParts` parts), so remote
boats are built exactly like "Your ship".
-}
decodeSharedShip : String -> SharedShip
decodeSharedShip payload =
    if List.any (\b -> b.name == payload) Boats.all then
        SharedBoat payload

    else
        case
            decodeShipInput Nothing payload
                |> Result.andThen (\decoded -> validateShip decoded.document |> Result.map (\_ -> decoded))
        of
            Ok decoded ->
                SharedBuild decoded

            Err err ->
                SharedUnusable err


{-| Encode a build as a compact `#b/...` build link, the form a loaded ship
is shared in with other players.
-}
encodeShipLink : Document -> String
encodeShipLink doc =
    HashPayload.encode { mode = HashPayload.FreeBuilding, document = doc, kit = Nothing }
        |> Flate.deflate
        |> Base64.fromBytes
        |> Maybe.map (\b64 -> "#b/" ++ HashPayload.standardToBase64Url b64)
        |> Maybe.withDefault ""


{-| The map a link names with `map=<code>`, if it names a valid one.
-}
mapFromHash : String -> Maybe MapParams
mapFromHash raw =
    HashPayload.extractFragment (String.trim raw)
        |> String.split "&"
        |> List.map String.trim
        |> List.filterMap
            (\seg ->
                if String.startsWith "map=" (String.toLower seg) then
                    MapParams.fromCode (String.dropLeft 4 seg)

                else
                    Nothing
            )
        |> List.head


{-| A link's fragment without its `map=` part, for the build decoders.
-}
hashWithoutMap : String -> String
hashWithoutMap raw =
    HashPayload.extractFragment (String.trim raw)
        |> String.split "&"
        |> List.filter (\seg -> not (String.startsWith "map=" (String.toLower (String.trim seg))))
        |> String.join "&"


{-| The page link of a map, with the player's build when there is one:
`#map=<code>` or `#map=<code>&b/...`.
-}
encodeMapHash : MapParams -> Maybe Document -> String
encodeMapHash map maybeDoc =
    "#map="
        ++ MapParams.toCode map
        ++ (case maybeDoc of
                Just doc ->
                    "&" ++ String.dropLeft 1 (encodeShipLink doc)

                Nothing ->
                    ""
           )


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
Part of `sailRule`.
-}
isSailPart : String -> Bool
isSailPart part =
    classifyPart part 0 == Sail


{-| Whether a part file is a mast (antennas, bars, poles, axles), as
`ingestDocument` counts masts.
-}
isMastPart : String -> Bool
isMastPart part =
    classifyPart part 0 == Mast


{-| Whether a part file is a cannon (`32828`, or a name with "cannon" or
"shooting"), as `ingestDocument` counts cannons.
-}
isCannonPart : String -> Bool
isCannonPart part =
    classifyPart part 0 == Cannon


{-| The one sail rule of the game, for player builds, other players' copies
and the computer's frigate and sloop alike: which flattened placements of a
ship go into `Sails.ShipMesh`'s `sails` scene (drawn apart, lowered and
raised with the sail level, and painted in a player or faction colour).

A placement is a sail when

  - its part name says so (`isSailPart`: flags, cloth, sails, wedge plates), or
  - it is a white (colour 15) 1 x N brick (1 x 1 up to 1 x 16) standing above
    the hull's deck.

"Above the deck" is decided by height alone, in the build's LDraw units
(Y down): the deck is one plate (8 LDU) above the top of the hull's lowest
layer, that is above the highest origin y among the parts that are not white
1 x N bricks (a brick's or plate's origin is its top face). This is the deck
of a starter hull (one brick and a plate) and lies under every deck of the
NPC ships. A white brick is a sail when its lowest point is at or above that
deck: standing on the deck, stacked on other bricks, or hanging from a yard.
A white brick down in the hull layers stays hull. (A white hull more than one
brick and a plate tall would have its upper white bricks taken as sails.)

Give it the whole ship's placements once and use the returned test for each
placement: the deck height is worked out once.

-}
sailRule : List Placement -> Placement -> Bool
sailRule placements =
    let
        deck =
            placements
                |> List.filter (not << isWhiteBrick)
                |> List.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)))
                |> List.maximum
                |> Maybe.map (\top -> top - 8)
    in
    \p ->
        isSailPart p.part
            || (case deck of
                    Just deckY ->
                        isWhiteBrick p && lowestPointY p <= deckY + 0.5

                    Nothing ->
                        False
               )


{-| A white (colour 15) 1 x N brick: the stacked square sails of the ships.
-}
isWhiteBrick : Placement -> Bool
isWhiteBrick p =
    p.color == 15 && List.member (partId p.part) oneByNBricks


oneByNBricks : List String
oneByNBricks =
    [ "3005", "3005b", "3004", "3004b", "3622", "3010", "3009", "3008", "6111", "6112", "2465" ]


partId : String -> String
partId name =
    String.toLower name
        |> String.replace "parts/" ""
        |> String.replace ".dat" ""


{-| The lowest point of a brick placement in LDraw y (larger is lower): the
brick's top face is its origin and its bottom is 24 LDU under it.
-}
lowestPointY : Placement -> Float
lowestPointY p =
    max (Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)))
        (Vec3.getY (Mat4.transform p.matrix (vec3 0 24 0)))


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
