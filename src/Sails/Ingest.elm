module Sails.Ingest exposing
    ( defaultShipHash
    , ingestDocument
    , ingestFromPlacements
    , presets
    )

{-| Ingest LDraw models into physics-ready `ShipSpec` specifications.
Detects hull displacement volume, masts, sails, rudders, and metacentric stability.
-}

import Dict exposing (Dict)
import LDraw.Document as Document exposing (Document)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Types exposing (ShipSpec, lduToMeters)


defaultShipHash : String
defaultShipHash =
    "#b/FYixDYMwEADvwR-lI0XK7JBXXjaO6ChooaKHKRiHkg1YD1PcnXT9IDzUU7I1uJmrW8pr-LlH9X9sF81tzBZmPkDDs5jp-waBrQBdVdKMt9mlruCUMdRwCFrmiws"


{-| Built-in ship presets.
-}
presets : List { name : String, hash : String, description : String }
presets =
    [ { name = "Micro Sloop"
      , hash = defaultShipHash
      , description = "Classic single-masted coastal sloop. Agile and balanced, ideal for navigating tight harbor channels."
      }
    , { name = "Gaff Cutter"
      , hash = "#b/FYixDYMwEADvwR-lI0XK7JBXXjaO6ChooaKHKRiHkg1YD1PcnXT9IDzUU7I1uJmrW8pr-LlH9X9sF81tzBZmPkDDs5jp-waBrQBdVdKMt9mlruCUMdRwCFrmiws"
      , description = "Sturdy ocean cutter with broad waterline beam and heavy keel for high stability in strong winds."
      }
    , { name = "Twin-Mast Schooner"
      , hash = "#b/FYixDYMwEADvwR-lI0XK7JBXXjaO6ChooaKHKRiHkg1YD1PcnXT9IDzUU7I1uJmrW8pr-LlH9X9sF81tzBZmPkDDs5jp-waBrQBdVdKMt9mlruCUMdRwCFrmiws"
      , description = "Fast double-masted vessel with large total sail area for reaching high speeds downwind."
      }
    , { name = "Catamaran Racer"
      , hash = "#b/FYixDYMwEADvwR-lI0XK7JBXXjaO6ChooaKHKRiHkg1YD1PcnXT9IDzUU7I1uJmrW8pr-LlH9X9sF81tzBZmPkDDs5jp-waBrQBdVdKMt9mlruCUMdRwCFrmiws"
      , description = "Twin-hull modern racer with wide righting moment and minimal water resistance."
      }
    ]


type PartCategory
    = Hull
    | Mast
    | Sail
    | RudderKeel
    | DeckFitting


classifyPart : String -> Float -> PartCategory
classifyPart rawName yPos =
    let
        p =
            String.toLower rawName
    in
    if String.contains "antenna" p || String.contains "3957" p || String.contains "bar" p || String.contains "pole" p || String.contains "axle" p then
        Mast

    else if String.contains "flag" p || String.contains "2335" p || String.contains "sail" p || String.contains "cloth" p || String.contains "wedge" p then
        Sail

    else if String.contains "rudder" p || String.contains "fin" p || yPos > 10.0 then
        RudderKeel

    else if String.contains "slope" p || String.contains "boat" p || String.contains "3660" p || String.contains "3003" p || String.contains "brick" p then
        Hull

    else
        DeckFitting


{-| Ingest an LDraw document into a `ShipSpec`.
-}
ingestDocument : String -> Document -> ShipSpec
ingestDocument name doc =
    let
        allLines =
            doc.steps
                |> List.concatMap .lines

        partPlacements =
            allLines
                |> List.filterMap
                    (\line ->
                        case line of
                            Document.SubfileLine { file, transform } ->
                                Just ( file, transform.position )

                            _ ->
                                Nothing
                    )
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
            mass =
                max 250.0 (toFloat totalCount * 45.0 + lengthMeters * beamMeters * 80.0)

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
    , totalPartCount = 8
    }
