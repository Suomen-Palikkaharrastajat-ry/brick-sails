module MapLinkTest exposing (suite)

{-| Tests of map links (`#map=<code>`, alone or in front of a build) and of
the per-map leaderboard (`Sails.Records`).
-}

import Expect
import Game.Leaderboard as Leaderboard
import Game.MapParams as MapParams
import LDraw.Document as Document
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.Ingest as Ingest
import Sails.Records as Records
import Test exposing (Test, describe, test)


boreal : MapParams.MapParams
boreal =
    { biome = MapParams.Boreal, season = MapParams.Winter, weather = MapParams.Snow, seed = 48213 }


entry : String -> Float -> Leaderboard.LeaderboardEntry
entry courseId time =
    { courseId = courseId, player = "You", timeSeconds = time, isoDate = "", vehicleName = "Sloop" }


suite : Test
suite =
    describe "Map links and records"
        [ describe "links"
            [ test "a map link names its map" <|
                \_ ->
                    Expect.equal (Just boreal) (Ingest.mapFromHash "https://ohoi.palikkaharrastajat.fi/#map=boreal-winter-snow-48213")
            , test "a map alone is not a ship" <|
                \_ ->
                    Expect.equal False (Ingest.hasShipPayload (Ingest.encodeMapHash boreal Nothing))
            , test "a link without a map, or with a broken one, names none" <|
                \_ ->
                    Expect.equal [ Nothing, Nothing, Nothing ]
                        (List.map Ingest.mapFromHash [ "#b/abc", "#map=jungle-summer-clear-1", "" ])
            , test "a map and a build travel together and come back apart" <|
                \_ ->
                    let
                        doc =
                            Document.parse Boats.default.ldraw

                        link =
                            Ingest.encodeMapHash boreal (Just doc)
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (String.startsWith "#map=boreal-winter-snow-48213&b/" link)
                        , \_ -> Expect.equal (Just boreal) (Ingest.mapFromHash link)
                        , \_ -> Expect.equal True (Ingest.hasShipPayload link)
                        , \_ ->
                            Ingest.decodeShipInput Nothing link
                                |> Result.map (.document >> Document.flattenedPlacements >> List.length)
                                |> Expect.equal (Ok (List.length (Document.flattenedPlacements doc)))
                        ]
                        ()
            , test "the map is left out before a build is decoded" <|
                \_ ->
                    Expect.equal "model=abc&parts=" (Ingest.hashWithoutMap "#map=fell-summer-clear-3&model=abc&parts=")
            , test "a course's map and leaderboard key come from the map" <|
                \_ ->
                    let
                        course =
                            Archipelago.courseFor (Archipelago.generate boreal) 0
                    in
                    Expect.equal ( boreal, "g1:boreal-winter-snow-48213" ) ( course.map, course.id )
            ]
        , describe "records"
            [ test "runs of the old fixed course and of other generators are dropped" <|
                \_ ->
                    { entries = [ entry "harbour-loop" 100, entry "g0:boreal-winter-snow-48213" 90, entry "g1:boreal-winter-snow-48213" 95 ] }
                        |> Records.tidy
                        |> .entries
                        |> List.map .courseId
                        |> Expect.equal [ "g1:boreal-winter-snow-48213" ]
            , test "only the maps sailed most recently keep their boards" <|
                \_ ->
                    let
                        many =
                            List.map (\i -> entry ("g1:desert-summer-clear-" ++ String.fromInt i) 100) (List.range 1 (Records.maxMaps + 5))

                        kept =
                            Records.tidy { entries = many } |> .entries |> List.map .courseId
                    in
                    Expect.all
                        [ \_ -> Expect.equal Records.maxMaps (List.length kept)
                        , \_ -> Expect.equal False (List.member "g1:desert-summer-clear-1" kept)
                        , \_ -> Expect.equal True (List.member ("g1:desert-summer-clear-" ++ String.fromInt (Records.maxMaps + 5)) kept)
                        ]
                        ()
            , test "the previous map is the one sailed last" <|
                \_ ->
                    Leaderboard.empty
                        |> Leaderboard.recordRun (entry "g1:fell-summer-clear-3" 100)
                        |> Leaderboard.recordRun (entry "g1:boreal-winter-snow-48213" 120)
                        |> Records.lastMap
                        |> Expect.equal (Just boreal)
            , test "no previous map before the first generated run" <|
                \_ ->
                    Expect.equal Nothing (Records.lastMap { entries = [ entry "harbour-loop" 100 ] })
            ]
        ]
