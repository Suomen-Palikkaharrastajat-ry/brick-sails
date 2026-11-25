module Sails.Records exposing
    ( lastMap
    , maxMaps
    , tidy
    )

{-| The local leaderboard of Brick Sails, one board per exact map.

Every run is recorded under its course id, the map's leaderboard key
(`Game.MapParams.leaderboardKey`, such as `g1:boreal-winter-snow-48213`).
`tidy` drops what cannot be compared any more: runs from before the maps
were generated (the old fixed course), runs of an older generator version,
and the boards of all but the `maxMaps` maps sailed most recently.

`Game.Leaderboard.recordRun` moves a map's board to the end of the list, so
the last entry is the map sailed most recently: `lastMap` reads it, which
gives the main screen its "previous map" after a reload.

-}

import Game.Leaderboard exposing (Leaderboard)
import Game.MapParams as MapParams exposing (MapParams)


{-| How many maps keep their boards.
-}
maxMaps : Int
maxMaps =
    60


prefix : String
prefix =
    MapParams.generatorVersion ++ ":"


{-| Keep only runs of the current generator, on the `maxMaps` maps sailed
most recently.
-}
tidy : Leaderboard -> Leaderboard
tidy board =
    let
        current =
            List.filter (\e -> String.startsWith prefix e.courseId) board.entries

        -- Course ids from the most recent back, each once.
        recent =
            List.foldr
                (\e acc ->
                    if List.member e.courseId acc then
                        acc

                    else
                        acc ++ [ e.courseId ]
                )
                []
                current
                |> List.take maxMaps
    in
    { board | entries = List.filter (\e -> List.member e.courseId recent) current }


{-| The map of the most recent run, if it is one of the current generator.
-}
lastMap : Leaderboard -> Maybe MapParams
lastMap board =
    board.entries
        |> List.reverse
        |> List.filter (\e -> String.startsWith prefix e.courseId)
        |> List.head
        |> Maybe.andThen (\e -> MapParams.fromCode (String.dropLeft (String.length prefix) e.courseId))
