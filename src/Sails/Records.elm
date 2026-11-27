module Sails.Records exposing
    ( Board
    , bestForCourse
    , empty
    , fromJson
    , lastMap
    , maxMaps
    , perCourse
    , recordRun
    , tidy
    , toJson
    )

{-| The local leaderboard of Brick Sails, one board per exact map.

Every run is recorded under its course id, the map's leaderboard key
(`Game.MapParams.leaderboardKey`, such as `g1:boreal-winter-snow-48213`).
Runs rank by the gold coins they earned (`Sails.Coins`), most first; of
two runs with the same coins the faster ranks first. The time is kept for
the finish screen.

The board is stored as the JSON of `Game.Leaderboard` (`{"entries": [...]}`
with `courseId`, `player`, `timeSeconds`, `isoDate`, `vehicleName`) plus a
`coins` field on each entry. Runs saved before the game scored coins have no
`coins` field: they were ranked by time and cannot be compared, so
`fromJson` leaves them out (the rest of the board still loads), and the next
save drops them for good. A board that does not read at all starts empty.

`tidy` drops what cannot be compared any more: runs from before the maps
were generated (the old fixed course), runs of an older generator version,
and the boards of all but the `maxMaps` maps sailed most recently.

`recordRun` moves a map's board to the end of the list, so the last entry
is the map sailed most recently: `lastMap` reads it, which gives the main
screen its "previous map" after a reload.

-}

import Game.MapParams as MapParams exposing (MapParams)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Sails.Types exposing (LeaderboardEntry)


{-| The saved runs.
-}
type alias Board =
    { entries : List LeaderboardEntry
    }


{-| A board with no runs.
-}
empty : Board
empty =
    { entries = [] }


{-| How many maps keep their boards.
-}
maxMaps : Int
maxMaps =
    60


{-| How many runs each map keeps.
-}
perCourse : Int
perCourse =
    10


prefix : String
prefix =
    MapParams.generatorVersion ++ ":"


{-| Best first: most coins, then the shortest time.
-}
rank : LeaderboardEntry -> ( Int, Float )
rank entry =
    ( negate entry.coins, entry.timeSeconds )


{-| Record a run, keeping the `perCourse` best runs of its map. The map's
board moves to the end of the list.
-}
recordRun : LeaderboardEntry -> Board -> Board
recordRun entry board =
    let
        ( course, others ) =
            List.partition (\e -> e.courseId == entry.courseId) board.entries
    in
    { entries =
        others
            ++ ((entry :: course)
                    |> List.sortBy rank
                    |> List.take perCourse
               )
    }


{-| The best run of a course: the most coins, the faster of a tie.
-}
bestForCourse : String -> Board -> Maybe LeaderboardEntry
bestForCourse courseId board =
    board.entries
        |> List.filter (\e -> e.courseId == courseId)
        |> List.sortBy rank
        |> List.head


{-| Keep only runs of the current generator, on the `maxMaps` maps sailed
most recently.
-}
tidy : Board -> Board
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
lastMap : Board -> Maybe MapParams
lastMap board =
    board.entries
        |> List.reverse
        |> List.filter (\e -> String.startsWith prefix e.courseId)
        |> List.head
        |> Maybe.andThen (\e -> MapParams.fromCode (String.dropLeft (String.length prefix) e.courseId))



-- JSON


{-| Read the saved board. Entries without coins (saved before the game
scored coins) are left out; anything unreadable starts an empty board.
-}
fromJson : String -> Board
fromJson raw =
    Decode.decodeString boardDecoder raw
        |> Result.withDefault empty


{-| The board as the JSON string `Game.Ports.saveLeaderboard` stores.
-}
toJson : Board -> String
toJson board =
    Encode.encode 0
        (Encode.object [ ( "entries", Encode.list encodeEntry board.entries ) ])


encodeEntry : LeaderboardEntry -> Encode.Value
encodeEntry entry =
    Encode.object
        [ ( "courseId", Encode.string entry.courseId )
        , ( "player", Encode.string entry.player )
        , ( "coins", Encode.int entry.coins )
        , ( "timeSeconds", Encode.float entry.timeSeconds )
        , ( "isoDate", Encode.string entry.isoDate )
        , ( "vehicleName", Encode.string entry.vehicleName )
        ]


boardDecoder : Decoder Board
boardDecoder =
    Decode.field "entries"
        (Decode.list (Decode.oneOf [ Decode.map Just entryDecoder, Decode.succeed Nothing ]))
        |> Decode.map (\entries -> { entries = List.filterMap identity entries })


entryDecoder : Decoder LeaderboardEntry
entryDecoder =
    Decode.map6 LeaderboardEntry
        (Decode.field "courseId" Decode.string)
        (Decode.field "player" Decode.string)
        (Decode.field "coins" Decode.int)
        (Decode.field "timeSeconds" Decode.float)
        (Decode.field "isoDate" Decode.string)
        (Decode.field "vehicleName" Decode.string)
