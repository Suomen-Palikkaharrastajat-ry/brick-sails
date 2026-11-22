module PlayModeTest exposing (suite)

{-| Play modes of a room: what each host mode opens, and which screen layout
a room's players get (one full view alone, a grid of views otherwise).
-}

import Expect
import Game.Grid as Grid exposing (Orientation(..))
import Game.Multiplayer as Multiplayer exposing (RoomState, SessionMode(..))
import Json.Decode as Decode
import Test exposing (Test, describe, test)


roomWithSlots : Int -> Int -> String
roomWithSlots count mySlot =
    let
        players =
            List.range 1 count
                |> List.map
                    (\slot ->
                        "{\"slot\":" ++ String.fromInt slot ++ ",\"id\":\"p" ++ String.fromInt slot ++ "\",\"name\":\"P" ++ String.fromInt slot ++ "\",\"position\":[0,0,0],\"rotation\":[0,0,0],\"speed\":0,\"progress\":0,\"finished\":false}"
                    )
                |> String.join ","
    in
    "{\"roomId\":\"ROOM42\",\"role\":\"player\",\"mySlot\":" ++ String.fromInt mySlot ++ ",\"status\":\"lobby\",\"countdown\":3,\"maxPlayers\":8,\"players\":[" ++ players ++ "]}"


roomOf : Int -> Int -> RoomState
roomOf count mySlot =
    Decode.decodeString Multiplayer.decodeRoomState (roomWithSlots count mySlot)
        |> Result.withDefault Multiplayer.emptyRoomState


layoutOf : Orientation -> RoomState -> Grid.Layout
layoutOf o room =
    Grid.layoutFor o (List.length room.players)


suite : Test
suite =
    describe "Play modes"
        [ describe "host modes"
            [ test "the 2-player host opens a room of two" <|
                \_ ->
                    Expect.equal 2 (Multiplayer.sessionCapacity TwoPlayers)
            , test "the grid host opens a room of eight" <|
                \_ ->
                    Expect.equal 8 (Multiplayer.sessionCapacity GridSession)
            ]
        , describe "screen layout of a room"
            [ test "a room of one player is the full view in either orientation" <|
                \_ ->
                    Expect.equal [ Grid.SingleView, Grid.SingleView ]
                        [ layoutOf Landscape (roomOf 1 1), layoutOf Portrait (roomOf 1 1) ]
            , test "a 2-player room splits side by side in landscape and stacks in portrait" <|
                \_ ->
                    Expect.equal
                        [ Grid.Tiles { cols = 2, rows = 1 }, Grid.Tiles { cols = 1, rows = 2 } ]
                        [ layoutOf Landscape (roomOf 2 1), layoutOf Portrait (roomOf 2 2) ]
            , test "a 4-player room is a 2 by 2 grid" <|
                \_ ->
                    Expect.equal (Grid.Tiles { cols = 2, rows = 2 }) (layoutOf Landscape (roomOf 4 3))
            , test "a 6-player room is 3 by 2 in landscape and 2 by 3 in portrait" <|
                \_ ->
                    Expect.equal
                        [ Grid.Tiles { cols = 3, rows = 2 }, Grid.Tiles { cols = 2, rows = 3 } ]
                        [ layoutOf Landscape (roomOf 6 6), layoutOf Portrait (roomOf 6 1) ]
            ]
        , describe "the viewer's own view"
            [ test "the local player's view comes first, the rest keep their order" <|
                \_ ->
                    let
                        room =
                            roomOf 4 3
                    in
                    Expect.equal [ 3, 1, 2, 4 ]
                        (Grid.selfFirst .slot room.mySlot room.players |> List.map .slot)
            , test "a room from an older host holds the default four players" <|
                \_ ->
                    let
                        legacy =
                            "{\"roomId\":\"ROOM42\",\"role\":\"player\",\"mySlot\":2,\"status\":\"lobby\",\"countdown\":3,\"players\":[]}"
                    in
                    Expect.equal (Ok Multiplayer.defaultCapacity)
                        (Decode.decodeString Multiplayer.decodeRoomState legacy |> Result.map .maxPlayers)
            ]
        ]
