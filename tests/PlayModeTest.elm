module PlayModeTest exposing (suite)

{-| Play modes of a room: what each host mode opens, and what a room holds. Every player sails on their own screen, in their own full view
(`Pages.Sail`), so a room has no split-screen layout.
-}

import Expect
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


suite : Test
suite =
    describe "Play modes"
        [ describe "host modes"
            [ test "the 2-player host opens a room of two" <|
                \_ ->
                    Expect.equal 2 (Multiplayer.sessionCapacity TwoPlayers)
            , test "the 8-player host opens a room of eight" <|
                \_ ->
                    Expect.equal 8 (Multiplayer.sessionCapacity GridSession)
            ]
        , describe "rooms"
            [ test "a room keeps its players by slot" <|
                \_ ->
                    Expect.equal [ 1, 2, 3, 4 ] (roomOf 4 3 |> .players |> List.map .slot)
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
