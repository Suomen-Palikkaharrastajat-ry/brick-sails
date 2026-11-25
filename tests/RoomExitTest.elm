module RoomExitTest exposing (suite)

{-| Leaving a course (stop, then Exit, or Back to boats) sends `leaveMultiplayerRoom`, and
`runtime/multiplayer.js` answers with the state of no room at all: an empty
room id and role, slot 1, lobby. That answer must not open a sailing view in a room,
or Exit only brings the same empty room back.
-}

import Expect
import Game.Multiplayer as Multiplayer exposing (RoomStatus(..))
import Json.Decode as Decode
import Sails.Room as Room exposing (RoomUpdate(..))
import Test exposing (Test, describe, test)


{-| The message `emitRoomState` sends right after `cleanup()` (the
`leaveMultiplayerRoom` port).
-}
leftRoomJson : String
leftRoomJson =
    "{\"roomId\":\"\",\"role\":\"\",\"mySlot\":1,\"status\":\"lobby\",\"countdown\":3,\"maxPlayers\":4,\"players\":[],\"events\":[]}"


realRoomJson : String
realRoomJson =
    "{\"roomId\":\"ABC123\",\"role\":\"host\",\"mySlot\":1,\"status\":\"lobby\",\"countdown\":3,\"maxPlayers\":4,\"players\":[],\"events\":[]}"


fromString : String -> RoomUpdate
fromString json =
    case Decode.decodeString Decode.value json of
        Ok value ->
            Room.fromPort value

        Err _ ->
            Unreadable


suite : Test
suite =
    describe "Leaving a race and its room"
        [ test "the state sent after leaving the room is no room" <|
            \_ ->
                Expect.equal NoRoom (fromString leftRoomJson)
        , test "a room with a code is a room" <|
            \_ ->
                case fromString realRoomJson of
                    InRoom room ->
                        Expect.equal ( "ABC123", Lobby, Multiplayer.HostCommentator ) ( room.roomId, room.status, room.role )

                    _ ->
                        Expect.fail "expected a room"
        , test "a message that is not a room state changes nothing" <|
            \_ ->
                Expect.equal Unreadable (fromString "{\"hello\":1}")
        ]
