module Sails.Room exposing (RoomUpdate(..), fromPort)

{-| What a `multiplayerRoomStateChanged` message from `runtime/multiplayer.js`
tells this game about its online room.
-}

import Game.Multiplayer as Multiplayer exposing (RoomState)
import Json.Decode as Decode


{-| A room this page is in, no room (the page has left it, or never was in
one), or a message that cannot be read.
-}
type RoomUpdate
    = InRoom RoomState
    | NoRoom
    | Unreadable


{-| Read a room state message. One with no room code is `NoRoom`:
`runtime/multiplayer.js` sends it after `leaveMultiplayerRoom` (an empty room
id and role), and it decodes as a room all the same. Taking it for one sent the
player straight back to sailing in a room with code "" after they left a
course, and Exit only sent the same message back.
-}
fromPort : Decode.Value -> RoomUpdate
fromPort raw =
    case Decode.decodeValue Multiplayer.decodeRoomState raw of
        Ok room ->
            if String.isEmpty (String.trim room.roomId) then
                NoRoom

            else
                InRoom room

        Err _ ->
            Unreadable
