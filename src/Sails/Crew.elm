module Sails.Crew exposing
    ( Crew
    , allInPort
    , heardMoorings
    , heardWrecks
    , mooredEvent
    , ofRoom
    , sharedTime
    , wreckedEvent
    )

{-| The cooperative rules of an online room: everyone in a room is one crew
sailing the same course together. Nobody wins or places; the voyage is done
when every ship of the crew is in port, and the crew shares one time.

Only the room voyage counts, from the host's start (`InRace`) on: a ship that
sails the course while the room waits in its lobby is warming up.

The crew's time is the crew clock (seconds since the room's start) when the
last ship moored. Each ship tells the room the clock it moored at, as a
`"moored"` room event, so every screen shows the same time even when one
page's clock fell behind (a hidden tab gets no frames).

-}

import Dict exposing (Dict)
import Game.Multiplayer as Multiplayer exposing (RoomState, RoomStatus(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Math.Vector3 exposing (Vec3)


{-| How many ships of the crew are in port, of how many.
-}
type alias Crew =
    { inPort : Int
    , total : Int
    }


{-| The crew of a room, given whether this player's own ship is in port
(fresher than the telemetry the room echoes back). Nothing alone, or while
the room is not on its voyage.
-}
ofRoom : Maybe RoomState -> Bool -> Maybe Crew
ofRoom maybeRoom ownInPort =
    case maybeRoom of
        Just room ->
            if onVoyage room then
                let
                    others =
                        crewmates room

                    count b =
                        if b then
                            1

                        else
                            0
                in
                Just
                    { inPort = count ownInPort + List.length (List.filter .finished others)
                    , total = 1 + List.length others
                    }

            else
                Nothing

        Nothing ->
            Nothing


onVoyage : RoomState -> Bool
onVoyage room =
    room.status == InRace || room.status == Finished


crewmates : RoomState -> List Multiplayer.RemotePlayer
crewmates room =
    List.filter (\p -> p.slot /= room.mySlot) room.players


{-| The whole crew is in port: the room's voyage is complete.
-}
allInPort : Crew -> Bool
allInPort crew =
    crew.total > 0 && crew.inPort >= crew.total


mooredKind : String
mooredKind =
    "moored"


{-| The room event that tells the crew this ship moored at crew clock `at`.
-}
mooredEvent : Float -> Vec3 -> Encode.Value
mooredEvent at position =
    Multiplayer.encodeEvent { kind = mooredKind, at = at, position = position, fields = [] }


{-| Add the crewmates' moorings a room state tells of (crew clock by slot).
-}
heardMoorings : RoomState -> Dict Int Float -> Dict Int Float
heardMoorings room moorings =
    room.events
        |> List.filter (\e -> e.kind == mooredKind && e.slot /= room.mySlot)
        |> List.foldl (\e acc -> Dict.insert e.slot e.at acc) moorings


{-| The crew's shared time: the latest mooring of the crew's ships. A ship
whose mooring this page has not heard of counts at this page's own clock.
-}
sharedTime : RoomState -> Dict Int Float -> Float -> Float
sharedTime room moorings clock =
    (room.mySlot :: List.map .slot (crewmates room))
        |> List.map (\slot -> Dict.get slot moorings |> Maybe.withDefault clock)
        |> List.maximum
        |> Maybe.withDefault clock


wreckedKind : String
wreckedKind =
    "wrecked"


{-| The room event that tells the crew this ship was wrecked at crew clock
`at`, where it was and the compass `heading` it was sailing, so the others
see it break up into its own bricks (`Sails.Wreck`).
-}
wreckedEvent : Float -> Vec3 -> Float -> Encode.Value
wreckedEvent at position heading =
    Multiplayer.encodeEvent { kind = wreckedKind, at = at, position = position, fields = [ ( "heading", Encode.float heading ) ] }


{-| The crewmates' wrecks a room state tells of: whose ship (`slot`), where
and on what heading.
-}
heardWrecks : RoomState -> List { slot : Int, position : Vec3, heading : Float }
heardWrecks room =
    room.events
        |> List.filter (\e -> e.kind == wreckedKind && e.slot /= room.mySlot)
        |> List.map
            (\e ->
                { slot = e.slot
                , position = e.position
                , heading = Decode.decodeValue (Decode.field "heading" Decode.float) e.data |> Result.withDefault 0
                }
            )
