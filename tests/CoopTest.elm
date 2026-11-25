module CoopTest exposing (suite)

{-| Cooperative play in an online room: the players are one crew on one
course. The voyage is complete when the whole crew is in port, the crew shares
one time, cannons never hit a crewmate, and a finish screen in a room has no
personal best, rank or winner. Sailing alone is unchanged.
-}

import Dict
import Expect
import Game.Multiplayer as Multiplayer exposing (RemotePlayer, Role(..), RoomEvent, RoomState, RoomStatus(..))
import Html.Attributes
import I18n exposing (Language(..))
import Json.Encode as Encode
import Math.Vector3 exposing (vec3)
import Pages.Results as Results
import Pages.Sail as Sail exposing (SailModel, SailMsg(..))
import Sails.Boats as Boats
import Sails.Crew as Crew
import Sails.I18n as T
import Sails.Physics as Physics
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


player : Int -> Bool -> RemotePlayer
player slot finished =
    { slot = slot
    , id = "p" ++ String.fromInt slot
    , name = "P" ++ String.fromInt slot
    , vehicleId = ""
    , vehicleReady = True
    , vehicleLoaded = True
    , position = vec3 0 0 (toFloat slot * 1000)
    , rotation = vec3 0 0 0
    , speed = 0
    , progress = 0
    , finished = finished
    , finishTime = Nothing
    }


{-| A room of `players`, seen by the player in slot `mySlot`.
-}
room : Role -> Int -> RoomStatus -> List RemotePlayer -> RoomState
room role mySlot status players =
    { roomId = "CREW42"
    , role = role
    , mySlot = mySlot
    , status = status
    , countdown = 0
    , players = players
    , maxPlayers = 4
    , events = []
    }


{-| A crewmate's "moored" room event: slot `slot` moored at crew clock `at`.
-}
moored : Int -> Float -> RoomEvent
moored slot at =
    { slot = slot, id = "m" ++ String.fromInt slot, kind = "moored", at = at, position = vec3 0 0 0, data = Encode.null }


sail : Maybe RoomState -> SailModel
sail maybeRoom =
    Sail.initSail En Boats.default Physics.defaultCourse True Nothing maybeRoom { available = False, enabled = False } 800 600


{-| One 20 ms frame.
-}
frame : SailModel -> SailModel
frame model =
    Tuple.first (Sail.update (Tick 20) model)


frames : Int -> SailModel -> SailModel
frames n model =
    List.foldl (\_ m -> frame m) model (List.range 1 n)


inPort : SailModel -> SailModel
inPort model =
    let
        progress =
            model.progress
    in
    { model | progress = { progress | finished = True, elapsedSeconds = 90 } }


resultsText : Maybe Results.CrewResult -> Query.Single ()
resultsText crew =
    Results.viewResults
        { course = Physics.defaultCourse
        , progress = (inPort (sail Nothing)).progress
        , boatName = Boats.default.name
        , personalBest = Nothing
        , crew = crew
        , language = En
        , actions = { onSailAgain = (), onReturnToShipyard = () }
        }
        |> Query.fromHtml


suite : Test
suite =
    describe "Cooperative rooms"
        [ describe "Sails.Crew"
            [ test "alone there is no crew" <|
                \_ -> Expect.equal Nothing (Crew.ofRoom Nothing True)
            , test "while the room waits in its lobby, nothing counts" <|
                \_ ->
                    Expect.equal Nothing
                        (Crew.ofRoom (Just (room HostCommentator 1 Lobby [ player 1 True, player 2 True ])) True)
            , test "the crew counts this ship and every crewmate in port" <|
                \_ ->
                    Expect.equal (Just { inPort = 2, total = 3 })
                        (Crew.ofRoom (Just (room JoinedPlayer 2 InRace [ player 1 True, player 2 False, player 3 False ])) True)
            , test "this player's own flag wins over its echoed telemetry" <|
                \_ ->
                    Expect.equal (Just { inPort = 0, total = 2 })
                        (Crew.ofRoom (Just (room JoinedPlayer 2 InRace [ player 1 False, player 2 True ])) False)
            , test "the voyage is not complete while one ship is still at sea" <|
                \_ ->
                    Crew.ofRoom (Just (room JoinedPlayer 2 InRace [ player 1 True, player 2 False, player 3 False ])) True
                        |> Maybe.map Crew.allInPort
                        |> Expect.equal (Just False)
            , test "the voyage is complete once the whole crew is in port" <|
                \_ ->
                    Crew.ofRoom (Just (room JoinedPlayer 2 InRace [ player 1 True, player 2 False, player 3 True ])) True
                        |> Maybe.map Crew.allInPort
                        |> Expect.equal (Just True)
            , test "a room with only this player is a crew of one" <|
                \_ ->
                    Expect.equal (Just { inPort = 1, total = 1 })
                        (Crew.ofRoom (Just (room HostCommentator 1 InRace [])) True)
            ]
        , describe "The crew's shared time"
            [ test "the crew clock runs while the room's voyage is under way" <|
                \_ ->
                    (frames 50 (sail (Just (room JoinedPlayer 2 InRace [ player 1 False, player 2 False ])))).crewClock
                        |> Expect.within (Expect.Absolute 0.001) 1.0
            , test "there is no crew time while a crewmate is still at sea" <|
                \_ ->
                    (frames 5 (inPort (sail (Just (room JoinedPlayer 2 InRace [ player 1 False, player 2 False ]))))).crewTime
                        |> Expect.equal Nothing
            , test "the crew time is the crew clock when the last ship moored" <|
                \_ ->
                    let
                        atSea =
                            frames 50 (inPort (sail (Just (room JoinedPlayer 2 InRace [ player 1 False, player 2 True ]))))

                        lastIn =
                            Tuple.first (Sail.update (UpdateMultiplayerRoom (room JoinedPlayer 2 InRace [ player 1 True, player 2 True ])) atSea)

                        later =
                            frames 50 (frame lastIn)
                    in
                    Expect.all
                        [ \_ -> Expect.equal Nothing atSea.crewTime
                        , \_ -> later.crewTime |> Maybe.withDefault 0 |> Expect.within (Expect.Absolute 0.001) 1.02
                        , \_ -> Expect.greaterThan 2.0 later.crewClock
                        ]
                        ()
            , test "a crewmate's mooring is heard from their room event, not this page's own" <|
                \_ ->
                    let
                        r =
                            { roomWith | events = [ moored 1 4.5, moored 2 9.9, { id = "x", slot = 1, kind = "bump", at = 7, position = vec3 0 0 0, data = Encode.null } ] }

                        roomWith =
                            room JoinedPlayer 2 InRace [ player 1 True, player 2 True ]
                    in
                    Expect.equal [ ( 1, 4.5 ) ] (Dict.toList (Crew.heardMoorings r Dict.empty))
            , test "the crew time is the latest mooring of the crew, the same on every screen" <|
                \_ ->
                    let
                        r =
                            room JoinedPlayer 2 InRace [ player 1 True, player 2 True, player 3 True ]
                    in
                    Expect.equal 6.0 (Crew.sharedTime r (Dict.fromList [ ( 1, 6.0 ), ( 2, 2.0 ), ( 3, 4.0 ) ]) 3.0)
            , test "a mooring this page has not heard of counts at its own clock" <|
                \_ ->
                    let
                        r =
                            room JoinedPlayer 2 InRace [ player 1 True, player 2 True ]
                    in
                    Expect.equal 3.0 (Crew.sharedTime r (Dict.fromList [ ( 2, 2.0 ) ]) 3.0)
            , test "this ship's mooring is noted at the crew clock, and a late crewmate sets the crew time" <|
                \_ ->
                    let
                        docked =
                            frames 50 (inPort (sail (Just (room JoinedPlayer 2 InRace [ player 1 False, player 2 True ]))))

                        late =
                            room JoinedPlayer 2 InRace [ player 1 True, player 2 True ]

                        heard =
                            Tuple.first (Sail.update (UpdateMultiplayerRoom { late | events = [ moored 1 5.0 ] }) docked)

                        after =
                            frame heard
                    in
                    Expect.all
                        [ \_ -> Dict.get 2 docked.moorings |> Maybe.withDefault -1 |> Expect.within (Expect.Absolute 0.001) 0.02
                        , \_ -> Expect.equal (Just 5.0) after.crewTime
                        ]
                        ()
            , test "sailing again alone keeps the crew's voyage" <|
                \_ ->
                    let
                        done =
                            frames 10 (inPort (sail (Just (room JoinedPlayer 2 InRace [ player 1 True, player 2 True ]))))

                        again =
                            Tuple.first (Sail.update RestartCourse done)
                    in
                    Expect.all
                        [ \_ -> Expect.equal False again.progress.finished
                        , \_ -> Expect.equal done.crewTime again.crewTime
                        , \_ -> Expect.notEqual Nothing again.crewTime
                        ]
                        ()
            , test "the host's start puts every ship, the host's too, back on the start line" <|
                \_ ->
                    let
                        warmedUp =
                            frames 10 (inPort (sail (Just (room HostCommentator 1 InRace [ player 1 True, player 2 True ]))))

                        started =
                            Tuple.first (Sail.update (UpdateMultiplayerRoom (room HostCommentator 1 (Multiplayer.Countdown 3) [ player 1 True, player 2 True ])) warmedUp)
                    in
                    Expect.all
                        [ \_ -> Expect.notEqual Nothing warmedUp.crewTime
                        , \_ -> Expect.equal False started.progress.finished
                        , \_ -> Expect.equal Nothing started.crewTime
                        , \_ -> Expect.equal 0 started.crewClock
                        ]
                        ()
            ]
        , describe "Cannons and crewmates"
            [ test "a cannonball flies on past a crewmate's ship" <|
                \_ ->
                    let
                        mate =
                            player 2 False

                        here =
                            vec3 0 6 0

                        base =
                            sail (Just (room JoinedPlayer 1 InRace [ player 1 False, { mate | position = here } ]))

                        ball =
                            { id = 1, sourceSlot = 1, position = here, velocity = vec3 0 5 0, age = 0 }
                    in
                    frame { base | cannonballs = [ ball ] }
                        |> .cannonballs
                        |> List.length
                        |> Expect.equal 1
            ]
        , describe "The finish screen"
            [ test "while the crew is at sea it waits for them, with no personal best" <|
                \_ ->
                    resultsText (Just { inPort = 1, total = 3, clock = 75, crewTime = Nothing })
                        |> Expect.all
                            [ Query.has [ Selector.text (T.crewWaiting En 1 3) ]
                            , Query.hasNot [ Selector.text (T.newPersonalBest En) ]
                            , Query.find [ Selector.attribute (Html.Attributes.attribute "data-testid" "result-time") ] >> Query.has [ Selector.text "1:15" ]
                            ]
            , test "once the whole crew is in port it shows the crew's time, not a rank" <|
                \_ ->
                    resultsText (Just { inPort = 3, total = 3, clock = 130, crewTime = Just 120 })
                        |> Expect.all
                            [ Query.has [ Selector.text (T.crewTimeLabel En 3 3) ]
                            , Query.find [ Selector.attribute (Html.Attributes.attribute "data-testid" "result-time") ] >> Query.has [ Selector.text "2:00" ]
                            , Query.hasNot [ Selector.text (T.newPersonalBest En) ]
                            , Query.hasNot [ Selector.text "Best: " ]
                            ]
            , test "sailing alone still shows the personal best" <|
                \_ ->
                    resultsText Nothing
                        |> Query.has [ Selector.text (T.newPersonalBest En) ]
            ]
        , describe "Sailing alone is unchanged"
            [ test "alone there is no crew clock or crew time" <|
                \_ ->
                    let
                        solo =
                            frames 300 (sail Nothing)
                    in
                    Expect.equal ( 0, Nothing ) ( solo.crewClock, solo.crewTime )
            , test "alone the boat sails off once the 3-2-1 is over" <|
                \_ ->
                    (frames 300 (sail Nothing)).progress.elapsedSeconds
                        |> Expect.greaterThan 2.5
            ]
        ]
