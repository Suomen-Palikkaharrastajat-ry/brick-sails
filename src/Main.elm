module Main exposing (Flags, Model, Msg, main)

{-| Top-level Elm Architecture entry point for Brick Sails (`brick-sails`).
Deployed to <https://ohoi.palikkaharrastajat.fi>.

Two screens: the boat picker (`Pages.Shipyard`) and the race (`Pages.Sail`).

-}

import Browser
import Browser.Events
import Game.Leaderboard as Leaderboard exposing (Leaderboard)
import Game.Multiplayer as Multiplayer
import Html exposing (Html, div)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Sail as Sail
import Pages.Shipyard as Shipyard
import Ports
import Sails.Boats as Boats
import Sails.Physics as Physics
import Sails.Types exposing (Boat, Course, ViewMode(..))


type alias Flags =
    { savedAudioMuted : Bool
    , savedLeaderboardJson : String
    }


type alias Model =
    { boat : Boat
    , course : Course
    , viewMode : ViewMode
    , sailModel : Maybe Sail.SailModel
    , audioMuted : Bool
    , leaderboard : Leaderboard
    , tilt : { available : Bool, enabled : Bool }
    , shipyardTime : Float
    , viewportWidth : Int
    , viewportHeight : Int
    }


type Msg
    = SelectBoat String
    | StartSailing
    | StartHostMultiplayer
    | JoinMultiplayer
    | MultiplayerRoomChanged Decode.Value
    | MultiplayerError String
    | ReturnToShipyard
    | ToggleMute
    | TiltStatusChanged { available : Bool, enabled : Bool }
    | ShipyardTick Float
    | ViewportResized { width : Int, height : Int }
    | SailMessage Sail.SailMsg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { boat = Boats.default
      , course = Physics.defaultCourse
      , viewMode = ViewShipyard
      , sailModel = Nothing
      , audioMuted = flags.savedAudioMuted
      , leaderboard =
            Decode.decodeString Leaderboard.decodeLeaderboard flags.savedLeaderboardJson
                |> Result.withDefault Leaderboard.empty
      , tilt = { available = False, enabled = False }
      , shipyardTime = 0
      , viewportWidth = 1024
      , viewportHeight = 768
      }
    , Cmd.none
    )


personalBest : Model -> Maybe Sails.Types.LeaderboardEntry
personalBest model =
    Leaderboard.bestForCourse model.course.id model.leaderboard


newSail : Maybe Multiplayer.RoomState -> Model -> Sail.SailModel
newSail room model =
    Sail.initSail model.boat model.course model.audioMuted (personalBest model) room model.tilt model.viewportWidth model.viewportHeight


silenceAudio : Cmd msg
silenceAudio =
    Ports.updateAudioState
        { active = False
        , windGain = 0
        , windHz = 350
        , washGain = 0
        , washHz = 180
        , hullGain = 0
        , toneHz = 0
        , toneGain = 0
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectBoat name ->
            ( { model | boat = Boats.byName name }, Cmd.none )

        StartSailing ->
            ( { model | viewMode = ViewSailing, sailModel = Just (newSail Nothing model) }
            , Ports.calibrateTilt ()
            )

        StartHostMultiplayer ->
            ( model
            , Ports.createMultiplayerRoom
                { roomId = ""
                , courseId = model.course.id
                , name = "Host"
                , vehicleHash = model.boat.name
                }
            )

        JoinMultiplayer ->
            ( model
            , Ports.joinMultiplayerRoom
                { roomId = ""
                , name = "Sailor"
                , vehicleHash = model.boat.name
                }
            )

        MultiplayerRoomChanged rawJson ->
            case Decode.decodeValue Multiplayer.decodeRoomState rawJson of
                Ok room ->
                    case model.sailModel of
                        Just sModel ->
                            let
                                ( nextSail, sCmd ) =
                                    Sail.update (Sail.UpdateMultiplayerRoom room) sModel
                            in
                            ( { model | sailModel = Just nextSail }, Cmd.map SailMessage sCmd )

                        Nothing ->
                            ( { model | viewMode = ViewSailing, sailModel = Just (newSail (Just room) model) }
                            , Cmd.batch
                                [ Ports.setUrlHash ("#room=" ++ room.roomId)
                                , Ports.calibrateTilt ()
                                ]
                            )

                Err _ ->
                    ( model, Cmd.none )

        MultiplayerError _ ->
            ( model, Cmd.none )

        ReturnToShipyard ->
            ( { model | viewMode = ViewShipyard, sailModel = Nothing }
            , Cmd.batch [ Ports.leaveMultiplayerRoom (), silenceAudio ]
            )

        ToggleMute ->
            ( { model
                | audioMuted = not model.audioMuted
                , sailModel = model.sailModel |> Maybe.map (\s -> { s | audioMuted = not model.audioMuted })
              }
            , Ports.setAudioMuted (not model.audioMuted)
            )

        TiltStatusChanged status ->
            ( { model
                | tilt = status
                , sailModel = model.sailModel |> Maybe.map (\s -> { s | tilt = status, tiltSteer = 0 })
              }
            , Cmd.none
            )

        ShipyardTick dt ->
            ( { model | shipyardTime = model.shipyardTime + min 0.05 (dt / 1000) }, Cmd.none )

        ViewportResized dims ->
            ( { model
                | viewportWidth = dims.width
                , viewportHeight = dims.height
                , sailModel = model.sailModel |> Maybe.map (\s -> { s | viewportWidth = dims.width, viewportHeight = dims.height })
              }
            , Cmd.none
            )

        SailMessage Sail.ReturnToShipyard ->
            update ReturnToShipyard model

        SailMessage Sail.ToggleMute ->
            update ToggleMute model

        SailMessage Sail.RestartCourse ->
            case model.sailModel of
                Just sModel ->
                    let
                        ( nextSail, sCmd ) =
                            Sail.update Sail.RestartCourse { sModel | personalBest = personalBest model }
                    in
                    ( { model | sailModel = Just nextSail }, Cmd.map SailMessage sCmd )

                Nothing ->
                    ( model, Cmd.none )

        SailMessage sMsg ->
            case model.sailModel of
                Just sModel ->
                    let
                        ( nextSail, sCmd ) =
                            Sail.update sMsg sModel

                        justFinished =
                            nextSail.progress.finished && not sModel.progress.finished
                    in
                    if justFinished then
                        recordFinish nextSail { model | sailModel = Just nextSail } (Cmd.map SailMessage sCmd)

                    else
                        ( { model | sailModel = Just nextSail }, Cmd.map SailMessage sCmd )

                Nothing ->
                    ( model, Cmd.none )


{-| Save the finished run to the local leaderboard.
-}
recordFinish : Sail.SailModel -> Model -> Cmd Msg -> ( Model, Cmd Msg )
recordFinish sail model cmd =
    let
        board =
            Leaderboard.recordRun
                { courseId = model.course.id
                , player = "You"
                , timeSeconds = Physics.finalTime sail.progress
                , isoDate = ""
                , vehicleName = sail.boat.name
                }
                model.leaderboard
    in
    ( { model | leaderboard = board }
    , Cmd.batch [ cmd, Ports.saveLeaderboard (Encode.encode 0 (Leaderboard.encodeLeaderboard board)) ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.viewportResized ViewportResized
        , Ports.multiplayerRoomStateChanged MultiplayerRoomChanged
        , Ports.multiplayerError MultiplayerError
        , Ports.tiltStatus TiltStatusChanged
        , case ( model.viewMode, model.sailModel ) of
            ( ViewSailing, Just sModel ) ->
                Sub.map SailMessage (Sail.subscriptions sModel)

            _ ->
                Browser.Events.onAnimationFrameDelta ShipyardTick
        ]


view : Model -> Html Msg
view model =
    case ( model.viewMode, model.sailModel ) of
        ( ViewSailing, Just sModel ) ->
            Html.map SailMessage
                (Sail.viewSail sModel
                    { onReturnToShipyard = Sail.ReturnToShipyard
                    , onRestartCourse = Sail.RestartCourse
                    }
                )

        _ ->
            Shipyard.viewShipyard
                { boat = model.boat
                , course = model.course
                , personalBest = personalBest model
                , time = model.shipyardTime
                , audioMuted = model.audioMuted
                , tilt = model.tilt
                , viewportWidth = model.viewportWidth
                , viewportHeight = model.viewportHeight
                , actions =
                    { onSelectBoat = SelectBoat
                    , onStartSailing = StartSailing
                    , onStartHostMultiplayer = StartHostMultiplayer
                    , onJoinMultiplayer = JoinMultiplayer
                    , onToggleMute = ToggleMute
                    }
                }
