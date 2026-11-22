module Main exposing (Flags, Model, Msg, main)

{-| Top-level Elm Architecture entry point for Brick Sails (`brick-sails`).
Deployed to https://ohoi.palikkaharrastajat.fi.
-}

import Browser
import Browser.Dom as Dom
import Browser.Events
import Dict
import Game.Leaderboard as Leaderboard exposing (Leaderboard)
import Game.Multiplayer as Multiplayer exposing (Role(..), RoomState)
import Html exposing (Html, div)
import Html.Attributes as Attr
import Json.Decode as Decode
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import Math.Vector3 as Vec3 exposing (vec3)
import Pages.Results as Results
import Pages.Sail as Sail
import Pages.Shipyard as Shipyard
import Ports
import Render.Camera as Camera exposing (Camera)
import Render.Scene as Scene exposing (Scene)
import Sails.Analyze as Analyze exposing (AnalysisReport)
import Sails.CameraRig as CameraRig
import Sails.Ingest as Ingest
import Sails.Physics as Physics
import Sails.Types
    exposing
        ( Course
        , CourseId(..)
        , CourseProgress
        , LeaderboardEntry
        , ShipSpec
        , ViewMode(..)
        )
import Studio.HashPayload as HashPayload
import Task


type alias Flags =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , indicesBase : String
    , initialHash : String
    , predecodedInitialBytes : Maybe (List Int)
    , coarsePointer : Bool
    , language : String
    , savedAudioMuted : Bool
    , savedLeaderboardJson : String
    , baseUrl : String
    }


type alias Model =
    { currentHash : String
    , spec : ShipSpec
    , scene : Scene
    , report : AnalysisReport
    , course : Course
    , viewMode : ViewMode
    , shipyardCamera : Camera
    , sailModel : Maybe Sail.SailModel
    , audioMuted : Bool
    , leaderboard : Leaderboard
    , viewportWidth : Int
    , viewportHeight : Int
    , baseUrl : String
    }


type Msg
    = SelectPreset String
    | SelectCourse CourseId
    | StartSailing
    | StartHostMultiplayer
    | JoinMultiplayer
    | MultiplayerRoomChanged Decode.Value
    | MultiplayerError String
    | ReturnToShipyard
    | RestartCourse
    | ToggleMute
    | CopyShareUrl
    | OpenQrScanner
    | QrCodeScanned String
    | QrScannerError String
    | HashChanged String
    | BinaryHashDecoded (Maybe (List Int))
    | FileContentReceived String
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
    let
        activeHash =
            if String.isEmpty flags.initialHash then
                Ingest.defaultShipHash

            else
                flags.initialHash

        spec =
            Ingest.ingestFromPlacements "Micro Sloop"
                [ ( "3660b.dat", vec3 40.0 -24.0 10.0 )
                , ( "3660b.dat", vec3 40.0 -24.0 70.0 )
                , ( "3003.dat", vec3 40.0 -24.0 40.0 )
                , ( "3068b.dat", vec3 40.0 -32.0 0.0 )
                , ( "3068b.dat", vec3 40.0 -32.0 80.0 )
                , ( "2335.dat", vec3 40.0 -124.0 40.0 )
                , ( "3957a.dat", vec3 40.0 -40.0 40.0 )
                , ( "87580.dat", vec3 40.0 -32.0 40.0 )
                ]

        report =
            Analyze.analyzeShip spec

        shipyardCam =
            Camera.init
                |> Camera.setSceneRadius 15.0
                |> CameraRig.lookAt (vec3 4.5 3.2 5.5) (vec3 0.0 0.2 0.0)

        board =
            Decode.decodeString Leaderboard.decodeLeaderboard flags.savedLeaderboardJson
                |> Result.withDefault Leaderboard.empty

        initialModel =
            { currentHash = activeHash
            , spec = spec
            , scene = Scene.empty
            , report = report
            , course = Physics.defaultCourse
            , viewMode = ViewShipyard
            , shipyardCamera = shipyardCam
            , sailModel = Nothing
            , audioMuted = flags.savedAudioMuted
            , leaderboard = board
            , viewportWidth = 1024
            , viewportHeight = 768
            , baseUrl = flags.baseUrl
            }

        -- Decode predecoded initial bytes if provided
        initialCmd =
            case flags.predecodedInitialBytes of
                Just bytes ->
                    case HashPayload.decode (HashPayload.listToBytes bytes) of
                        Just payload ->
                            let
                                ( newModel, _ ) =
                                    applyDocument payload.document activeHash initialModel
                            in
                            Task.perform (\_ -> ViewportResized { width = 1024, height = 768 }) (Task.succeed ())

                        Nothing ->
                            Cmd.none

                Nothing ->
                    Ports.decodeBinaryHash activeHash
    in
    ( initialModel, initialCmd )


applyDocument : Document -> String -> Model -> ( Model, Cmd Msg )
applyDocument doc hash model =
    let
        spec =
            Ingest.ingestDocument "Custom Sloop" doc

        report =
            Analyze.analyzeShip spec
    in
    ( { model
        | currentHash = hash
        , spec = spec
        , report = report
      }
    , Ports.setUrlHash hash
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPreset hash ->
            ( model, Ports.decodeBinaryHash hash )

        SelectCourse cid ->
            ( model, Cmd.none )

        StartSailing ->
            let
                pb =
                    Leaderboard.bestForCourse (Sails.Types.courseIdToString model.course.id) model.leaderboard

                sail =
                    Sail.initSail model.spec model.scene model.course model.audioMuted pb Nothing model.viewportWidth model.viewportHeight
            in
            ( { model
                | viewMode = ViewSailing
                , sailModel = Just sail
              }
            , Ports.requestTiltPermission ()
            )

        StartHostMultiplayer ->
            ( model
            , Ports.createMultiplayerRoom
                { roomId = ""
                , courseId = Sails.Types.courseIdToString model.course.id
                , name = "Host Commentator"
                , vehicleHash = model.currentHash
                }
            )

        JoinMultiplayer ->
            ( model
            , Ports.joinMultiplayerRoom
                { roomId = ""
                , name = "Sailor"
                , vehicleHash = model.currentHash
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
                            ( { model | sailModel = Just nextSail }
                            , Cmd.map SailMessage sCmd
                            )

                        Nothing ->
                            let
                                pb =
                                    Leaderboard.bestForCourse (Sails.Types.courseIdToString model.course.id) model.leaderboard

                                newSail =
                                    Sail.initSail model.spec model.scene model.course model.audioMuted pb (Just room) model.viewportWidth model.viewportHeight
                            in
                            ( { model
                                | viewMode = ViewSailing
                                , sailModel = Just newSail
                              }
                            , Cmd.batch
                                [ Ports.setUrlHash ("#room=" ++ room.roomId)
                                , Ports.requestTiltPermission ()
                                ]
                            )

                Err _ ->
                    ( model, Cmd.none )

        MultiplayerError _ ->
            ( model, Cmd.none )

        ReturnToShipyard ->
            ( { model
                | viewMode = ViewShipyard
                , sailModel = Nothing
              }
            , Cmd.batch
                [ Ports.leaveMultiplayerRoom ()
                , Ports.updateAudioState
                    { active = False
                    , windGain = 0.0
                    , windHz = 350.0
                    , washGain = 0.0
                    , washHz = 180.0
                    , hullGain = 0.0
                    , toneHz = 0.0
                    , toneGain = 0.0
                    }
                ]
            )

        RestartCourse ->
            update StartSailing model

        ToggleMute ->
            let
                nextMuted =
                    not model.audioMuted

                updatedSail =
                    model.sailModel
                        |> Maybe.map (\s -> { s | audioMuted = nextMuted })
            in
            ( { model | audioMuted = nextMuted, sailModel = updatedSail }
            , Ports.setAudioMuted nextMuted
            )

        CopyShareUrl ->
            ( model, Ports.copyShareUrl model.currentHash )

        OpenQrScanner ->
            ( model, Ports.openQrScanner () )

        QrCodeScanned scannedHash ->
            ( { model | currentHash = scannedHash }, Ports.decodeBinaryHash scannedHash )

        QrScannerError _ ->
            ( model, Cmd.none )

        HashChanged newHash ->
            ( { model | currentHash = newHash }, Ports.decodeBinaryHash newHash )

        BinaryHashDecoded maybeBytes ->
            case maybeBytes of
                Just bytes ->
                    case HashPayload.decode (HashPayload.listToBytes bytes) of
                        Just payload ->
                            applyDocument payload.document model.currentHash model

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FileContentReceived fileText ->
            case Parser.parse fileText of
                Ok doc ->
                    applyDocument doc model.currentHash model

                Err _ ->
                    ( model, Cmd.none )

        ViewportResized dims ->
            let
                updatedSail =
                    model.sailModel
                        |> Maybe.map (\s -> { s | viewportWidth = dims.width, viewportHeight = dims.height })
            in
            ( { model
                | viewportWidth = dims.width
                , viewportHeight = dims.height
                , sailModel = updatedSail
              }
            , Cmd.none
            )

        SailMessage sMsg ->
            case sMsg of
                Sail.ReturnToShipyard ->
                    update ReturnToShipyard model

                _ ->
                    case model.sailModel of
                        Just sModel ->
                            let
                                ( nextSail, sCmd ) =
                                    Sail.update sMsg sModel
                            in
                            ( { model | sailModel = Just nextSail }
                            , Cmd.map SailMessage sCmd
                            )

                        Nothing ->
                            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.hashChanged HashChanged
        , Ports.binaryHashDecoded BinaryHashDecoded
        , Ports.fileContentReceived FileContentReceived
        , Ports.viewportResized ViewportResized
        , Ports.multiplayerRoomStateChanged MultiplayerRoomChanged
        , Ports.multiplayerError MultiplayerError
        , Ports.qrCodeScanned QrCodeScanned
        , Ports.qrScannerError QrScannerError
        , case ( model.viewMode, model.sailModel ) of
            ( ViewSailing, Just sModel ) ->
                Sub.map SailMessage (Sail.subscriptions sModel)

            _ ->
                Sub.none
        ]


view : Model -> Html Msg
view model =
    case model.viewMode of
        ViewShipyard ->
            Shipyard.viewShipyard
                { spec = model.spec
                , scene = model.scene
                , camera = model.shipyardCamera
                , report = model.report
                , selectedCourse = model.course.id
                , audioMuted = model.audioMuted
                , currentHash = model.currentHash
                , viewportWidth = model.viewportWidth
                , viewportHeight = model.viewportHeight
                , actions =
                    { onSelectPreset = SelectPreset
                    , onSelectCourse = SelectCourse
                    , onStartSailing = StartSailing
                    , onStartHostMultiplayer = StartHostMultiplayer
                    , onJoinMultiplayer = JoinMultiplayer
                    , onCopyShareUrl = CopyShareUrl
                    , onOpenQrScanner = OpenQrScanner
                    , onToggleMute = ToggleMute
                    }
                }

        ViewSailing ->
            case model.sailModel of
                Just sModel ->
                    Html.map SailMessage
                        (Sail.viewSail sModel
                            { onReturnToShipyard = Sail.ReturnToShipyard
                            , onRestartCourse = Sail.RestartCourse
                            }
                        )

                Nothing ->
                    div [] []

        ViewResults ->
            div [] []
