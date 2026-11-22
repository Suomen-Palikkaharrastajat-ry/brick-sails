module Pages.Sail exposing
    ( SailModel
    , SailMsg(..)
    , initSail
    , subscriptions
    , update
    , viewSail
    )

{-| Active 3D sailing simulation gameplay page:
60 FPS physics tick, waypoint progression, procedural Web Audio telemetry,
interactive camera controllers, and HUD overlay.
-}

import Browser.Events
import Game.Audio as GameAudio
import Game.Camera as GameCamera
import Game.Leaderboard as Leaderboard
import Html exposing (Html, div)
import Html.Attributes as Attr
import Json.Decode as Decode
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Pages.Results as Results
import Ports
import Render.Camera as Camera exposing (Camera)
import Render.Scene exposing (Scene)
import Sails.HUD as HUD
import Sails.Physics as Physics
import Sails.Render as Render
import Sails.Types
    exposing
        ( CameraMode(..)
        , Course
        , CourseProgress
        , LeaderboardEntry
        , SailingState
        , SfxEvent(..)
        , ShipSpec
        )


type alias SailModel =
    { spec : ShipSpec
    , scene : Scene
    , course : Course
    , state : SailingState
    , progress : CourseProgress
    , camera : Camera
    , cameraMode : CameraMode
    , orbitYaw : Float
    , orbitPitch : Float
    , orbitDistance : Float
    , simTime : Float
    , audioMuted : Bool
    , personalBest : Maybe LeaderboardEntry
    , viewportWidth : Int
    , viewportHeight : Int
    }


type SailMsg
    = Tick Float
    | KeyDown String
    | KeyUp String
    | TiltOrientation Float
    | SteerLeft
    | SteerRight
    | SheetIn
    | SheetOut
    | Tack
    | CycleCamera
    | ToggleMute
    | ReturnToShipyard
    | RestartCourse
    | CopyShareUrl


initSail : ShipSpec -> Scene -> Course -> Bool -> Maybe LeaderboardEntry -> Int -> Int -> SailModel
initSail spec scene course audioMuted pb width height =
    let
        initState =
            Physics.initSailingState course

        initCam =
            Camera.defaultCamera
    in
    { spec = spec
    , scene = scene
    , course = course
    , state = initState
    , progress =
        { currentWaypointIndex = 0
        , totalWaypoints = List.length course.waypoints
        , elapsedSeconds = 0.0
        , splits = []
        , finished = False
        }
    , camera =
        { initCam
            | eye = vec3 0.0 5.0 -12.0
            , target = vec3 0.0 0.5 0.0
            , sceneRadius = 600.0
        }
    , cameraMode = Chase
    , orbitYaw = 0.0
    , orbitPitch = 0.25
    , orbitDistance = 14.0
    , simTime = 0.0
    , audioMuted = audioMuted
    , personalBest = pb
    , viewportWidth = width
    , viewportHeight = height
    }


update : SailMsg -> SailModel -> ( SailModel, Cmd SailMsg )
update msg model =
    case msg of
        Tick dtMs ->
            let
                dt =
                    clamp 0.001 0.05 (dtMs / 1000.0)

                newSimTime =
                    model.simTime + dt

                -- Step physics
                newState =
                    if model.progress.finished then
                        model.state

                    else
                        Physics.stepPhysics dt newSimTime model.course.wind model.spec model.state

                -- Check waypoint completion
                ( newProgress, sfxCmd ) =
                    if model.progress.finished then
                        ( model.progress, Cmd.none )

                    else
                        checkWaypoints dt newState model.course model.progress

                -- Update camera target and eye
                shipPos =
                    newState.position

                headingRad =
                    newState.heading * pi / 180.0

                forwardVec =
                    vec3 (sin headingRad) 0.0 (cos headingRad)

                ( targetEye, targetLookAt ) =
                    case model.cameraMode of
                        Chase ->
                            let
                                chaseEye =
                                    GameCamera.chasePosition shipPos forwardVec 12.0 4.2
                            in
                            ( chaseEye, shipPos )

                        Orbit ->
                            let
                                orbitEye =
                                    GameCamera.orbitPosition shipPos
                                        { yaw = model.orbitYaw
                                        , pitch = model.orbitPitch
                                        , distance = model.orbitDistance
                                        }
                            in
                            ( orbitEye, shipPos )

                        Helm ->
                            let
                                helmEye =
                                    GameCamera.helmPosition shipPos forwardVec 1.2
                            in
                            ( helmEye, Vec3.add shipPos (Vec3.scale 10.0 forwardVec) )

                oldCam =
                    model.camera

                newEye =
                    GameCamera.dampVec3 6.0 oldCam.eye targetEye dt

                newLookAt =
                    GameCamera.dampVec3 6.0 oldCam.target targetLookAt dt

                newCam =
                    { oldCam | eye = newEye, target = newLookAt }

                -- Send audio telemetry
                audioCmd =
                    if model.audioMuted then
                        Cmd.none

                    else
                        let
                            wSpeed =
                                newState.apparentWindSpeed

                            boatSpeed =
                                newState.waterSpeed
                        in
                        Ports.updateAudioState
                            { active = not model.progress.finished
                            , windGain = clamp 0.0 0.85 (wSpeed / 16.0)
                            , windHz = 280.0 + wSpeed * 70.0
                            , washGain = clamp 0.0 0.75 (boatSpeed / 10.0)
                            , washHz = 140.0 + boatSpeed * 25.0
                            , hullGain = clamp 0.0 0.25 (abs newState.heelAngle / 40.0)
                            , toneHz = 0.0
                            , toneGain = 0.0
                            }
            in
            ( { model
                | state = newState
                , progress = newProgress
                , camera = newCam
                , simTime = newSimTime
              }
            , Cmd.batch [ audioCmd, sfxCmd ]
            )

        SteerLeft ->
            let
                s =
                    model.state

                newRudder =
                    clamp -35.0 35.0 (s.rudderAngle - 10.0)
            in
            ( { model | state = { s | rudderAngle = newRudder } }, Cmd.none )

        SteerRight ->
            let
                s =
                    model.state

                newRudder =
                    clamp -35.0 35.0 (s.rudderAngle + 10.0)
            in
            ( { model | state = { s | rudderAngle = newRudder } }, Cmd.none )

        SheetIn ->
            let
                s =
                    model.state

                newSheet =
                    clamp 0.0 1.0 (s.sailSheet - 0.15)
            in
            ( { model | state = { s | sailSheet = newSheet } }, Cmd.none )

        SheetOut ->
            let
                s =
                    model.state

                newSheet =
                    clamp 0.0 1.0 (s.sailSheet + 0.15)
            in
            ( { model | state = { s | sailSheet = newSheet } }, Cmd.none )

        Tack ->
            let
                s =
                    model.state

                -- Tack maneuver: flip rudder to swing bow through wind
                newHeading =
                    modBy 360 (round (s.heading + (if s.apparentWindAngle > 0 then -70 else 70) + 360))
                        |> toFloat
            in
            ( { model | state = { s | heading = newHeading } }
            , Ports.playSfx (GameAudio.encodeOneShot (GameAudio.Splash 0.5))
            )

        TiltOrientation tiltNorm ->
            let
                s =
                    model.state

                targetRudder =
                    clamp -35.0 35.0 (tiltNorm * 35.0)
            in
            ( { model | state = { s | rudderAngle = targetRudder } }, Cmd.none )

        CycleCamera ->
            let
                nextMode =
                    case model.cameraMode of
                        Chase ->
                            Orbit

                        Orbit ->
                            Helm

                        Helm ->
                            Chase
            in
            ( { model | cameraMode = nextMode }, Cmd.none )

        ToggleMute ->
            let
                nextMuted =
                    not model.audioMuted
            in
            ( { model | audioMuted = nextMuted }, Ports.setAudioMuted nextMuted )

        KeyDown key ->
            case key of
                "ArrowLeft" ->
                    update SteerLeft model

                "KeyA" ->
                    update SteerLeft model

                "ArrowRight" ->
                    update SteerRight model

                "KeyD" ->
                    update SteerRight model

                "ArrowUp" ->
                    update SheetIn model

                "KeyW" ->
                    update SheetIn model

                "ArrowDown" ->
                    update SheetOut model

                "KeyS" ->
                    update SheetOut model

                "Space" ->
                    update Tack model

                "KeyC" ->
                    update CycleCamera model

                "KeyM" ->
                    update ToggleMute model

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            case key of
                "ArrowLeft" ->
                    let
                        s =
                            model.state
                    in
                    ( { model | state = { s | rudderAngle = 0.0 } }, Cmd.none )

                "ArrowRight" ->
                    let
                        s =
                            model.state
                    in
                    ( { model | state = { s | rudderAngle = 0.0 } }, Cmd.none )

                "KeyA" ->
                    let
                        s =
                            model.state
                    in
                    ( { model | state = { s | rudderAngle = 0.0 } }, Cmd.none )

                "KeyD" ->
                    let
                        s =
                            model.state
                    in
                    ( { model | state = { s | rudderAngle = 0.0 } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RestartCourse ->
            ( initSail model.spec model.scene model.course model.audioMuted model.personalBest model.viewportWidth model.viewportHeight
            , Cmd.none
            )

        CopyShareUrl ->
            ( model, Ports.copyShareUrl "" )

        ReturnToShipyard ->
            ( model, Cmd.none )


checkWaypoints : Float -> SailingState -> Course -> CourseProgress -> ( CourseProgress, Cmd SailMsg )
checkWaypoints dt state course progress =
    let
        newElapsed =
            progress.elapsedSeconds + dt

        currentWpMaybe =
            List.drop progress.currentWaypointIndex course.waypoints
                |> List.head
    in
    case currentWpMaybe of
        Just wp ->
            let
                sPos =
                    Vec3.toRecord state.position

                wPos =
                    Vec3.toRecord wp.position

                dx =
                    sPos.x - wPos.x

                dz =
                    sPos.z - wPos.z

                distSq =
                    dx * dx + dz * dz

                hitRadius =
                    wp.radius
            in
            if distSq <= (hitRadius * hitRadius) then
                let
                    nextIdx =
                        progress.currentWaypointIndex + 1

                    isLast =
                        nextIdx >= progress.totalWaypoints

                    newSplits =
                        progress.splits ++ [ newElapsed ]

                    sfx =
                        if isLast then
                            Ports.playSfx (GameAudio.encodeOneShot GameAudio.Finish)

                        else
                            Ports.playSfx (GameAudio.encodeOneShot GameAudio.Bell)
                in
                ( { progress
                    | currentWaypointIndex = nextIdx
                    , elapsedSeconds = newElapsed
                    , splits = newSplits
                    , finished = isLast
                  }
                , sfx
                )

            else
                ( { progress | elapsedSeconds = newElapsed }, Cmd.none )

        Nothing ->
            ( { progress | elapsedSeconds = newElapsed, finished = True }, Cmd.none )


subscriptions : SailModel -> Sub SailMsg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "code" Decode.string))
        , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "code" Decode.string))
        , Ports.tiltOrientationReceived TiltOrientation
        ]


viewSail :
    SailModel
    ->
        { onReturnToShipyard : SailMsg
        , onRestartCourse : SailMsg
        }
    -> Html SailMsg
viewSail model callbacks =
    div [ Attr.class "relative w-full h-full overflow-hidden bg-slate-950 select-none font-sans" ]
        [ -- 1. Fullscreen WebGL 3D Sailing Viewport
          div [ Attr.class "absolute inset-0 z-0" ]
            [ Render.renderSailingViewport
                model.camera
                model.state
                model.spec
                model.scene
                model.course
                model.simTime
                model.viewportWidth
                model.viewportHeight
            ]
        , -- 2. HUD Overlay
          HUD.viewHudOverlay
            { state = model.state
            , spec = model.spec
            , course = model.course
            , progress = model.progress
            , audioMuted = model.audioMuted
            , cameraMode = model.cameraMode
            , actions =
                { onToggleMute = ToggleMute
                , onCycleCamera = CycleCamera
                , onReturnToShipyard = callbacks.onReturnToShipyard
                , onRestartCourse = callbacks.onRestartCourse
                , onSteerLeft = SteerLeft
                , onSteerRight = SteerRight
                , onSheetIn = SheetIn
                , onSheetOut = SheetOut
                , onTack = Tack
                }
            }
        , -- 3. Finished Regatta Results Modal
          if model.progress.finished then
            Results.viewResults
                { course = model.course
                , progress = model.progress
                , spec = model.spec
                , personalBest = model.personalBest
                , actions =
                    { onSailAgain = callbacks.onRestartCourse
                    , onReturnToShipyard = callbacks.onReturnToShipyard
                    , onCopyShareUrl = CopyShareUrl
                    }
                }

          else
            div [] []
        ]
