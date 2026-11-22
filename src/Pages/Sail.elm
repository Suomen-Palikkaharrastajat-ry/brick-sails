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
import FeatherIcons
import Game.Audio as GameAudio
import Game.Camera as GameCamera
import Game.Leaderboard as Leaderboard
import Game.Multiplayer as Multiplayer exposing (RemotePlayer, Role(..), RoomState, RoomStatus(..))
import Game.MultiplayerUI as MultiplayerUI
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
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
        , Cannonball
        , CombatImpact
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
    , multiplayer : Maybe RoomState
    , cannonballs : List Cannonball
    , impacts : List CombatImpact
    , cannonCooldown : Float
    , hitsScored : Int
    , hitsTaken : Int
    , nextCannonId : Int
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
    | FireCannons
    | CycleCamera
    | ToggleMute
    | ReturnToShipyard
    | RestartCourse
    | CopyShareUrl
    | StartMultiplayerCountdown
    | UpdateMultiplayerRoom RoomState


initSail : ShipSpec -> Scene -> Course -> Bool -> Maybe LeaderboardEntry -> Maybe RoomState -> Int -> Int -> SailModel
initSail spec scene course audioMuted pb maybeMulti width height =
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
    , multiplayer = maybeMulti
    , cannonballs = []
    , impacts = []
    , cannonCooldown = 0.0
    , hitsScored = 0
    , hitsTaken = 0
    , nextCannonId = 1
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

                newCooldown =
                    max 0.0 (model.cannonCooldown - dt)

                -- Step physics
                steppedState =
                    if model.progress.finished then
                        model.state

                    else
                        Physics.stepPhysics dt newSimTime model.course.wind model.spec model.state

                -- Ballistic cannonball projectile flight and water surface impacts
                ( steppedBalls, waterSplashes ) =
                    Physics.stepCombatProjectiles dt newSimTime model.cannonballs

                mySlotNum =
                    case model.multiplayer of
                        Just r ->
                            r.mySlot

                        Nothing ->
                            1

                localShipTarget =
                    { slot = mySlotNum
                    , position = steppedState.position
                    , radius = max 1.2 (model.spec.lengthMeters * 0.4)
                    }

                remoteShipTargets =
                    case model.multiplayer of
                        Just r ->
                            List.filter (\p -> p.slot /= r.mySlot) r.players
                                |> List.map
                                    (\p ->
                                        { slot = p.slot
                                        , position = p.position
                                        , radius = max 1.2 (model.spec.lengthMeters * 0.4)
                                        }
                                    )

                        Nothing ->
                            []

                allTargets =
                    localShipTarget :: remoteShipTargets

                ( survBalls, shipHitImpacts, hitSlots ) =
                    Physics.checkCombatShipHits steppedBalls allTargets

                allNewImpacts =
                    waterSplashes ++ shipHitImpacts

                steppedImpacts =
                    Physics.stepCombatImpacts dt (model.impacts ++ allNewImpacts)

                localWasHit =
                    List.member mySlotNum hitSlots

                scoredCount =
                    List.filter (\s -> s /= mySlotNum) hitSlots |> List.length

                newState =
                    if localWasHit then
                        { steppedState
                            | heelAngle = steppedState.heelAngle + 5.0
                            , pitchAngle = steppedState.pitchAngle - 2.5
                        }

                    else
                        steppedState

                combatSfxCmd =
                    if not (List.isEmpty shipHitImpacts) then
                        Ports.playSfx (GameAudio.encodeOneShot (GameAudio.Impact 1.0))

                    else if not (List.isEmpty waterSplashes) then
                        Ports.playSfx (GameAudio.encodeOneShot (GameAudio.Splash 0.5))

                    else
                        Cmd.none

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

                -- Send multiplayer telemetry if in a room
                telemetryCmd =
                    case model.multiplayer of
                        Just room ->
                            let
                                sPos =
                                    Vec3.toRecord newState.position

                                progRatio =
                                    toFloat newProgress.currentWaypointIndex / toFloat (max 1 newProgress.totalWaypoints)
                            in
                            Ports.sendMultiplayerTelemetry
                                { pos = [ sPos.x, sPos.y, sPos.z ]
                                , rot = [ newState.pitchAngle, newState.heading, newState.heelAngle ]
                                , speed = newState.waterSpeed
                                , progress = progRatio
                                , finished = newProgress.finished
                                }

                        Nothing ->
                            Cmd.none
            in
            ( { model
                | state = newState
                , progress = newProgress
                , camera = newCam
                , simTime = newSimTime
                , cannonballs = survBalls
                , impacts = steppedImpacts
                , cannonCooldown = newCooldown
                , hitsScored = model.hitsScored + scoredCount
                , hitsTaken = model.hitsTaken + (if localWasHit then 1 else 0)
              }
            , Cmd.batch [ audioCmd, sfxCmd, combatSfxCmd, telemetryCmd ]
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

        FireCannons ->
            if model.cannonCooldown <= 0.0 then
                let
                    mySlotNum =
                        case model.multiplayer of
                            Just r ->
                                r.mySlot

                            Nothing ->
                                1

                    ( newBalls, recoilState ) =
                        Physics.fireCannons mySlotNum model.spec model.state model.nextCannonId

                    fireSfx =
                        Ports.playSfx (GameAudio.encodeOneShot (GameAudio.Impact 0.8))
                in
                ( { model
                    | cannonballs = model.cannonballs ++ newBalls
                    , cannonCooldown = 1.2
                    , nextCannonId = model.nextCannonId + List.length newBalls
                    , state = recoilState
                  }
                , fireSfx
                )

            else
                ( model, Cmd.none )

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
                    update FireCannons model

                "KeyF" ->
                    update FireCannons model

                "KeyT" ->
                    update Tack model

                "KeyX" ->
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
            ( initSail model.spec model.scene model.course model.audioMuted model.personalBest model.multiplayer model.viewportWidth model.viewportHeight
            , Cmd.none
            )

        CopyShareUrl ->
            let
                roomArg =
                    case model.multiplayer of
                        Just r ->
                            "#room=" ++ r.roomId

                        Nothing ->
                            ""
            in
            ( model, Ports.copyShareUrl roomArg )

        StartMultiplayerCountdown ->
            ( model, Ports.startMultiplayerCountdown () )

        UpdateMultiplayerRoom room ->
            ( { model | multiplayer = Just room }, Cmd.none )

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
    case model.multiplayer of
        Just room ->
            case room.role of
                HostCommentator ->
                    viewHostCommentatorSplitScreen model callbacks room

                JoinedPlayer ->
                    viewSinglePlayerSail model callbacks (Just room)

        Nothing ->
            viewSinglePlayerSail model callbacks Nothing


viewHostCommentatorSplitScreen :
    SailModel
    ->
        { onReturnToShipyard : SailMsg
        , onRestartCourse : SailMsg
        }
    -> RoomState
    -> Html SailMsg
viewHostCommentatorSplitScreen model callbacks room =
    let
        qW =
            max 160 (model.viewportWidth // 2)

        qH =
            max 120 (model.viewportHeight // 2)

        allShips =
            List.map
                (\p ->
                    let
                        remState =
                            { position = p.position
                            , heading = (Vec3.toRecord p.rotation).y
                            , heelAngle = (Vec3.toRecord p.rotation).z
                            , pitchAngle = (Vec3.toRecord p.rotation).x
                            , rudderAngle = 0.0
                            , sailSheet = 0.5
                            , waterSpeed = p.speed
                            , apparentWindSpeed = 12.0
                            , apparentWindAngle = 45.0
                            }
                    in
                    { spec = model.spec
                    , scene = model.scene
                    , state = remState
                    , slot = p.slot
                    }
                )
                room.players

        renderQuadrant slot =
            let
                playerMaybe =
                    List.filter (\p -> p.slot == slot) room.players
                        |> List.head
            in
            case playerMaybe of
                Just p ->
                    let
                        remHeading =
                            (Vec3.toRecord p.rotation).y

                        headingRad =
                            remHeading * pi / 180.0

                        fwd =
                            vec3 (sin headingRad) 0.0 (cos headingRad)

                        camEye =
                            GameCamera.chasePosition p.position fwd 14.0 4.8

                        cam =
                            { eye = camEye
                            , target = p.position
                            , up = vec3 0 1 0
                            , fov = 55.0
                            , near = 0.1
                            , far = 1200.0
                            , sceneRadius = 600.0
                            }
                    in
                    div [ Attr.class "relative w-full h-full overflow-hidden border border-slate-800 bg-slate-950" ]
                        [ Render.renderSailingViewportMulti cam allShips model.cannonballs model.impacts model.course model.simTime qW qH
                        , MultiplayerUI.viewPlayerBadge
                            { slot = slot
                            , name = p.name
                            , speedLabel = String.fromFloat (toFloat (round (p.speed * 10)) / 10.0) ++ " kn"
                            , finished = p.finished
                            }
                        ]

                Nothing ->
                    MultiplayerUI.viewWaitingSlotCard slot room.roomId "sailor"
    in
    div [ Attr.class "relative w-full h-full overflow-hidden bg-slate-950 select-none font-sans" ]
        [ -- 2x2 Grid Viewports
          div [ Attr.class "grid grid-cols-2 grid-rows-2 w-full h-full" ]
            [ renderQuadrant 1
            , renderQuadrant 2
            , renderQuadrant 3
            , renderQuadrant 4
            ]
        , -- Top Commentator Bar
          MultiplayerUI.viewCommentatorHeader
            { room = room
            , startLabel = "START REGATTA"
            , onCopyShareLink = CopyShareUrl
            , onStartCountdown = StartMultiplayerCountdown
            }
        , -- Synchronized Countdown Overlay
          case room.status of
            Countdown n ->
                MultiplayerUI.viewCountdownOverlay { secondsRemaining = n }

            _ ->
                text ""
        ]


viewSinglePlayerSail :
    SailModel
    ->
        { onReturnToShipyard : SailMsg
        , onRestartCourse : SailMsg
        }
    -> Maybe RoomState
    -> Html SailMsg
viewSinglePlayerSail model callbacks maybeRoom =
    let
        mySlotNum =
            case maybeRoom of
                Just r ->
                    r.mySlot

                Nothing ->
                    1

        localShip =
            { spec = model.spec
            , scene = model.scene
            , state = model.state
            , slot = mySlotNum
            }

        remoteShips =
            case maybeRoom of
                Just r ->
                    List.filter (\p -> p.slot /= r.mySlot) r.players
                        |> List.map
                            (\p ->
                                { spec = model.spec
                                , scene = model.scene
                                , state =
                                    { position = p.position
                                    , heading = (Vec3.toRecord p.rotation).y
                                    , heelAngle = (Vec3.toRecord p.rotation).z
                                    , pitchAngle = (Vec3.toRecord p.rotation).x
                                    , rudderAngle = 0.0
                                    , sailSheet = 0.5
                                    , waterSpeed = p.speed
                                    , apparentWindSpeed = 12.0
                                    , apparentWindAngle = 45.0
                                    }
                                , slot = p.slot
                                }
                            )

                Nothing ->
                    []

        allShips =
            localShip :: remoteShips
    in
    div [ Attr.class "relative w-full h-full overflow-hidden bg-slate-950 select-none font-sans" ]
        [ -- 1. Fullscreen WebGL 3D Sailing Viewport with Local and Remote Ships
          div [ Attr.class "absolute inset-0 z-0" ]
            [ Render.renderSailingViewportMulti
                model.camera
                allShips
                model.cannonballs
                model.impacts
                model.course
                model.simTime
                model.viewportWidth
                model.viewportHeight
            ]
        , -- 2. Multiplayer Room Indicator Banner
          case maybeRoom of
            Just room ->
                div [ Attr.class "absolute top-4 left-4 z-20 flex items-center gap-2 px-3 py-1.5 rounded-2xl bg-slate-950/80 backdrop-blur-md border border-white/10 shadow-lg pointer-events-auto" ]
                    [ span [ Attr.class ("px-2 py-0.5 rounded-lg text-xs font-black uppercase " ++ Multiplayer.slotBadgeClass room.mySlot) ]
                        [ text ("P" ++ String.fromInt room.mySlot) ]
                    , span [ Attr.class "text-xs font-mono font-bold text-sky-400" ] [ text ("Room: " ++ room.roomId) ]
                    , button
                        [ onClick CopyShareUrl
                        , Attr.title "Copy room link"
                        , Attr.class "p-1 rounded-lg bg-slate-800 hover:bg-slate-700 text-slate-300 transition-colors"
                        ]
                        [ FeatherIcons.share2 |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml [] ]
                    ]

            Nothing ->
                text ""
        , -- 3. HUD Overlay
          HUD.viewHudOverlay
            { state = model.state
            , spec = model.spec
            , course = model.course
            , progress = model.progress
            , audioMuted = model.audioMuted
            , cameraMode = model.cameraMode
            , cannonCooldown = model.cannonCooldown
            , hitsScored = model.hitsScored
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
                , onFireCannons = FireCannons
                }
            }
        , -- 4. Synchronized Countdown Overlay
          case maybeRoom of
            Just room ->
                case room.status of
                    Countdown n ->
                        div [ Attr.class "absolute inset-0 z-50 flex flex-col items-center justify-center bg-black/60 pointer-events-none select-none backdrop-blur-sm" ]
                            [ div [ Attr.class "text-9xl font-black text-amber-400 animate-pulse drop-shadow-2xl font-mono" ]
                                [ text (if n > 0 then String.fromInt n else "GO!") ]
                            ]

                    _ ->
                        text ""

            Nothing ->
                text ""
        , -- 5. Finished Regatta Results Modal
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
