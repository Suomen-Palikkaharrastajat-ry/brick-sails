module Pages.Sail exposing
    ( SailModel
    , SailMsg(..)
    , initSail
    , subscriptions
    , update
    , viewSail
    )

{-| The race: a 3-2-1 start, then sail through the buoys in order while a
follow camera keeps the boat and the next buoy on screen. Cannons lob a ball
at the nearest floating target in range; each hit takes 2 s off the time.
-}

import Browser.Events
import FeatherIcons
import Game.Audio as GameAudio
import Game.Multiplayer as Multiplayer exposing (Role(..), RoomState, RoomStatus(..))
import Game.MultiplayerUI as MultiplayerUI
import Game.Ports as GamePorts
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Pages.Results as Results
import Ports
import Sails.CameraRig as CameraRig exposing (Follow)
import Sails.HUD as HUD exposing (Side(..))
import Sails.Physics as Physics
import Sails.Render as Render exposing (RenderBoat)
import Sails.ShipMesh exposing (ShipMesh)
import Sails.Types
    exposing
        ( Boat
        , Cannonball
        , CombatImpact
        , Course
        , CourseProgress
        , LeaderboardEntry
        , SailingState
        , Target
        )


type Phase
    = Countdown Float
    | Racing
    | WaitingForRoom


type alias SailModel =
    { boat : Boat
    , course : Course
    , state : SailingState
    , progress : CourseProgress
    , phase : Phase
    , follow : Follow
    , simTime : Float
    , audioMuted : Bool
    , personalBest : Maybe LeaderboardEntry
    , multiplayer : Maybe RoomState
    , targets : List Target
    , cannonballs : List Cannonball
    , impacts : List CombatImpact
    , cannonCooldown : Float
    , nextCannonId : Int
    , keyLeft : Bool
    , keyRight : Bool
    , touchLeft : Bool
    , touchRight : Bool
    , tiltSteer : Float
    , tilt : { available : Bool, enabled : Bool }
    , toast : Maybe ( String, Float )
    , viewportWidth : Int
    , viewportHeight : Int
    }


type SailMsg
    = Tick Float
    | KeyDown String
    | KeyUp String
    | Steer Side Bool
    | TiltSteer Float
    | ReleaseControls
    | FireCannons
    | ToggleMute
    | ReturnToShipyard
    | RestartCourse
    | CopyShareUrl
    | StartMultiplayerCountdown
    | UpdateMultiplayerRoom RoomState


countdownSeconds : Float
countdownSeconds =
    3.0


cannonReload : Float
cannonReload =
    0.8


initSail :
    Boat
    -> Course
    -> Bool
    -> Maybe LeaderboardEntry
    -> Maybe RoomState
    -> { available : Bool, enabled : Bool }
    -> Int
    -> Int
    -> SailModel
initSail boat course audioMuted pb maybeMulti tilt width height =
    let
        state =
            Physics.initSailingState course

        heldByRoom =
            case maybeMulti of
                Just room ->
                    case room.status of
                        Multiplayer.Countdown _ ->
                            True

                        _ ->
                            False

                Nothing ->
                    False
    in
    { boat = boat
    , course = course
    , state = state
    , progress = Physics.initProgress course
    , phase =
        if heldByRoom then
            WaitingForRoom

        else
            Countdown countdownSeconds
    , follow = CameraRig.initFollow state.position state.heading
    , simTime = 0
    , audioMuted = audioMuted
    , personalBest = pb
    , multiplayer = maybeMulti
    , targets = course.targets
    , cannonballs = []
    , impacts = []
    , cannonCooldown = 0
    , nextCannonId = 1
    , keyLeft = False
    , keyRight = False
    , touchLeft = False
    , touchRight = False
    , tiltSteer = 0
    , tilt = tilt
    , toast = Nothing
    , viewportWidth = width
    , viewportHeight = height
    }


sfx : GameAudio.OneShotSound -> Cmd msg
sfx sound =
    Ports.playSfx (GameAudio.encodeOneShot sound)


mySlot : SailModel -> Int
mySlot model =
    model.multiplayer |> Maybe.map .mySlot |> Maybe.withDefault 1


steerInput : SailModel -> Float
steerInput model =
    let
        held a b =
            if a || b then
                1

            else
                0

        tilt =
            if model.tilt.enabled then
                model.tiltSteer

            else
                0
    in
    clamp -1 1 (held model.keyRight model.touchRight - held model.keyLeft model.touchLeft + tilt)


nextBuoy : SailModel -> Maybe Vec3
nextBuoy model =
    if model.progress.finished then
        Nothing

    else
        List.drop model.progress.currentWaypointIndex model.course.waypoints
            |> List.head
            |> Maybe.map .position


aimPoints : SailModel -> List Vec3
aimPoints model =
    let
        targets =
            model.targets |> List.filter (not << .sunk) |> List.map .position

        ships =
            remotePlayers model |> List.map .position
    in
    targets ++ ships


remotePlayers : SailModel -> List Multiplayer.RemotePlayer
remotePlayers model =
    case model.multiplayer of
        Just r ->
            List.filter (\p -> p.slot /= r.mySlot) r.players

        Nothing ->
            []


targetInRange : SailModel -> Bool
targetInRange model =
    aimPoints model
        |> List.any (\p -> horizontal model.state.position p <= Physics.cannonRange)


horizontal : Vec3 -> Vec3 -> Float
horizontal a b =
    let
        pa =
            Vec3.toRecord a

        pb =
            Vec3.toRecord b
    in
    sqrt ((pa.x - pb.x) ^ 2 + (pa.z - pb.z) ^ 2)


update : SailMsg -> SailModel -> ( SailModel, Cmd SailMsg )
update msg model =
    case msg of
        Tick dtMs ->
            tick (clamp 0.001 0.05 (dtMs / 1000)) model

        Steer Left down ->
            ( { model | touchLeft = down }, Cmd.none )

        Steer Right down ->
            ( { model | touchRight = down }, Cmd.none )

        TiltSteer v ->
            ( { model | tiltSteer = clamp -1 1 v }, Cmd.none )

        ReleaseControls ->
            ( { model | keyLeft = False, keyRight = False, touchLeft = False, touchRight = False }, Cmd.none )

        FireCannons ->
            if model.cannonCooldown <= 0 && model.phase == Racing then
                let
                    balls =
                        Physics.fireCannons (mySlot model) model.state (aimPoints model) model.nextCannonId
                in
                ( { model
                    | cannonballs = model.cannonballs ++ balls
                    , cannonCooldown = cannonReload
                    , nextCannonId = model.nextCannonId + List.length balls
                  }
                , sfx (GameAudio.Impact 0.8)
                )

            else
                ( model, Cmd.none )

        ToggleMute ->
            ( { model | audioMuted = not model.audioMuted }, GamePorts.setAudioMuted (not model.audioMuted) )

        KeyDown key ->
            case key of
                "ArrowLeft" ->
                    ( { model | keyLeft = True }, Cmd.none )

                "KeyA" ->
                    ( { model | keyLeft = True }, Cmd.none )

                "ArrowRight" ->
                    ( { model | keyRight = True }, Cmd.none )

                "KeyD" ->
                    ( { model | keyRight = True }, Cmd.none )

                "Space" ->
                    update FireCannons model

                "KeyF" ->
                    update FireCannons model

                "ArrowUp" ->
                    update FireCannons model

                "KeyM" ->
                    update ToggleMute model

                "KeyR" ->
                    update RestartCourse model

                "Enter" ->
                    if model.progress.finished then
                        update RestartCourse model

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            case key of
                "ArrowLeft" ->
                    ( { model | keyLeft = False }, Cmd.none )

                "KeyA" ->
                    ( { model | keyLeft = False }, Cmd.none )

                "ArrowRight" ->
                    ( { model | keyRight = False }, Cmd.none )

                "KeyD" ->
                    ( { model | keyRight = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RestartCourse ->
            let
                fresh =
                    initSail model.boat model.course model.audioMuted model.personalBest model.multiplayer model.tilt model.viewportWidth model.viewportHeight
            in
            ( { fresh | multiplayer = model.multiplayer }, GamePorts.calibrateTilt () )

        CopyShareUrl ->
            ( model
            , GamePorts.copyShareUrl
                (model.multiplayer |> Maybe.map (\r -> "#room=" ++ r.roomId) |> Maybe.withDefault "")
            )

        StartMultiplayerCountdown ->
            ( model, GamePorts.startMultiplayerCountdown () )

        UpdateMultiplayerRoom room ->
            let
                wasCounting =
                    case model.multiplayer |> Maybe.map .status of
                        Just (Multiplayer.Countdown _) ->
                            True

                        _ ->
                            False
            in
            case ( room.status, room.role ) of
                ( Multiplayer.Countdown _, JoinedPlayer ) ->
                    if wasCounting then
                        ( { model | multiplayer = Just room }, Cmd.none )

                    else
                        -- The host started the race: everyone back to the start line.
                        let
                            fresh =
                                initSail model.boat model.course model.audioMuted model.personalBest (Just room) model.tilt model.viewportWidth model.viewportHeight
                        in
                        ( { fresh | phase = WaitingForRoom }, GamePorts.calibrateTilt () )

                ( InRace, _ ) ->
                    if model.phase == WaitingForRoom then
                        ( { model | multiplayer = Just room, phase = Racing, toast = Just ( "GO!", 0.8 ) }, sfx GameAudio.Horn )

                    else
                        ( { model | multiplayer = Just room }, Cmd.none )

                _ ->
                    ( { model | multiplayer = Just room }, Cmd.none )

        ReturnToShipyard ->
            ( model, Cmd.none )


tick : Float -> SailModel -> ( SailModel, Cmd SailMsg )
tick dt model =
    let
        time =
            model.simTime + dt

        ( phase, phaseCmd, goToast ) =
            case model.phase of
                Countdown t ->
                    if t - dt <= 0 then
                        ( Racing, sfx GameAudio.Horn, Just ( "GO!", 0.8 ) )

                    else
                        ( Countdown (t - dt), Cmd.none, Nothing )

                other ->
                    ( other, Cmd.none, Nothing )

        racing =
            phase == Racing && not model.progress.finished

        state =
            Physics.stepBoat
                { dt = dt
                , time = time
                , steer = steerInput model
                , throttle =
                    if racing then
                        1

                    else
                        0
                }
                model.course.wind
                model.boat
                model.course.islands
                model.state

        ( progressAfterBuoys, passed ) =
            if racing then
                Physics.stepProgress dt state model.course.waypoints model.progress

            else
                ( model.progress, Nothing )

        ( flying, splashes ) =
            Physics.stepProjectiles dt time model.cannonballs

        ( afterTargets, hitFlashes, ( targets, hits ) ) =
            Physics.checkTargetHits flying model.targets

        ( afterShips, shipFlashes, _ ) =
            Physics.checkShipHits afterTargets
                (remotePlayers model |> List.map (\p -> { slot = p.slot, position = p.position }))

        countedHits =
            if model.progress.finished then
                0

            else
                hits

        progress =
            { progressAfterBuoys | targetsHit = progressAfterBuoys.targetsHit + countedHits }

        passToast =
            case passed of
                Just i ->
                    if progress.finished then
                        Just ( "FINISH!", 1.5 )

                    else
                        Just ( "Buoy " ++ String.fromInt (i + 1) ++ "/" ++ String.fromInt progress.totalWaypoints, 1.0 )

                Nothing ->
                    Nothing

        hitToast =
            if countedHits > 0 then
                Just ( "Hit! −" ++ String.fromInt (round (Physics.bonusPerHit * toFloat countedHits)) ++ " s", 1.0 )

            else
                Nothing

        toast =
            case List.filterMap identity [ goToast, passToast, hitToast ] of
                t :: _ ->
                    Just t

                [] ->
                    model.toast
                        |> Maybe.andThen
                            (\( s, left ) ->
                                if left - dt > 0 then
                                    Just ( s, left - dt )

                                else
                                    Nothing
                            )

        sfxCmds =
            [ case passed of
                Just _ ->
                    if progress.finished then
                        sfx GameAudio.Finish

                    else
                        sfx GameAudio.Bell

                Nothing ->
                    Cmd.none
            , if not (List.isEmpty hitFlashes && List.isEmpty shipFlashes) then
                sfx (GameAudio.Impact 1.0)

              else if not (List.isEmpty splashes) then
                sfx (GameAudio.Splash 0.5)

              else
                Cmd.none
            , if state.bumped && not model.state.bumped then
                sfx (GameAudio.Splash 0.8)

              else
                Cmd.none
            ]

        next =
            Maybe.map .position (List.head (List.drop progress.currentWaypointIndex model.course.waypoints))
                |> (\n ->
                        if progress.finished then
                            Nothing

                        else
                            n
                   )

        aspect =
            toFloat (max 1 model.viewportWidth) / toFloat (max 1 model.viewportHeight)

        follow =
            CameraRig.stepFollow dt aspect state.position state.heading next model.follow

        audioCmd =
            if model.audioMuted then
                Cmd.none

            else
                Ports.updateAudioState
                    { active = not progress.finished
                    , windGain = 0.15 + 0.35 * state.sailPower
                    , windHz = 300 + 200 * state.sailPower
                    , washGain = clamp 0 0.6 (state.speed / 14)
                    , washHz = 140 + state.speed * 20
                    , hullGain = clamp 0 0.2 (abs state.heel / 40)
                    , toneHz = 0
                    , toneGain = 0
                    }

        telemetryCmd =
            case model.multiplayer of
                Just _ ->
                    let
                        p =
                            Vec3.toRecord state.position
                    in
                    GamePorts.sendMultiplayerTelemetry
                        { pos = [ p.x, p.y, p.z ]
                        , rot = [ state.pitch, state.heading, state.heel ]
                        , speed = state.speed
                        , progress = toFloat progress.currentWaypointIndex / toFloat (max 1 progress.totalWaypoints)
                        , finished = progress.finished
                        }

                Nothing ->
                    Cmd.none
    in
    ( { model
        | state = state
        , progress = progress
        , phase = phase
        , follow = follow
        , simTime = time
        , targets = Physics.stepTargets dt time targets
        , cannonballs = afterShips
        , impacts = Physics.stepImpacts dt (model.impacts ++ splashes ++ hitFlashes ++ shipFlashes)
        , cannonCooldown = max 0 (model.cannonCooldown - dt)
        , toast = toast
      }
    , Cmd.batch (phaseCmd :: audioCmd :: telemetryCmd :: sfxCmds)
    )


subscriptions : SailModel -> Sub SailMsg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "code" Decode.string))
        , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "code" Decode.string))
        , Browser.Events.onVisibilityChange (\_ -> ReleaseControls)
        , GamePorts.tiltSteer TiltSteer
        ]



-- VIEW


slotTint : Int -> Vec3
slotTint slot =
    case slot of
        1 ->
            vec3 0.2 0.45 0.95

        2 ->
            vec3 0.92 0.2 0.18

        3 ->
            vec3 0.98 0.82 0.15

        _ ->
            vec3 0.2 0.75 0.3


{-| Geometry of another player's ship by vehicle id (see `Main`), and whether
it is still on its way.
-}
type alias RemoteShipLookup =
    String -> { mesh : Maybe ShipMesh, loading : Bool }


{-| Another player's boat, in their player colour: the ship they shared (a
built-in boat or their own build), or the default boat until it arrives.
-}
remoteBoat : RemoteShipLookup -> Multiplayer.RemotePlayer -> RenderBoat
remoteBoat remoteShip p =
    let
        rot =
            Vec3.toRecord p.rotation
    in
    { mesh = (remoteShip p.vehicleId).mesh
    , state =
        { position = p.position
        , heading = rot.y
        , speed = p.speed
        , steer = 0
        , sailPower = 1
        , sailAngle = 30
        , heel = rot.z
        , pitch = rot.x
        , bumped = False
        }
    , sailTint = Just (slotTint p.slot)
    }


{-| Draw the race. `meshFor` looks up a boat's brick geometry by boat name
(see `Main`); the player's own boat is looked up as `model.boat.name`.
-}
viewSail :
    SailModel
    ->
        { onReturnToShipyard : SailMsg
        , onRestartCourse : SailMsg
        , meshFor : String -> Maybe ShipMesh
        , remoteShip : RemoteShipLookup
        }
    -> Html SailMsg
viewSail model callbacks =
    case model.multiplayer of
        Just room ->
            case room.role of
                HostCommentator ->
                    viewHostSplitScreen callbacks.remoteShip model room

                JoinedPlayer ->
                    viewRace model callbacks (Just room)

        Nothing ->
            viewRace model callbacks Nothing


viewHostSplitScreen : RemoteShipLookup -> SailModel -> RoomState -> Html SailMsg
viewHostSplitScreen remoteShip model room =
    let
        qW =
            max 160 (model.viewportWidth // 2)

        qH =
            max 120 (model.viewportHeight // 2)

        boats =
            List.map (remoteBoat remoteShip) room.players

        quadrant slot =
            case List.filter (\p -> p.slot == slot) room.players |> List.head of
                Just p ->
                    let
                        cam =
                            CameraRig.followView { yaw = (Vec3.toRecord p.rotation).y, distance = 34, focus = p.position }
                    in
                    div [ Attr.class "relative w-full h-full overflow-hidden border border-slate-800 bg-slate-950" ]
                        [ Render.renderWorld cam
                            { course = model.course
                            , nextIndex = -1
                            , boats = boats
                            , guide = Nothing
                            , targets = model.targets
                            , balls = []
                            , impacts = []
                            , time = model.simTime
                            }
                            qW
                            qH
                        , MultiplayerUI.viewPlayerBadge
                            { slot = slot
                            , name =
                                if (remoteShip p.vehicleId).loading then
                                    p.name ++ " · loading ship…"

                                else
                                    p.name
                            , speedLabel = String.fromInt (round (p.speed * 1.94)) ++ " kn"
                            , finished = p.finished
                            }
                        ]

                Nothing ->
                    MultiplayerUI.viewWaitingSlotCard slot room.roomId "sailor"
    in
    div [ Attr.class "relative w-full h-full overflow-hidden bg-slate-950 select-none font-sans" ]
        [ div [ Attr.class "grid grid-cols-2 grid-rows-2 w-full h-full" ]
            [ quadrant 1, quadrant 2, quadrant 3, quadrant 4 ]
        , MultiplayerUI.viewCommentatorHeader
            { room = room
            , startLabel = "START RACE"
            , onCopyShareLink = CopyShareUrl
            , onStartCountdown = StartMultiplayerCountdown
            }
        , case room.status of
            Multiplayer.Countdown n ->
                MultiplayerUI.viewCountdownOverlay { secondsRemaining = n }

            _ ->
                text ""
        ]


viewRace :
    SailModel
    -> { onReturnToShipyard : SailMsg, onRestartCourse : SailMsg, meshFor : String -> Maybe ShipMesh, remoteShip : RemoteShipLookup }
    -> Maybe RoomState
    -> Html SailMsg
viewRace model callbacks maybeRoom =
    let
        boats =
            { mesh = callbacks.meshFor model.boat.name, state = model.state, sailTint = Nothing }
                :: List.map (remoteBoat callbacks.remoteShip) (remotePlayers model)

        loadingShips =
            remotePlayers model
                |> List.filter (\p -> (callbacks.remoteShip p.vehicleId).loading)

        countdown =
            case model.phase of
                Countdown t ->
                    Just (ceiling t)

                WaitingForRoom ->
                    case maybeRoom |> Maybe.map .status of
                        Just (Multiplayer.Countdown n) ->
                            Just n

                        _ ->
                            Nothing

                Racing ->
                    Nothing
    in
    div [ Attr.class "relative w-full h-full overflow-hidden bg-slate-950 select-none font-sans" ]
        [ div [ Attr.class "absolute inset-0 z-0" ]
            [ Render.renderWorld (CameraRig.followView model.follow)
                { course = model.course
                , nextIndex =
                    if model.progress.finished then
                        model.progress.totalWaypoints

                    else
                        model.progress.currentWaypointIndex
                , boats = boats
                , guide = nextBuoy model |> Maybe.map (\n -> { from = model.state.position, to = n })
                , targets = model.targets
                , balls = model.cannonballs
                , impacts = model.impacts
                , time = model.simTime
                }
                model.viewportWidth
                model.viewportHeight
            ]
        , if model.progress.finished then
            text ""

          else
            HUD.viewHud
                { progress = model.progress
                , course = model.course
                , state = model.state
                , cameraYaw = model.follow.yaw
                , cannonReady = model.cannonCooldown <= 0
                , targetInRange = targetInRange model
                , audioMuted = model.audioMuted
                , tilt = model.tilt
                , countdown = countdown
                , toast = Maybe.map Tuple.first model.toast
                , actions =
                    { onSteer = Steer
                    , onFire = FireCannons
                    , onToggleMute = ToggleMute
                    , onRestart = callbacks.onRestartCourse
                    , onExit = callbacks.onReturnToShipyard
                    }
                }
        , case maybeRoom of
            Just room ->
                div [ Attr.class "absolute bottom-24 left-2 z-20 flex items-center gap-2 px-3 py-1.5 rounded-2xl bg-slate-950/80 border border-white/10 pointer-events-auto" ]
                    [ span [ Attr.class ("px-2 py-0.5 rounded-lg text-xs font-black uppercase " ++ Multiplayer.slotBadgeClass room.mySlot) ]
                        [ text ("P" ++ String.fromInt room.mySlot) ]
                    , span [ Attr.class "text-xs font-mono font-bold text-sky-300" ] [ text room.roomId ]
                    , if List.isEmpty loadingShips then
                        text ""

                      else
                        span [ Attr.class "text-xs font-bold text-amber-300", Attr.attribute "data-testid" "loading-ships" ]
                            [ text
                                ("Loading "
                                    ++ String.join ", " (List.map (\p -> "P" ++ String.fromInt p.slot) loadingShips)
                                    ++ " ship…"
                                )
                            ]
                    , button
                        [ onClick CopyShareUrl
                        , Attr.title "Copy room link"
                        , Attr.class "w-11 h-11 flex items-center justify-center rounded-lg bg-slate-800 text-slate-300"
                        ]
                        [ FeatherIcons.share2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                    ]

            Nothing ->
                text ""
        , if model.progress.finished then
            Results.viewResults
                { course = model.course
                , progress = model.progress
                , boatName = model.boat.name
                , personalBest = model.personalBest
                , actions =
                    { onSailAgain = callbacks.onRestartCourse
                    , onReturnToShipyard = callbacks.onReturnToShipyard
                    }
                }

          else
            text ""
        ]
