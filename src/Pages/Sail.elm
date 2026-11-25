module Pages.Sail exposing
    ( SailModel
    , SailMsg(..)
    , initSail
    , subscriptions
    , update
    , viewSail
    )

{-| The voyage: a 3-2-1 start, then dock at the harbours in order while a
follow camera keeps the boat and the next harbour on screen. Cannons lob a ball
at the nearest floating target in range; each hit takes 2 s off the time.
-}

import Browser.Events
import FeatherIcons
import Game.Audio as GameAudio
import Game.Multiplayer as Multiplayer exposing (Role(..), RoomState, RoomStatus(..))
import Game.Ports as GamePorts
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import I18n exposing (Language)
import Json.Decode as Decode
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Pages.Results as Results
import Ports
import Sails.CameraRig as CameraRig exposing (Follow)
import Sails.Ending as Ending exposing (Ending)
import Sails.HUD as HUD exposing (Side(..))
import Sails.Hull as Hull exposing (Hull)
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.Pirates as Pirates exposing (Pirate)
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
import Sails.Weather as Weather exposing (Weather)
import Sails.Wreck as Wreck


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
    , sailLevel : Int
    , weather : Weather
    , run : Int
    , tilt : { available : Bool, enabled : Bool }
    , toast : Maybe ( String, Float )
    , viewportWidth : Int
    , viewportHeight : Int
    , hull : Hull
    , rockTimer : Float
    , seaStrain : Float
    , debris : List Wreck.Piece
    , ending : Maybe Ending
    , pirates : List Pirate
    , piratesSunk : List Int
    , pirateCooldown : Float
    , language : Language
    }


type SailMsg
    = Tick Float
    | KeyDown String
    | KeyUp String
    | Steer Side Bool
    | TiltSteer Float
    | VisibilityChanged Browser.Events.Visibility
    | FireCannons
    | RaiseSail
    | LowerSail
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
    Language
    -> Boat
    -> Course
    -> Bool
    -> Maybe LeaderboardEntry
    -> Maybe RoomState
    -> { available : Bool, enabled : Bool }
    -> Int
    -> Int
    -> SailModel
initSail language boat course audioMuted pb maybeMulti tilt width height =
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
    , sailLevel = Physics.defaultSailLevel
    , weather = Weather.init (weatherSeed course maybeMulti 0)
    , run = 0
    , tilt = tilt
    , toast = Nothing
    , viewportWidth = width
    , viewportHeight = height
    , hull = Hull.fresh
    , rockTimer = 0
    , seaStrain = 0
    , debris = []
    , ending = Nothing
    , pirates = Pirates.fleet (pirateSeed course maybeMulti 0) course
    , piratesSunk = []
    , pirateCooldown = 0
    , language = language
    }


{-| Seed of the race's pirates (`Sails.Pirates.fleet`): as the weather's,
the room's in a room, else new for each run of the course.
-}
pirateSeed : Course -> Maybe RoomState -> Int -> Int
pirateSeed course room run =
    weatherSeed course room run * 7 + 3


{-| Seed of the race's weather. In a room every player gets the room's
weather; alone, each new run of the course gets new weather.
-}
weatherSeed : Course -> Maybe RoomState -> Int -> Int
weatherSeed course room run =
    case room of
        Just r ->
            Weather.seedFrom (course.id ++ "#" ++ r.roomId)

        Nothing ->
            Weather.seedFrom (course.id ++ "/" ++ String.fromInt run)


releaseControls : SailModel -> SailModel
releaseControls model =
    { model | keyLeft = False, keyRight = False, touchLeft = False, touchRight = False }


silenceAudio : Cmd msg
silenceAudio =
    Ports.updateAudioState
        { active = False
        , windGain = 0
        , windHz = 0
        , washGain = 0
        , washHz = 0
        , hullGain = 0
        , toneHz = 0
        , toneGain = 0
        }


{-| Seconds the ROCKS warning stays up after the boat strikes an island.
-}
rockWarningSeconds : Float
rockWarningSeconds =
    1.5


{-| Rough-sea strain (see `Sails.Physics.roughSeaStrain`) above which the
POUNDING warning shows.
-}
poundingStrain : Float
poundingStrain =
    0.25


{-| Speed under which the boat counts as stopped, 1 km/h in m/s: the HUD
then offers to leave or restart the race, as Brick Racer does.
-}
stoppedSpeed : Float
stoppedSpeed =
    1 / 3.6


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


nextHarbour : SailModel -> Maybe Vec3
nextHarbour model =
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

        pirates =
            afloatPirates model |> List.map (Tuple.second >> .position)
    in
    targets ++ ships ++ pirates


{-| The pirates still afloat, with where they are now.
-}
afloatPirates : SailModel -> List ( Pirate, SailingState )
afloatPirates model =
    model.pirates
        |> List.filter (\p -> not (List.member p.id model.piratesSunk))
        |> List.map (\p -> ( p, onTheWaves model (Pirates.stateAt model.simTime p) ))


{-| A pirate lifted onto the swell at its place, as the targets are.
-}
onTheWaves : SailModel -> SailingState -> SailingState
onTheWaves model ps =
    let
        r =
            Vec3.toRecord ps.position

        sea =
            (Weather.conditions model.course.wind model.weather).sea
    in
    { ps | position = vec3 r.x (Physics.seaElevation sea r.x r.z model.simTime) r.z }


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


{-| Compass bearing back to the course while the boat is off it (see
`Physics.courseFreeRange`), for the HUD's arrow.
-}
backToCourse : SailModel -> Maybe Float
backToCourse model =
    Physics.nearestCoursePoint (Physics.courseRoute model.course) model.state.position
        |> Maybe.andThen
            (\near ->
                if near.distance > Physics.courseFreeRange then
                    let
                        p =
                            Vec3.toRecord model.state.position

                        c =
                            Vec3.toRecord near.point
                    in
                    Just (atan2 (c.x - p.x) (c.z - p.z) * 180 / pi)

                else
                    Nothing
            )


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

        VisibilityChanged Browser.Events.Hidden ->
            -- A hidden tab gets no animation frames, so the last wind and
            -- wash sound would keep playing: silence it (the race is not
            -- paused; the next frame back in view sends the sound again).
            ( releaseControls model, silenceAudio )

        VisibilityChanged Browser.Events.Visible ->
            ( releaseControls model, Cmd.none )

        FireCannons ->
            if model.cannonCooldown <= 0 && model.phase == Racing && not (Hull.wrecked model.hull) then
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

        RaiseSail ->
            ( { model | sailLevel = min (List.length Physics.sailLevels - 1) (model.sailLevel + 1) }, Cmd.none )

        LowerSail ->
            ( { model | sailLevel = max 0 (model.sailLevel - 1) }, Cmd.none )

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

                "KeyW" ->
                    update RaiseSail model

                "KeyS" ->
                    update LowerSail model

                "PageUp" ->
                    update RaiseSail model

                "PageDown" ->
                    update LowerSail model

                "KeyM" ->
                    update ToggleMute model

                "KeyR" ->
                    update RestartCourse model

                "Enter" ->
                    if model.progress.finished || Hull.wrecked model.hull then
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
                    initSail model.language model.boat model.course model.audioMuted model.personalBest model.multiplayer model.tilt model.viewportWidth model.viewportHeight
            in
            ( { fresh
                | multiplayer = model.multiplayer
                , run = model.run + 1
                , weather = Weather.init (weatherSeed model.course model.multiplayer (model.run + 1))
                , pirates = Pirates.fleet (pirateSeed model.course model.multiplayer (model.run + 1)) model.course
                , sailLevel = model.sailLevel
              }
            , GamePorts.calibrateTilt ()
            )

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
                                initSail model.language model.boat model.course model.audioMuted model.personalBest (Just room) model.tilt model.viewportWidth model.viewportHeight
                        in
                        ( { fresh | phase = WaitingForRoom }, GamePorts.calibrateTilt () )

                ( InRace, _ ) ->
                    if model.phase == WaitingForRoom then
                        ( { model | multiplayer = Just room, phase = Racing, toast = Just ( T.goBanner model.language, 0.8 ) }, sfx GameAudio.Horn )

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
                        ( Racing, sfx GameAudio.Horn, Just ( T.goBanner model.language, 0.8 ) )

                    else
                        ( Countdown (t - dt), Cmd.none, Nothing )

                other ->
                    ( other, Cmd.none, Nothing )

        wreckedBefore =
            Hull.wrecked model.hull

        racing =
            phase == Racing && not model.progress.finished && not wreckedBefore

        ( weather, boltStruck ) =
            Weather.step dt model.weather

        conditions =
            Weather.conditions model.course.wind weather

        sailed =
            Physics.stepBoat
                { dt = dt
                , time = time
                , steer = steerInput model
                , throttle =
                    if racing then
                        Physics.harbourThrottle model.course.waypoints model.progress model.state

                    else
                        0
                , sea = conditions.sea
                , sail = Tuple.second (Physics.sailLevel model.sailLevel)
                }
                conditions.wind
                model.boat
                model.course.islands
                model.state

        -- Far off the course a current carries the boat back towards it.
        state =
            if racing then
                Physics.driftWith dt (Physics.courseCurrent (Physics.courseRoute model.course) sailed.position) sailed

            else
                sailed

        rock =
            Physics.rockImpact model.course.islands model.state state

        roughSea =
            Physics.roughSeaStrain conditions.sea time model.boat state

        hull =
            Hull.step dt
                racing
                { rock = rock
                , cannonHits =
                    if boarding then
                        1

                    else
                        0
                , roughSea = roughSea
                }
                model.hull

        justWrecked =
            Hull.wrecked hull && not wreckedBefore

        knocked =
            hull.health < model.hull.health - 3

        debris =
            if justWrecked then
                Wreck.breakUp state

            else
                Wreck.step dt conditions.sea time model.debris

        ( progressAfterHarbours, passed ) =
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

        -- Pirates where they are this frame (`Sails.Pirates`).
        piratesNow =
            afloatPirates { model | simTime = time }

        ( afterPirates, pirateFlashes, pirateSlotsHit ) =
            Physics.checkShipHits afterShips
                (List.map (\( p, ps ) -> { slot = Pirates.shipSlot p.id, position = ps.position }) piratesNow)

        sunkNow =
            List.filter (\( p, _ ) -> List.member (Pirates.shipSlot p.id) pirateSlotsHit) piratesNow

        boarding =
            racing
                && model.pirateCooldown
                <= 0
                && List.any (\( _, ps ) -> horizontal state.position ps.position <= Pirates.boardingRange) piratesNow

        countedHits =
            if model.progress.finished then
                0

            else
                hits + List.length sunkNow

        treasure =
            if racing then
                Physics.reachedTreasure state model.course.treasures progressAfterHarbours.treasureFound

            else
                []

        treasureBonus =
            List.sum (List.map .bonusSeconds treasure)

        progress =
            { progressAfterHarbours
                | targetsHit = progressAfterHarbours.targetsHit + countedHits
                , treasureFound = progressAfterHarbours.treasureFound ++ List.map .id treasure
                , treasureBonus = progressAfterHarbours.treasureBonus + treasureBonus
            }

        treasureToast =
            if List.isEmpty treasure then
                Nothing

            else
                Just ( T.treasureToast model.language (round treasureBonus), 1.6 )

        passToast =
            case passed of
                Just i ->
                    if progress.finished then
                        Just ( T.finishToast model.language, 1.5 )

                    else
                        Just ( T.harbourToast model.language (i + 1) progress.totalWaypoints, 1.0 )

                Nothing ->
                    Nothing

        hitToast =
            if countedHits > 0 then
                Just ( T.hitToast model.language (round (Physics.bonusPerHit * toFloat countedHits)), 1.0 )

            else
                Nothing

        hullToast =
            if justWrecked then
                Just ( T.shipwrecked model.language, 2.0 )

            else if boarding then
                Just ( T.piratesBoardedToast model.language, 1.4 )

            else if not (List.isEmpty sunkNow) then
                Just ( T.pirateSunkToast model.language (round (Physics.bonusPerHit * toFloat (List.length sunkNow))), 1.4 )

            else if knocked then
                Just ( T.hullDamagedToast model.language (Hull.percent hull), 1.2 )

            else
                Nothing

        toast =
            case List.filterMap identity [ hullToast, goToast, passToast, treasureToast, hitToast ] of
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
            , if not (List.isEmpty hitFlashes && List.isEmpty shipFlashes && List.isEmpty pirateFlashes) then
                sfx (GameAudio.Impact 1.0)

              else if not (List.isEmpty splashes) then
                sfx (GameAudio.Splash 0.5)

              else
                Cmd.none
            , if List.isEmpty treasure then
                Cmd.none

              else
                sfx GameAudio.Checkpoint
            , if boltStruck && not model.audioMuted then
                sfx (GameAudio.Impact 0.5)

              else
                Cmd.none
            , if state.bumped && not model.state.bumped then
                sfx (GameAudio.Splash 0.8)

              else
                Cmd.none
            , if justWrecked then
                sfx (GameAudio.Impact 1.0)

              else if knocked then
                sfx (GameAudio.Impact 0.6)

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

        -- A hard knock on rock jolts the camera by how hard it was; the
        -- boat breaking up jolts it fully. Nudges do not.
        shake =
            (if racing then
                Maybe.withDefault 0 (Maybe.map CameraRig.knockShake rock)

             else
                0
            )
                + (if justWrecked then
                    1

                   else
                    0
                  )

        follow =
            CameraRig.stepFollow dt aspect state.position state.heading state.speed next model.follow
                |> CameraRig.jolt shake

        audioCmd =
            if model.audioMuted then
                Cmd.none

            else
                Ports.updateAudioState
                    { active = not progress.finished
                    , windGain = clamp 0 0.9 ((0.15 + 0.35 * state.sailPower) * (0.6 + 0.4 * conditions.wind.speed / model.course.wind.speed) + 0.2 * conditions.rain)
                    , windHz = 300 + 200 * state.sailPower + 120 * conditions.gloom
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

        -- The finish celebration (`Sails.Ending`): it starts when the last
        -- harbour is reached, and bricks burst over the boat as it goes on.
        ending =
            if progress.finished then
                case model.ending of
                    Just e ->
                        Just (Ending.step dt e)

                    Nothing ->
                        Just
                            (Ending.start
                                { targetSeconds = model.course.targetSeconds
                                , personalBest = Maybe.map .timeSeconds model.personalBest
                                }
                                progress
                            )

            else
                Nothing

        celebration =
            case ending of
                Just e ->
                    let
                        -- Bursts already gone off before this step.
                        done =
                            model.ending
                                |> Maybe.map (Ending.bursts Nothing)
                                |> Maybe.withDefault 0
                    in
                    List.range done (done + Ending.bursts model.ending e - 1)
                        |> List.concatMap (\n -> Ending.confetti n state)

                Nothing ->
                    []
    in
    ( { model
        | state = state
        , progress = progress
        , phase = phase
        , follow = follow
        , simTime = time
        , targets = Physics.stepTargets conditions.sea dt time targets
        , weather = weather
        , cannonballs = afterPirates
        , impacts = Physics.stepImpacts dt (model.impacts ++ splashes ++ hitFlashes ++ shipFlashes ++ pirateFlashes)
        , piratesSunk = model.piratesSunk ++ List.map (Tuple.first >> .id) sunkNow
        , pirateCooldown =
            if boarding then
                Pirates.boardingCooldown

            else
                max 0 (model.pirateCooldown - dt)
        , cannonCooldown = max 0 (model.cannonCooldown - dt)
        , toast = toast
        , hull = hull
        , rockTimer =
            if racing && rock /= Nothing then
                rockWarningSeconds

            else
                max 0 (model.rockTimer - dt)
        , seaStrain =
            if racing then
                roughSea

            else
                0
        , debris = debris ++ List.concatMap (Tuple.second >> Wreck.breakUp) sunkNow ++ celebration
        , ending = ending
      }
    , Cmd.batch (phaseCmd :: audioCmd :: telemetryCmd :: sfxCmds)
    )


subscriptions : SailModel -> Sub SailMsg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "code" Decode.string))
        , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "code" Decode.string))
        , Browser.Events.onVisibilityChange VisibilityChanged
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
        , sail = 1
        }
    , sailTint = Just (slotTint p.slot)
    }


{-| A pirate ship as drawn: the pirates' boat with black sails.
-}
pirateBoat : (String -> Maybe ShipMesh) -> ( Pirate, SailingState ) -> RenderBoat
pirateBoat meshFor ( _, state ) =
    { mesh = meshFor Pirates.shipName
    , state = state
    , sailTint = Just (vec3 0.08 0.08 0.1)
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
        , props : Render.Props
        }
    -> Html SailMsg
viewSail model callbacks =
    viewRace model callbacks model.multiplayer


viewRace :
    SailModel
    -> { onReturnToShipyard : SailMsg, onRestartCourse : SailMsg, meshFor : String -> Maybe ShipMesh, remoteShip : RemoteShipLookup, props : Render.Props }
    -> Maybe RoomState
    -> Html SailMsg
viewRace model callbacks maybeRoom =
    let
        wrecked =
            Hull.wrecked model.hull

        -- A wrecked boat is drawn as its loose bricks (`debris`) instead.
        ownBoat =
            if wrecked then
                []

            else
                [ { mesh = callbacks.meshFor model.boat.name, state = model.state, sailTint = Nothing } ]

        boats =
            ownBoat
                ++ List.map (remoteBoat callbacks.remoteShip) (remotePlayers model)
                ++ List.map (pirateBoat callbacks.meshFor) (afloatPirates model)

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

        worldArgs =
            { course = model.course
            , nextIndex =
                if model.progress.finished then
                    model.progress.totalWaypoints

                else
                    model.progress.currentWaypointIndex
            , boats = boats
            , guide = nextHarbour model |> Maybe.map (\n -> { from = model.state.position, to = n })
            , targets = model.targets
            , balls = model.cannonballs
            , impacts = model.impacts
            , time = model.simTime
            , weather = Weather.conditions model.course.wind model.weather
            , scenery = callbacks.meshFor
            , treasureFound = model.progress.treasureFound
            , props = callbacks.props
            , debris = model.debris
            }
    in
    div
        [ Attr.class "is-racing relative w-full h-full overflow-hidden bg-brand select-none font-sans"

        -- A tap anywhere in the race starts tilt steering where the phone
        -- still has to ask for the sensor (iOS), see tilt-steer.js.
        , Attr.attribute "data-tilt-start" ""
        ]
        [ -- Alone or in an online room, the player's own follow view fills
          -- the screen; the other players' boats sail in it.
          div [ Attr.class "absolute inset-0 z-0" ]
            [ Render.renderWorld (CameraRig.followView model.follow) worldArgs model.viewportWidth model.viewportHeight ]
        , if model.progress.finished then
            text ""

          else if wrecked then
            HUD.viewWrecked model.language { onRestart = callbacks.onRestartCourse, onExit = callbacks.onReturnToShipyard }

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
                , sailLevel = model.sailLevel
                , wind = (Weather.conditions model.course.wind model.weather).wind
                , weather = Weather.label model.weather
                , flash = (Weather.conditions model.course.wind model.weather).flash
                , hull = model.hull
                , hazards = { rocks = model.rockTimer > 0, pounding = model.seaStrain > poundingStrain }
                , stopped = abs model.state.speed < stoppedSpeed
                , nextHarbourDistance =
                    nextHarbour model
                        |> Maybe.map (\n -> Vec3.distance (Vec3.setY 0 n) (Vec3.setY 0 model.state.position))
                , courseBearing = backToCourse model
                , language = model.language
                , actions =
                    { onSteer = Steer
                    , onFire = FireCannons
                    , onRaiseSail = RaiseSail
                    , onLowerSail = LowerSail
                    , onToggleMute = ToggleMute
                    , onRestart = callbacks.onRestartCourse
                    , onExit = callbacks.onReturnToShipyard
                    }
                }
        , hostHeader model.language maybeRoom
        , lobbyRoster model.language maybeRoom
        , case maybeRoom of
            Just room ->
                -- In portrait the pill sits above the steering and sail
                -- buttons, which it would cover lower down. Once the race starts
                -- the other players' name tags stack above it.
                div [ Attr.class "absolute bottom-[152px] landscape:bottom-24 left-2 z-20 flex flex-col items-start gap-1.5 pointer-events-none" ]
                    ((if room.status == Lobby then
                        -- The lobby roster names everyone while the room waits.
                        []

                      else
                        List.map (playerBadge model.language) (List.sortBy .slot (remotePlayers model))
                     )
                        ++ [ roomPill model.language loadingShips room ]
                    )

            Nothing ->
                text ""
        , case ( model.progress.finished, model.ending ) of
            ( True, Just ending ) ->
                Results.viewResults
                    { course = model.course
                    , progress = model.progress
                    , boatName = model.boat.name
                    , personalBest = model.personalBest
                    , ending = ending
                    , language = model.language
                    , actions =
                        { onSailAgain = callbacks.onRestartCourse
                        , onBackToMenu = callbacks.onReturnToShipyard
                        }
                    }

            _ ->
                text ""
        ]


{-| The room pill: this player's slot, the room code, the ships still
loading and the copy-link button.
-}
roomPill : Language -> List Multiplayer.RemotePlayer -> RoomState -> Html SailMsg
roomPill language loadingShips room =
    div [ Attr.class "flex items-center gap-2 px-3 py-1.5 rounded-2xl bg-brand/80 border border-white/10 pointer-events-auto" ]
        [ span [ Attr.class ("px-2 py-0.5 rounded-lg text-xs font-black uppercase " ++ Multiplayer.slotBadgeClass room.mySlot) ]
            [ text ("P" ++ String.fromInt room.mySlot) ]
        , span [ Attr.class "text-xs font-mono font-bold text-sky-300" ] [ text room.roomId ]
        , if List.isEmpty loadingShips then
            text ""

          else
            span [ Attr.class "text-xs font-bold text-brand-yellow", Attr.attribute "data-testid" "loading-ships" ]
                [ text (T.loadingShips language (List.map .slot loadingShips)) ]
        , button
            [ onClick CopyShareUrl
            , Attr.title (T.copyRoomLink language)
            , Attr.attribute "aria-label" (T.copyRoomLink language)
            , Attr.class "w-11 h-11 flex items-center justify-center rounded-lg bg-slate-800 text-slate-300"
            ]
            [ FeatherIcons.share2 |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
        ]


{-| Commentator controls (room code, player count, copy link, start) for the
host of a room. In portrait it sits below the HUD's status panel and the
countdown, which share the top of a narrow screen.
-}
hostHeader : Language -> Maybe RoomState -> Html SailMsg
hostHeader language maybeRoom =
    case maybeRoom of
        Just room ->
            case room.role of
                HostCommentator ->
                    div
                        [ Attr.class "absolute top-[200px] landscape:top-3 inset-x-0 mx-auto w-fit max-w-[calc(100%-16px)] z-30 flex flex-wrap justify-center items-center gap-2 landscape:gap-3 px-3 landscape:px-4 py-2 rounded-2xl bg-brand/90 backdrop-blur-md border border-white/10 shadow-2xl pointer-events-auto"
                        , Attr.attribute "data-testid" "host-header"
                        ]
                        [ span [ Attr.class "font-mono font-extrabold text-sky-400 text-xs px-2.5 py-1 rounded-lg bg-sky-500/10 border border-sky-500/30 whitespace-nowrap" ]
                            [ text (T.roomLabel language room.roomId) ]
                        , span [ Attr.class "text-xs font-bold text-slate-300 tabular-nums whitespace-nowrap" ]
                            [ text (T.playerCount language (List.length room.players) room.maxPlayers) ]
                        , button
                            [ Attr.type_ "button"
                            , onClick CopyShareUrl
                            , Attr.class "text-xs font-bold px-2.5 py-1 rounded-xl bg-slate-800 hover:bg-slate-700 active:scale-95 text-slate-200 whitespace-nowrap border border-white/10"
                            ]
                            [ text (T.copyLink language) ]
                        , case room.status of
                            Lobby ->
                                button
                                    [ Attr.type_ "button"
                                    , onClick StartMultiplayerCountdown
                                    , Attr.class "text-xs font-black px-3.5 py-1 rounded-xl bg-gradient-to-r from-emerald-500 to-teal-500 hover:brightness-110 active:scale-95 text-slate-950 shadow-lg shadow-emerald-500/25 whitespace-nowrap"
                                    ]
                                    [ text (T.startRace language) ]

                            Multiplayer.Countdown n ->
                                statusChip "text-amber-400 bg-amber-500/10 border-amber-500/20"
                                    (T.raceStatus language { countdown = Just n, finished = False })

                            InRace ->
                                statusChip "text-emerald-400 bg-emerald-500/10 border-emerald-500/20"
                                    (T.raceStatus language { countdown = Nothing, finished = False })

                            Finished ->
                                statusChip "text-purple-400 bg-purple-500/10 border-purple-500/20"
                                    (T.raceStatus language { countdown = Nothing, finished = True })
                        ]

                JoinedPlayer ->
                    text ""

        Nothing ->
            text ""


statusChip : String -> String -> Html msg
statusChip colours label =
    span [ Attr.class ("text-xs font-extrabold uppercase tracking-wider px-2.5 py-1 rounded-lg border whitespace-nowrap " ++ colours) ]
        [ text label ]


{-| Who is in the room, while it waits in its lobby: every seat, with the
name of the player in it or an open seat. It sits above the start hint, and
in landscape between the steering and the sail buttons; in portrait above
the room pill too.
-}
lobbyRoster : Language -> Maybe RoomState -> Html SailMsg
lobbyRoster language maybeRoom =
    case maybeRoom of
        Just room ->
            case room.status of
                Lobby ->
                    div
                        [ Attr.class "absolute bottom-[296px] landscape:bottom-[84px] inset-x-0 z-30 px-3 landscape:px-[180px] pointer-events-none"
                        , Attr.attribute "data-testid" "lobby-roster"
                        ]
                        [ div [ Attr.class "flex flex-wrap items-center justify-center gap-1.5" ]
                            (List.map (rosterSeat language room) (List.range 1 room.maxPlayers))
                        ]

                _ ->
                    text ""

        Nothing ->
            text ""


rosterSeat : Language -> RoomState -> Int -> Html msg
rosterSeat language room slot =
    case List.filter (\p -> p.slot == slot) room.players |> List.head of
        Just p ->
            span
                [ Attr.class ("px-2 py-0.5 rounded-lg text-xs font-bold border truncate max-w-[9rem] bg-slate-950/70 " ++ Multiplayer.slotBadgeClass slot)
                , Attr.title (T.slotLabel language slot)
                ]
                [ text
                    (if slot == room.mySlot then
                        p.name ++ " " ++ T.you language

                     else
                        p.name
                    )
                ]

        Nothing ->
            span
                [ Attr.class "px-2 py-0.5 rounded-lg text-xs font-semibold border border-dashed border-slate-700 bg-slate-950/60 text-slate-400"
                , Attr.title (T.slotLabel language slot)
                ]
                [ text (T.slotOpen language slot) ]


{-| Name tag of another player in the room: slot, name, speed and whether
they finished.
-}
playerBadge : Language -> Multiplayer.RemotePlayer -> Html msg
playerBadge language p =
    div
        [ Attr.class "flex items-center gap-2 px-2.5 py-1 rounded-xl bg-slate-900/80 backdrop-blur-md border border-white/10 shadow-lg"
        , Attr.attribute "data-testid" "player-badge"
        ]
        [ span [ Attr.class ("px-1.5 py-0.5 rounded text-[10px] font-black uppercase " ++ Multiplayer.slotBadgeClass p.slot) ]
            [ text ("P" ++ String.fromInt p.slot) ]
        , span [ Attr.class "text-xs font-bold text-white truncate max-w-[110px]" ] [ text p.name ]
        , span [ Attr.class "text-xs font-mono font-bold text-sky-400" ] [ text (String.fromInt (round (p.speed * 1.94)) ++ " kn") ]
        , if p.finished then
            span [ Attr.class "text-xs font-black text-emerald-400" ] [ text (T.finishedBadge language) ]

          else
            text ""
        ]
