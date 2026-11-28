module Pages.Sail exposing
    ( SailModel
    , SailMsg(..)
    , buildHudGoals
    , initSail
    , subscriptions
    , update
    , viewSail
    , waitingForRoom
    )

{-| The voyage: a 3-2-1 start, then dock at the harbours in order while a
follow camera keeps the boat and the next harbour on screen. Cannons lob a ball
at the nearest ship in range (a pirate, the ghost ship or a rival); sinking a
pirate wins coins (`Sails.Coins`).

In an online room the players are one crew (`Sails.Crew`): the host's start
puts every ship, the host's too, back on the start line, cannons never aim at
or hit a crewmate, and the voyage is done when the whole crew is in port. The
crew clock (`crewClock`) runs from the room's start; each ship tells the crew
the clock it moored at (`moorings`, by slot), and the crew's shared time
(`crewTime`) is the latest of them once the whole crew is in port.

-}

import Browser.Events
import Dict exposing (Dict)
import FeatherIcons
import Game.Multiplayer as Multiplayer exposing (Role(..), RoomState, RoomStatus(..))
import Game.Ports as GamePorts
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, preventDefaultOn)
import Html.Keyed as Keyed
import I18n exposing (Language)
import Json.Decode as Decode
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Pages.Results as Results
import Ports
import Sails.Archipelago as Archipelago
import Sails.CameraRig as CameraRig exposing (Follow)
import Sails.CannonPick as CannonPick
import Sails.Coins as Coins exposing (FlyingCoin)
import Sails.Crew as Crew exposing (Crew)
import Sails.DamageMarks as DamageMarks
import Sails.Ending as Ending exposing (Ending)
import Sails.FrameGuard as FrameGuard exposing (FrameGuard)
import Sails.GhostShip as GhostShip exposing (GhostShip)
import Sails.HUD as HUD exposing (Side(..))
import Sails.Harbours as Harbours
import Sails.Hull as Hull exposing (Hull)
import Sails.I18n as T
import Sails.IslandMesh as IslandMesh
import Sails.Physics as Physics
import Sails.Pirates as Pirates exposing (Pirate)
import Sails.Render as Render exposing (RenderBoat)
import Sails.Scenery as Scenery
import Sails.ShipMesh exposing (Muzzle, ShipMesh)
import Sails.Sound as Sound exposing (Sound)
import Sails.Structures as Structures
import Sails.TiltPitch as TiltPitch
import Sails.Types
    exposing
        ( Boat
        , Cannonball
        , CombatImpact
        , Course
        , CourseProgress
        , Faction(..)
        , LeaderboardEntry
        , SailingState
        , StandardState(..)
        , standardForCarrier
        )
import Sails.Weather as Weather exposing (Weather)
import Sails.World as World
import Sails.Wreck as Wreck


type Phase
    = Racing
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
    , tiltPitch : TiltPitch.Pitch
    , toast : Maybe ( String, Float )
    , viewportWidth : Int
    , viewportHeight : Int
    , hull : Hull
    , rockTimer : Float
    , seaStrain : Float
    , debris : List Wreck.Piece
    , flyingCoins : List FlyingCoin
    , ending : Maybe Ending
    , pirates : List Pirate
    , piratesSunk : List Int
    , pirateHits : Dict Int Int
    , pirateCooldown : Float
    , pirateGuns : Dict Int Float
    , touchShoot : Bool
    , language : Language
    , crewClock : Float
    , moorings : Dict Int Float
    , crewTime : Maybe Float
    , playerFaction : Faction
    , standards : List StandardState
    , ghostShip : GhostShip
    , anchored : Bool
    , showHelp : Bool
    , combatTurnTarget : Maybe Float
    , -- Seconds since the boat last fired, 0 on every shot, running while
      -- racing; the HUD's goal hints hide under 3 s (`HUD.hintVisible`).
      sinceShot : Float
    , islandMeshes : IslandMesh.CourseMesh
    , cannons : List Muzzle
    , -- Cannon ball hits each harbour's pier has taken (`Sails.Harbours`);
      -- a smashed pier stays smashed until the course is sailed again.
      harbourDamage : Harbours.Damage
    , -- The buildings and town piers that can be shot down, and what has
      -- befallen them (`Sails.Structures`); fresh when the course is sailed
      -- again.
      structures : Structures.State
    , -- Durability bars over the structures just hit (`Sails.DamageMarks`).
      damageMarks : DamageMarks.Pool
    , -- The canvas resolution guard (`Sails.FrameGuard`): drawing only, it
      -- lowers the 3D canvas resolution while frames stay slow.
      frameGuard : FrameGuard
    , -- The 3D canvas's WebGL context (`GraphicsLost`, `GraphicsRestored`):
      -- while `lost`, the voyage waits and the HUD says the graphics are
      -- being restored; `generation` keys the canvas, so a restored context
      -- gets a fresh canvas that uploads every mesh again.
      graphics : { lost : Bool, generation : Int }
    }


type SailMsg
    = Tick Float
    | KeyDown String
    | KeyUp String
    | Steer Side Bool
    | TiltSteer Float
    | TiltOrientation { available : Bool, gamma : Float, beta : Float }
    | VisibilityChanged Browser.Events.Visibility
    | FireCannons
    | PointerFire { x : Float, y : Float, pointerType : String }
    | RaiseSail
    | LowerSail
    | ToggleMute
    | ToggleHelp
    | ReturnToShipyard
    | RestartCourse
    | NewMap
    | CopyShareUrl
    | StartMultiplayerCountdown
    | UpdateMultiplayerRoom RoomState
    | DropAnchor
    | GraphicsLost
    | GraphicsRestored


{-| Radius (m) the boat bumps off another player's ship or the ghost ship
at (plus its own, `Physics.boatRadius`); pirates have one per kind
(`Pirates.collisionRadius`).
-}
shipBumpRadius : Float
shipBumpRadius =
    2.3


{-| How close (m) a cannon ball must come to another player's ship or the
ghost ship to hit it; pirates have one per kind (`Pirates.hitRadius`).
-}
shipHitRadius : Float
shipHitRadius =
    2.6


cannonReload : Float
cannonReload =
    0.8


{-| How far (m) from the boat, measured across the water
(`Physics.horizontalDistance`), a battle the boat is not part of is still
heard: a pirate volley, or a ball from a pirate or another player striking
a building, toppling it or splashing down. Further off it is silent, so
fights far off-screen do not drum on. The boat's own shots are always
heard. A pirate fires at the boat only within `Pirates.reach`, well inside
this.
-}
combatHearingRange : Float
combatHearingRange =
    60


{-| Whether `position` is within `combatHearingRange` of the boat at `boat`.
-}
withinEarshot : Vec3 -> Vec3 -> Bool
withinEarshot boat position =
    Physics.horizontalDistance boat position <= combatHearingRange


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
initSail =
    initSailWith Nothing


initSailWith :
    Maybe IslandMesh.CourseMesh
    -> Language
    -> Boat
    -> Course
    -> Bool
    -> Maybe LeaderboardEntry
    -> Maybe RoomState
    -> { available : Bool, enabled : Bool }
    -> Int
    -> Int
    -> SailModel
initSailWith cachedIslands language boat baseCourse audioMuted pb maybeMulti tilt width height =
    let
        course =
            routeCourse maybeMulti baseCourse

        islandMeshes =
            Maybe.withDefault (IslandMesh.forCourse course) cachedIslands

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
            Racing
    , follow = CameraRig.initFollow state.position state.heading
    , simTime = 0
    , audioMuted = audioMuted
    , personalBest = pb
    , multiplayer = maybeMulti
    , cannonballs = []
    , impacts = []
    , cannonCooldown = 0
    , nextCannonId = 1
    , keyLeft = False
    , keyRight = False
    , touchLeft = False
    , touchRight = False
    , tiltSteer = 0
    , tiltPitch = TiltPitch.init
    , sailLevel = Physics.defaultSailLevel
    , weather = Weather.init (World.weatherProfile course.map) (weatherSeed course maybeMulti 0)
    , run = 0
    , tilt = tilt
    , toast = Nothing
    , viewportWidth = width
    , viewportHeight = height
    , hull = Hull.fresh
    , rockTimer = 0
    , seaStrain = 0
    , debris = []
    , flyingCoins = []
    , ending = Nothing
    , pirates = Pirates.fleet (pirateSeed course maybeMulti 0) course
    , piratesSunk = []
    , pirateHits = Dict.empty
    , pirateCooldown = 0
    , pirateGuns = Dict.empty
    , touchShoot = False
    , language = language
    , crewClock = 0
    , moorings = Dict.empty
    , crewTime = Nothing
    , playerFaction =
        maybeMulti
            |> Maybe.map (\r -> Physics.factionForSlot r.mySlot)
            |> Maybe.withDefault Pirates
    , standards = Physics.initStandards
    , ghostShip = GhostShip.init course
    , anchored = False
    , showHelp = False
    , combatTurnTarget = Nothing
    , sinceShot = HUD.hintReturnSeconds
    , islandMeshes = islandMeshes
    , cannons = []
    , harbourDamage = Dict.empty
    , structures = Structures.init course islandMeshes
    , damageMarks = DamageMarks.empty
    , frameGuard = FrameGuard.init
    , graphics = { lost = False, generation = 0 }
    }


{-| Whether the race is held on the start line until the room's host
starts it.
-}
waitingForRoom : SailModel -> Bool
waitingForRoom model =
    model.phase == WaitingForRoom


{-| The course with this player's harbour order (`Archipelago.courseFor`
on the course's map): in a room, the order of the player's own seat, so
each boat calls at the same harbours in its own order; alone, always the
first order, so personal bests stay comparable.

The other boats in a room report their progress as the share of their
harbours docked at, which means the same whatever their order.

-}
routeCourse : Maybe RoomState -> Course -> Course
routeCourse room course =
    Archipelago.courseFor (Archipelago.generate course.map) (room |> Maybe.map (\r -> r.mySlot - 1) |> Maybe.withDefault 0)


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
    { model | keyLeft = False, keyRight = False, touchLeft = False, touchRight = False, combatTurnTarget = Nothing }


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


sfx : Sound -> Cmd msg
sfx sound =
    Ports.playSfx (Sound.encode sound)


mySlot : SailModel -> Int
mySlot model =
    model.multiplayer |> Maybe.map .mySlot |> Maybe.withDefault 1


steerInput : SailModel -> Float
steerInput model =
    let
        touch =
            if model.touchRight && not model.touchLeft then
                1

            else if model.touchLeft && not model.touchRight then
                -1

            else
                0

        keys =
            if model.keyRight && not model.keyLeft then
                1

            else if model.keyLeft && not model.keyRight then
                -1

            else
                0
    in
    if touch /= 0 then
        touch

    else if keys /= 0 then
        keys

    else
        case model.combatTurnTarget of
            Just targetH ->
                let
                    rawErr =
                        targetH - model.state.heading

                    err =
                        rawErr - 360.0 * toFloat (floor ((rawErr + 180.0) / 360.0))
                in
                if abs err < 3.0 then
                    0

                else
                    clamp -1.0 1.0 (-err * 0.05)

            Nothing ->
                if model.tilt.enabled then
                    clamp -1 1 model.tiltSteer

                else
                    0


nextHarbour : SailModel -> Maybe Vec3
nextHarbour model =
    if model.progress.finished then
        Nothing

    else
        List.drop model.progress.currentWaypointIndex model.course.waypoints
            |> List.head
            |> Maybe.map .position


{-| What the cannons aim at: the pirate ships still afloat, the ghost ship
when it can be fought and the other players' ships. Never a crewmate's ship.
-}
aimPoints : SailModel -> List Vec3
aimPoints model =
    let
        pirates =
            afloatPirates model |> List.map (Tuple.second >> .position)

        remotes =
            remotePlayers model |> List.map .position

        ghost =
            if GhostShip.canTarget model.ghostShip then
                [ model.ghostShip.state.position ]

            else
                []
    in
    pirates ++ remotes ++ ghost


{-| What the cannons choose among (`Sails.Structures.cannonTargets`): the
foes of `aimPoints` while one is in range, else those and the structures in
range: the buildings and town piers still standing and the harbours' piers,
but never the harbour the boat is to dock at next.
-}
cannonTargets : SailModel -> List Vec3
cannonTargets model =
    Structures.cannonTargets model.state.position
        (aimPoints model)
        (Structures.pierAims (dockingAt model) (Harbours.intactFootprints model.harbourDamage model.course)
            ++ List.map Structures.aimPoint (Structures.standing model.structures)
        )


{-| The harbour the boat is to dock at next, if the voyage is not over.
-}
dockingAt : SailModel -> Maybe Int
dockingAt model =
    if model.progress.finished then
        Nothing

    else
        Just model.progress.currentWaypointIndex


{-| The pirates still afloat, with where they are now.
-}
afloatPirates : SailModel -> List ( Pirate, SailingState )
afloatPirates model =
    model.pirates
        |> List.filter (\p -> not (List.member p.id model.piratesSunk))
        |> List.map (\p -> ( p, onTheWaves model (Pirates.stateAt model.simTime p) ))


{-| A pirate lifted onto the swell at its place.
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


{-| The crew of the room this player sails in, while the room is on its
voyage (see `Sails.Crew`).
-}
crewOf : SailModel -> Maybe Crew
crewOf model =
    Crew.ofRoom model.multiplayer model.progress.finished


{-| The room's voyage is under way (or done): the crew clock runs.
-}
roomVoyage : SailModel -> Bool
roomVoyage model =
    case model.multiplayer |> Maybe.map .status of
        Just InRace ->
            True

        Just Finished ->
            True

        _ ->
            False


remotePlayers : SailModel -> List Multiplayer.RemotePlayer
remotePlayers model =
    case model.multiplayer of
        Just r ->
            List.filter (\p -> p.slot /= r.mySlot) r.players

        Nothing ->
            []


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


{-| Floating goal indicators around the boat showing bearings to harbour,
flags at sea and pirates.
-}
buildHudGoals : SailModel -> List HUD.HudGoal
buildHudGoals model =
    let
        shipPos =
            model.state.position

        shipRec =
            Vec3.toRecord shipPos

        camView =
            CameraRig.followView model.follow

        camEye =
            Vec3.toRecord camView.eye

        camTarget =
            Vec3.toRecord camView.target

        -- True camera line-of-sight bearing in world horizontal space (accounts for camera lead, broadside bias, and yaw lag)
        camBearing =
            atan2 (camTarget.x - camEye.x) (camTarget.z - camEye.z) * 180.0 / pi

        -- Nothing for a goal right on the ship (no bearing to point along).
        calcGoal kind targetPos =
            let
                tRec =
                    Vec3.toRecord targetPos
            in
            HUD.goalBearing shipRec tRec
                |> Maybe.map
                    (\worldBearing ->
                        { kind = kind
                        , angleDeg = HUD.goalScreenAngle camBearing worldBearing
                        , distanceM = Physics.horizontalDistance shipPos targetPos
                        }
                    )

        -- 1. Next harbour waypoint (harbours are sailed in order, so the
        -- current one is the only harbour that counts)
        harbourGoal =
            nextHarbour model
                |> Maybe.andThen (calcGoal HUD.HarbourGoal)

        -- 2. Nearest lost standard at sea (within 350m)
        standardGoal =
            model.standards
                |> List.filterMap
                    (\s ->
                        case s of
                            AtSea _ spos _ ->
                                Just ( Physics.horizontalDistance shipPos spos, spos )

                            _ ->
                                Nothing
                    )
                |> List.filter (\( d, _ ) -> d < 350)
                |> List.sortBy Tuple.first
                |> List.head
                |> Maybe.andThen (\( _, spos ) -> calcGoal HUD.StandardGoal spos)

        -- 3. Nearest pirate ship afloat (within 250m)
        pirateGoal =
            afloatPirates model
                |> List.map (\( _, pstate ) -> ( Physics.horizontalDistance shipPos pstate.position, pstate.position ))
                |> List.filter (\( d, _ ) -> d < 250 && d > 6)
                |> List.sortBy Tuple.first
                |> List.head
                |> Maybe.andThen (\( _, ppos ) -> calcGoal HUD.PirateGoal ppos)
    in
    -- Closest first; HUD.shownGoals drops those the boat is on.
    HUD.shownGoals (List.filterMap identity [ harbourGoal, standardGoal, pirateGoal ])


update : SailMsg -> SailModel -> ( SailModel, Cmd SailMsg )
update msg model =
    case msg of
        Tick dtMs ->
            if model.graphics.lost then
                -- Nothing is drawn while the WebGL context is lost: the
                -- voyage waits for it to come back.
                ( model, Cmd.none )

            else
                tick (clamp 0.001 0.05 (dtMs / 1000)) { model | frameGuard = FrameGuard.record dtMs model.frameGuard }

        GraphicsLost ->
            ( { model | graphics = { lost = True, generation = model.graphics.generation } } |> releaseControls
            , silenceAudio
            )

        GraphicsRestored ->
            -- The old canvas's cached shaders and buffers died with its
            -- context: a new key makes Elm draw on a fresh canvas instead.
            ( { model | graphics = { lost = False, generation = model.graphics.generation + 1 } }, Cmd.none )

        Steer Left down ->
            ( { model | touchLeft = down }, Cmd.none )

        Steer Right down ->
            ( { model | touchRight = down }, Cmd.none )

        TiltSteer v ->
            ( { model | tiltSteer = clamp -1 1 v }, Cmd.none )

        TiltOrientation orientation ->
            if orientation.available then
                let
                    before =
                        model.tiltPitch

                    after =
                        TiltPitch.receive orientation.beta before

                    moved =
                        { model | tiltPitch = after }
                in
                if not model.tilt.enabled || after.zone == before.zone then
                    ( moved, Cmd.none )

                else
                    case after.zone of
                        TiltPitch.Forward ->
                            update RaiseSail moved

                        TiltPitch.Backward ->
                            update LowerSail moved

                        TiltPitch.Level ->
                            ( moved, Cmd.none )

            else
                ( model, Cmd.none )

        VisibilityChanged Browser.Events.Hidden ->
            -- A hidden tab gets no animation frames, so the last wind and
            -- wash sound would keep playing: silence it (the race is not
            -- paused; the next frame back in view sends the sound again).
            ( releaseControls model, silenceAudio )

        VisibilityChanged Browser.Events.Visible ->
            ( releaseControls model, Cmd.none )

        FireCannons ->
            -- A ship with built cannons fires one of them, chosen and
            -- ranged exactly as a tap does (`fireChosenCannon`). Only a
            -- ship with none fires the old hull broadside.
            if List.isEmpty model.cannons then
                let
                    aimTarget =
                        case Physics.findBroadsideTarget model.state (cannonTargets model) of
                            Just ( _, aim, _ ) ->
                                let
                                    p =
                                        Vec3.toRecord aim

                                    s =
                                        Vec3.toRecord model.state.position
                                in
                                vec3 (p.x - s.x) 0 (p.z - s.z)

                            Nothing ->
                                let
                                    rad =
                                        degrees model.state.heading
                                in
                                vec3 (cos rad) 0 (0 - sin rad)
                in
                fireInAimDirection aimTarget model

            else
                fireChosenCannon (CannonPick.allCannons model.cannons) model

        PointerFire tap ->
            let
                tapped =
                    { model | touchShoot = tap.pointerType == "touch" }
            in
            -- Only a tap on one of the drawn cannons fires, from that
            -- cannon (ahead, astern or out to its side). A tap near several
            -- fires the one nearest the foe. Taps anywhere else do nothing.
            case CannonPick.tapCandidates (CameraRig.followView model.follow) model.viewportWidth model.viewportHeight model.state model.cannons { x = tap.x, y = tap.y } of
                [] ->
                    ( model, Cmd.none )

                candidates ->
                    fireChosenCannon candidates tapped

        RaiseSail ->
            ( { model | sailLevel = min (List.length Physics.sailLevels - 1) (model.sailLevel + 1) }, Cmd.none )

        LowerSail ->
            ( { model | sailLevel = max 0 (model.sailLevel - 1) }, Cmd.none )

        DropAnchor ->
            if model.anchored then
                ( { model
                    | anchored = False
                    , sailLevel = Physics.defaultSailLevel
                    , toast = Just ( T.anchorWeighedToast model.language, 1.5 )
                  }
                , sfx Sound.AnchorWeigh
                )

            else if Physics.canDropAnchor model.course.waypoints model.progress model.state then
                let
                    ( newProgress, passed ) =
                        Physics.dropAnchorProgress model.progress

                    state =
                        model.state

                    stoppedState =
                        { state | speed = 0 }

                    passedToast =
                        case passed of
                            Just i ->
                                if newProgress.finished then
                                    Just ( T.finishToast model.language, 2.5 )

                                else
                                    Just ( T.harbourToast model.language (i + 1) newProgress.totalWaypoints ++ " · " ++ T.coinsGained model.language Coins.harbourDocked, 2.0 )

                            Nothing ->
                                Nothing

                    ending =
                        if newProgress.finished then
                            Just
                                (Ending.start
                                    { coinTarget = Coins.courseTarget model.course
                                    , personalBest = Maybe.map .coins model.personalBest
                                    }
                                    newProgress
                                )

                        else
                            model.ending
                in
                ( { model
                    | progress = newProgress
                    , state = stoppedState
                    , sailLevel = 0
                    , toast = passedToast
                    , ending = ending
                    , anchored = True
                  }
                , Cmd.batch
                    (sfx Sound.AnchorDrop
                        :: (case passed of
                                Just _ ->
                                    [ sfx (Sound.Coins Coins.harbourDocked)
                                    , if newProgress.finished then
                                        sfx Sound.Finish

                                      else
                                        sfx Sound.Bell
                                    ]

                                Nothing ->
                                    []
                           )
                    )
                )

            else
                let
                    state =
                        model.state
                in
                ( { model
                    | anchored = True
                    , sailLevel = 0
                    , state = { state | speed = 0 }
                    , toast = Just ( T.anchorDroppedToast model.language, 1.5 )
                  }
                , sfx Sound.AnchorDrop
                )

        ToggleMute ->
            ( { model | audioMuted = not model.audioMuted }, GamePorts.setAudioMuted (not model.audioMuted) )

        ToggleHelp ->
            ( { model | showHelp = not model.showHelp }, Cmd.none )

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

                "KeyQ" ->
                    update LowerSail model

                "KeyE" ->
                    update RaiseSail model

                "KeyH" ->
                    update ToggleHelp model

                "Slash" ->
                    update ToggleHelp model

                "Space" ->
                    update DropAnchor model

                "KeyB" ->
                    update DropAnchor model

                "KeyF" ->
                    update FireCannons model

                "ArrowUp" ->
                    update RaiseSail model

                "ArrowDown" ->
                    update LowerSail model

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

                    else if Physics.canDropAnchor model.course.waypoints model.progress model.state then
                        update DropAnchor model

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
                    initSailWith (Just model.islandMeshes) model.language model.boat model.course model.audioMuted model.personalBest model.multiplayer model.tilt model.viewportWidth model.viewportHeight
            in
            ( { fresh
                | multiplayer = model.multiplayer
                , run = model.run + 1
                , frameGuard = model.frameGuard
                , graphics = model.graphics
                , weather = Weather.init (World.weatherProfile model.course.map) (weatherSeed model.course model.multiplayer (model.run + 1))
                , pirates = Pirates.fleet (pirateSeed model.course model.multiplayer (model.run + 1)) model.course
                , sailLevel = model.sailLevel
                , tiltPitch = TiltPitch.calibrate model.tiltPitch

                -- Back to the start line alone: the crew's voyage goes on.
                , crewClock = model.crewClock
                , moorings = Dict.remove (mySlot model) model.moorings
                , crewTime = model.crewTime
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
                -- The crewmates' moorings this room state tells of.
                heard =
                    Crew.heardMoorings room model.moorings

                -- Crewmates' ships wrecked: they break up into their own
                -- bricks here too.
                crewWrecks =
                    Crew.heardWrecks room
                        |> List.concatMap
                            (\w ->
                                case List.filter (\p -> p.slot == w.slot) room.players of
                                    player :: _ ->
                                        Wreck.burst (Wreck.RemoteShip player.vehicleId) w.position w.heading 0

                                    [] ->
                                        []
                            )

                withWrecks =
                    { model | debris = model.debris ++ crewWrecks }

                wasCounting =
                    case model.multiplayer |> Maybe.map .status of
                        Just (Multiplayer.Countdown _) ->
                            True

                        _ ->
                            False
            in
            case room.status of
                Multiplayer.Countdown _ ->
                    if wasCounting then
                        ( { withWrecks | multiplayer = Just room, moorings = heard }, Cmd.none )

                    else
                        -- The host started the voyage: the whole crew, the
                        -- host too, back to the start line to sail together.
                        let
                            fresh =
                                initSailWith (Just model.islandMeshes) model.language model.boat model.course model.audioMuted model.personalBest (Just room) model.tilt model.viewportWidth model.viewportHeight
                        in
                        ( { fresh | phase = WaitingForRoom, tiltPitch = TiltPitch.calibrate model.tiltPitch, frameGuard = model.frameGuard, graphics = model.graphics }, GamePorts.calibrateTilt () )

                InRace ->
                    if model.phase == WaitingForRoom then
                        ( { withWrecks | multiplayer = Just room, moorings = heard, phase = Racing, toast = Just ( T.goBanner model.language, 0.8 ) }, sfx Sound.Horn )

                    else
                        ( { withWrecks | multiplayer = Just room, moorings = heard }, Cmd.none )

                _ ->
                    ( { withWrecks | multiplayer = Just room, moorings = heard }, Cmd.none )

        ReturnToShipyard ->
            ( model, Cmd.none )

        NewMap ->
            ( model, Cmd.none )


fireInDirection : Float -> SailModel -> ( SailModel, Cmd SailMsg )
fireInDirection sideSign m =
    let
        rad =
            degrees m.state.heading

        aimDir =
            vec3 (sideSign * cos rad) 0 (sideSign * (0 - sin rad))
    in
    fireInAimDirection aimDir m


fireInAimDirection : Vec3 -> SailModel -> ( SailModel, Cmd SailMsg )
fireInAimDirection aimDir =
    fireShot (\m -> Physics.fireTowardsFrom m.cannons (mySlot m) m.state aimDir (cannonTargets m) m.nextCannonId) aimDir


{-| Fire one of the `candidates` (from a tap, `CannonPick.tapCandidates`,
or the F key, `CannonPick.allCannons`): the one nearest the foe
(`CannonPick.selectCannon` with `CannonPick.nearestFoe`), from its own
muzzle, with the power that fits the foe's distance when the foe is in that
cannon's arc (`Physics.fireCannonAt`). The tap and the F key share this one
path.
-}
fireChosenCannon : List ( Muzzle, Float ) -> SailModel -> ( SailModel, Cmd SailMsg )
fireChosenCannon candidates m =
    let
        foe =
            CannonPick.nearestFoe m.state (cannonTargets m)
    in
    case CannonPick.selectCannon m.state foe candidates of
        Just muzzle ->
            fireCannonShot muzzle foe m

        Nothing ->
            ( m, Cmd.none )


{-| Fire one built cannon at `foe` (see `Physics.fireCannonAt`): along its
own barrel, or at the foe when it is in the cannon's arc. Unlike a broadside
it sets no turn target, so a bow or stern chaser never swings the ship
sideways, and the camera looks along the shot (`CameraRig.setCannonShotFocus`)
while the ball flies, then swings back to the heading.
-}
fireCannonShot : Muzzle -> Maybe Vec3 -> SailModel -> ( SailModel, Cmd SailMsg )
fireCannonShot muzzle foe m =
    fireShotWith True
        (\s -> Physics.fireCannonAt muzzle foe (mySlot s) s.state s.nextCannonId)
        (Vec3.sub (Physics.cannonShotVelocity m.state muzzle foe) (Physics.shipVelocity m.state))
        m


{-| Fire the balls `shoot` makes (see `Physics.fireTowardsFrom`) when the
guns are loaded and the race is on; `aimDir` is the camera's shot bearing
when nothing is fired.
-}
fireShot : (SailModel -> ( List Cannonball, Float, Float )) -> Vec3 -> SailModel -> ( SailModel, Cmd SailMsg )
fireShot =
    fireShotWith False


{-| `fireShot`; with `cannon` True (a single built cannon) the ship is not
turned and the camera follows the shot along its bearing.
-}
fireShotWith : Bool -> (SailModel -> ( List Cannonball, Float, Float )) -> Vec3 -> SailModel -> ( SailModel, Cmd SailMsg )
fireShotWith cannon shoot aimDir m =
    if m.cannonCooldown <= 0 && m.phase == Racing && not (Hull.wrecked m.hull) && not m.progress.finished then
        let
            ( balls, _, targetHeading ) =
                shoot m

            -- World bearing of the shot, in the same convention as heading:
            -- the way the ball actually flies (its horizontal velocity),
            -- falling back to the aim direction if nothing was fired.
            -- The camera rides with the ship, so this is the ball's
            -- velocity relative to the ship (the ship's own velocity,
            -- which an untargeted ball carries, taken off).
            aimRec =
                Vec3.toRecord aimDir

            shotBearing =
                balls
                    |> List.head
                    |> Maybe.map (Physics.shotBearing m.state)
                    |> Maybe.withDefault (atan2 aimRec.x aimRec.z * 180.0 / pi)

            -- A single cannon's shot may go ahead or astern: the camera
            -- turns to look along it, and the ship is not turned.
            newFollow =
                if cannon then
                    CameraRig.setCannonShotFocus shotBearing 1.8 m.follow

                else
                    CameraRig.setShootFocus shotBearing 1.8 m.follow
        in
        ( { m
            | cannonballs = m.cannonballs ++ balls
            , cannonCooldown = cannonReload
            , nextCannonId = m.nextCannonId + List.length balls
            , follow = newFollow
            , combatTurnTarget =
                if cannon then
                    Nothing

                else
                    Just targetHeading
            , sinceShot = 0
          }
        , sfx Sound.CannonFire
        )

    else
        ( m, Cmd.none )


tick : Float -> SailModel -> ( SailModel, Cmd SailMsg )
tick dt model =
    let
        time =
            model.simTime + dt

        phase =
            model.phase

        wreckedBefore =
            Hull.wrecked model.hull

        racing =
            phase == Racing && not model.progress.finished && not wreckedBefore

        ( weather, boltStruck ) =
            Weather.step dt model.weather

        conditions =
            Weather.conditions model.course.wind weather

        sailed =
            if model.anchored then
                let
                    s =
                        model.state

                    pos =
                        Vec3.toRecord s.position
                in
                { s
                    | speed = 0
                    , position = vec3 pos.x (Physics.seaElevation conditions.sea pos.x pos.z time) pos.z
                    , sail = Tuple.second (Physics.sailLevel model.sailLevel)
                }

            else
                Physics.stepBoat
                    { dt = dt
                    , time = time
                    , steer = steerInput model
                    , throttle =
                        if racing then
                            if model.anchored then
                                0

                            else
                                1

                        else
                            0
                    , sea = conditions.sea
                    , sail = Tuple.second (Physics.sailLevel model.sailLevel)
                    }
                    conditions.wind
                    model.boat
                    model.course.islands
                    model.state

        -- Far off the course a current carries the boat back towards it, and
        -- a pirate ship in its way bumps it off (`Physics.bumpShips`).
        drifted =
            if racing && not model.anchored then
                Physics.driftWith dt (Physics.courseCurrent (Physics.courseRoute model.course) sailed.position) sailed

            else
                sailed

        otherShips =
            List.map (\( p, ps ) -> { position = ps.position, radius = Pirates.collisionRadius p.kind }) piratesNow
                ++ (remotePlayers model |> List.map (\rp -> { position = rp.position, radius = shipBumpRadius }))
                ++ (if GhostShip.canTarget model.ghostShip then
                        [ { position = model.ghostShip.state.position, radius = shipBumpRadius } ]

                    else
                        []
                   )

        bumpedShips =
            Physics.bumpShips otherShips drifted
                |> Physics.keepInArena model.course.arena

        -- The harbours' piers are solid (`Sails.Harbours`): slow, the boat
        -- stops against one; at ramming speed it smashes it.
        standingPiers =
            Harbours.intactFootprints model.harbourDamage model.course

        -- The harbour the boat docks at next only stops it, at any speed.
        dockingHarbour =
            if model.progress.finished then
                Nothing

            else
                Just model.progress.currentWaypointIndex

        ( bumpedDrifted, rammedHarbours ) =
            Harbours.collide dockingHarbour standingPiers bumpedShips

        reefImpact =
            Physics.checkReefGrounding (Physics.reefsOf model.course) model.boat model.state bumpedDrifted

        state =
            case reefImpact of
                Just _ ->
                    { bumpedDrifted | bumped = True, speed = bumpedDrifted.speed * 0.6 }

                Nothing ->
                    bumpedDrifted

        rock =
            case Physics.rockImpact model.course.islands model.state state of
                Just s ->
                    Just s

                Nothing ->
                    reefImpact

        roughSea =
            Physics.roughSeaStrain conditions.sea time model.boat state

        hull =
            Hull.step dt
                racing
                { rock = rock
                , cannonHits =
                    (if boarding then
                        1

                     else
                        0
                    )
                        + boatHitCount
                , roughSea = roughSea
                }
                model.hull

        justWrecked =
            Hull.wrecked hull && not wreckedBefore

        knocked =
            hull.health < model.hull.health - 3

        steppedDebris =
            if justWrecked then
                Wreck.breakUp (Wreck.Ship { name = model.boat.name, scale = 1 }) state

            else
                Wreck.step dt conditions.sea time model.debris

        -- Floating debris the boat sails through is fished out and patches
        -- the hull (`Sails.Wreck.salvage`), only while racing, as damage
        -- counts only while racing, and only while the hull is hurt and
        -- afloat (otherwise the pieces float on until they expire).
        ( debrisCollected, repairedHull, debris ) =
            if racing then
                Wreck.salvage state.position hull steppedDebris

            else
                ( 0, hull, steppedDebris )

        ( progressAfterHarbours, passed ) =
            if racing then
                let
                    ( p, passedWp ) =
                        Physics.stepProgress dt state model.course.waypoints model.progress
                in
                if Physics.canDropAnchor model.course.waypoints model.progress state then
                    -- In interactive sailing, docking requires the anchor icon to drop anchor.
                    -- Time in berth accumulates for the mooring indicator, but the waypoint does not advance until anchor is dropped:
                    -- Nor does it earn the harbour's coins before then.
                    ( { p
                        | currentWaypointIndex = model.progress.currentWaypointIndex
                        , splits = model.progress.splits
                        , finished = model.progress.finished
                        , coins = model.progress.coins
                        , berthSeconds = min (Physics.dockSeconds - 0.05) p.berthSeconds
                      }
                    , Nothing
                    )

                else
                    ( p, passedWp )

            else
                ( model.progress, Nothing )

        ( flying, splashes ) =
            Physics.stepProjectiles conditions.sea dt time model.cannonballs

        -- The boat's own balls go for pirates, ghost ship and other players;
        -- balls from others (pirates or other players) can hit this boat.
        ( ownFlying, othersFlying ) =
            List.partition (\b -> b.sourceSlot == mySlot model) flying

        ( afterBoatHits, boatFlashes, boatHitCount ) =
            Physics.checkBoatHits state.position othersFlying

        ( pirateShots, nextShotId, pirateGuns ) =
            if racing then
                pirateVolleys dt state model.structures model.nextCannonId piratesNow model.pirateGuns

            else
                ( [], model.nextCannonId, model.pirateGuns )

        -- Pirates where they are this frame (`Sails.Pirates`). Cannonballs
        -- can hit them, the ghost ship, or other player ships.
        piratesNow =
            afloatPirates { model | simTime = time }

        ghostTargets =
            if GhostShip.canTarget model.ghostShip then
                [ { slot = GhostShip.ghostShipSlot, position = model.ghostShip.state.position, radius = shipHitRadius } ]

            else
                []

        remoteTargets =
            remotePlayers model
                |> List.map (\rp -> { slot = rp.slot, position = rp.position, radius = shipHitRadius })

        shipsToCheck =
            List.map (\( p, ps ) -> { slot = Pirates.shipSlot p.id, position = ps.position, radius = Pirates.hitRadius p.kind }) piratesNow
                ++ ghostTargets
                ++ remoteTargets

        ( afterShipHits, pirateFlashes, pirateSlotsHit ) =
            Physics.checkShipHits ownFlying shipsToCheck

        -- The boat's own balls hit the piers still standing, too: after
        -- `Harbours.hitsToDestroy` hits a pier is smashed.
        ( afterPirates, harbourFlashes, harbourHits ) =
            Harbours.checkHits (List.filter (\( i, _ ) -> not (List.member i rammedHarbours)) standingPiers) afterShipHits

        ( harbourDamage, shotDownHarbours ) =
            Harbours.recordHits harbourHits (List.foldl Harbours.wreck model.harbourDamage rammedHarbours)

        pierDebris =
            List.concatMap
                (\( i, fp ) ->
                    if List.member i rammedHarbours then
                        Harbours.ram fp bumpedShips.heading bumpedShips.speed

                    else if List.member i shotDownHarbours then
                        Harbours.smash fp 0 0

                    else
                        []
                )
                standingPiers

        smashedHarbours =
            rammedHarbours ++ shotDownHarbours

        -- Balls hitting the buildings and town piers still standing
        -- (`Sails.Structures`): the boat's own balls hit both, the patrol
        -- ships' balls only buildings. At its durability one falls.
        standingStructures =
            Structures.standing model.structures

        ( afterStructures, structureFlashes, ownStructureHits ) =
            Structures.checkHits standingStructures afterPirates

        ( othersAfterStructures, otherStructureFlashes, otherStructureHits ) =
            Structures.checkHits (List.filter (.kind >> Structures.isBuilding) standingStructures) afterBoatHits

        ( structures, fallen ) =
            Structures.recordHits (World.islandPalette model.course.map) model.islandMeshes (ownStructureHits ++ otherStructureHits) model.structures

        townPierDebris =
            fallen.structures
                |> List.filter (\s -> s.kind == Structures.TownPier)
                |> List.concatMap (\s -> Wreck.burst Wreck.HarbourPier (vec3 s.x 0 s.z) 0 0)

        -- A bar over each structure hit shows the hits it has left.
        damageMarks =
            List.foldl
                (\( i, fp ) pool ->
                    if List.member i harbourHits then
                        DamageMarks.hit ("harbour:" ++ String.fromInt i) (vec3 fp.x 4.2 fp.z) (Harbours.hitsLeft harbourDamage i) Harbours.hitsToDestroy pool

                    else
                        pool
                )
                (DamageMarks.step dt model.damageMarks)
                standingPiers
                |> (\pool ->
                        List.foldl
                            (\s acc ->
                                if List.member s.id (ownStructureHits ++ otherStructureHits) then
                                    DamageMarks.hit ("structure:" ++ String.fromInt s.id) (Structures.topPoint s) (Structures.hitsLeft structures s) (Structures.durability s.kind) acc

                                else
                                    acc
                            )
                            pool
                            standingStructures
                   )

        -- Each cannon hit adds to a pirate ship's count; it sinks when the
        -- count reaches `Pirates.hitsToSink`.
        pirateHitsNow =
            List.foldl
                (\slot acc -> Dict.update slot (\m -> Just (Maybe.withDefault 0 m + 1)) acc)
                model.pirateHits
                pirateSlotsHit

        sunkNow =
            List.filter
                (\( p, _ ) ->
                    let
                        slot =
                            Pirates.shipSlot p.id
                    in
                    List.member slot pirateSlotsHit
                        && (Dict.get slot pirateHitsNow |> Maybe.withDefault 0)
                        >= Pirates.hitsToSink
                )
                piratesNow

        ( updatedGhostShip, ghostAppeared, ghostBanished ) =
            GhostShip.step time model.weather pirateSlotsHit model.ghostShip

        agedStandards =
            Physics.stepStandards dt model.standards

        droppedOnWreckStandards =
            if justWrecked then
                case model.state.carriedStandard of
                    Just f ->
                        List.map
                            (\s ->
                                if s == CarriedBy (mySlot model) f then
                                    AtSea f model.state.position 0.0

                                else
                                    s
                            )
                            agedStandards

                    Nothing ->
                        agedStandards

            else
                agedStandards

        droppedPirateStandards =
            List.foldl
                (\( p, ps ) stds ->
                    let
                        slot =
                            Pirates.shipSlot p.id
                    in
                    List.map
                        (\s ->
                            case s of
                                CarriedBy sSlot f ->
                                    if sSlot == slot then
                                        AtSea f ps.position 0.0

                                    else
                                        s

                                _ ->
                                    s
                        )
                        stds
                )
                droppedOnWreckStandards
                sunkNow

        regalia =
            if justWrecked then
                { standards = droppedPirateStandards
                , carriedStandard = Nothing
                , toast = Maybe.map (\f -> ( T.standardDroppedToast f model.language, 1.8 )) model.state.carriedStandard
                , repatriated = False
                }

            else if racing then
                case model.state.carriedStandard of
                    Nothing ->
                        let
                            recoverableAtSea =
                                droppedPirateStandards
                                    |> List.filter (Physics.canRecoverStandardAtSea state.position)
                                    |> List.head
                        in
                        case recoverableAtSea of
                            Just (AtSea f _ _) ->
                                let
                                    slot =
                                        mySlot model

                                    updatedStandards =
                                        List.map
                                            (\s ->
                                                case s of
                                                    AtSea sf _ _ ->
                                                        if sf == f then
                                                            CarriedBy slot f

                                                        else
                                                            s

                                                    _ ->
                                                        s
                                            )
                                            droppedPirateStandards
                                in
                                { standards = updatedStandards
                                , carriedStandard = Just f
                                , toast = Just ( T.standardSeizedToast f model.language, 2.0 )
                                , repatriated = False
                                }

                            _ ->
                                let
                                    enemyFactions =
                                        [ Pirates, BlueCoats, RedCoats ]
                                            |> List.filter (\f -> f /= model.playerFaction)

                                    seizableFaction =
                                        enemyFactions
                                            |> List.filter
                                                (\f ->
                                                    List.any (Physics.canSeizeStandard state.position state.speed model.course f) droppedPirateStandards
                                                )
                                            |> List.head
                                in
                                case seizableFaction of
                                    Just enemyFaction ->
                                        let
                                            slot =
                                                mySlot model

                                            updatedStandards =
                                                List.map
                                                    (\s ->
                                                        if s == AtCitadel enemyFaction then
                                                            CarriedBy slot enemyFaction

                                                        else
                                                            s
                                                    )
                                                    droppedPirateStandards
                                        in
                                        { standards = updatedStandards
                                        , carriedStandard = Just enemyFaction
                                        , toast = Just ( T.standardSeizedToast enemyFaction model.language, 2.0 )
                                        , repatriated = False
                                        }

                                    Nothing ->
                                        { standards = droppedPirateStandards
                                        , carriedStandard = Nothing
                                        , toast = Nothing
                                        , repatriated = False
                                        }

                    Just carriedEnemy ->
                        if Physics.canRepatriateStandard state.position state.speed model.course model.playerFaction (Just carriedEnemy) then
                            let
                                slot =
                                    mySlot model

                                updatedStandards =
                                    List.map
                                        (\s ->
                                            if s == CarriedBy slot carriedEnemy then
                                                AtCitadel carriedEnemy

                                            else
                                                s
                                        )
                                        droppedPirateStandards
                            in
                            { standards = updatedStandards
                            , carriedStandard = Nothing
                            , toast = Just ( T.standardRepatriatedToast carriedEnemy model.language, 2.5 )
                            , repatriated = True
                            }

                        else
                            { standards = droppedPirateStandards
                            , carriedStandard = Just carriedEnemy
                            , toast = Nothing
                            , repatriated = False
                            }

            else
                { standards = droppedPirateStandards
                , carriedStandard = model.state.carriedStandard
                , toast = Nothing
                , repatriated = False
                }

        finalState =
            { state | carriedStandard = regalia.carriedStandard }

        boardingPirate =
            if racing && model.pirateCooldown <= 0 then
                piratesNow
                    |> List.filter (\( p, ps ) -> horizontal state.position ps.position <= Pirates.boardingRange p.kind)
                    |> List.map Tuple.first
                    |> List.head

            else
                Nothing

        boarding =
            boardingPirate /= Nothing

        countedHits =
            if model.progress.finished then
                0

            else
                List.length sunkNow

        treasure =
            if racing then
                Physics.reachedTreasure state model.course.treasures progressAfterHarbours.treasureFound

            else
                []

        -- The coins of this frame (`Sails.Coins`); the harbour's coins
        -- are already in `progressAfterHarbours`.
        sunkCoins =
            if model.progress.finished then
                0

            else
                Coins.pirateSunk * List.length sunkNow

        boardingCoins =
            if boarding then
                Coins.pirateBoarded

            else
                0

        treasureCoins =
            Coins.treasureChest * List.length treasure

        scored =
            { progressAfterHarbours
                | shipsSunk = progressAfterHarbours.shipsSunk + countedHits
                , treasureFound = progressAfterHarbours.treasureFound ++ List.map .id treasure
                , coins = progressAfterHarbours.coins + sunkCoins + boardingCoins + treasureCoins
            }

        -- A smashed harbour can no longer be docked at: its leg is done
        -- and the boat sails on to the next (`Harbours.skipWrecked`).
        ( progress, skippedHarbours ) =
            if racing then
                Harbours.skipWrecked harbourDamage scored

            else
                ( scored, [] )

        finishedBySkip =
            not (List.isEmpty skippedHarbours) && progress.finished && not scored.finished

        -- Coins flying: a burst out of each ship sunk, a pop over the boat
        -- for a boarding fought off or a chest taken aboard.
        coinSeed =
            round (time * 60)

        newCoins =
            (if sunkCoins > 0 then
                List.concatMap (\( p, ps ) -> Coins.burst (coinSeed + p.id) ps.position) sunkNow

             else
                []
            )
                ++ (if boardingCoins + treasureCoins > 0 then
                        Coins.pop coinSeed (Physics.shipVelocity state) state.position

                    else
                        []
                   )

        treasureToast =
            if List.isEmpty treasure then
                Nothing

            else
                Just ( T.treasureToast model.language treasureCoins, 1.6 )

        passToast =
            case passed of
                Just i ->
                    if progress.finished then
                        Just ( T.finishToast model.language, 1.5 )

                    else
                        Just ( T.harbourToast model.language (i + 1) progress.totalWaypoints ++ " · " ++ T.coinsGained model.language Coins.harbourDocked, 1.0 )

                Nothing ->
                    if finishedBySkip then
                        Just ( T.finishToast model.language, 1.5 )

                    else
                        Nothing

        harbourToast =
            if not (List.isEmpty smashedHarbours) then
                Just ( T.harbourSmashedToast model.language, 1.8 )

            else
                case List.reverse harbourHits of
                    i :: _ ->
                        Just ( T.harbourHitToast model.language (Harbours.hitsLeft harbourDamage i), 1.2 )

                    [] ->
                        Nothing

        hitToast =
            if countedHits > 0 then
                Just ( T.hitToast model.language, 1.0 )

            else
                Nothing

        hullToast =
            if justWrecked then
                Just ( T.shipwrecked model.language, 2.0 )

            else
                case boardingPirate of
                    Just pirate ->
                        Just ( T.factionBoardingToast pirate.faction model.language ++ " " ++ T.coinsGained model.language boardingCoins, 1.4 )

                    Nothing ->
                        case sunkNow of
                            ( sunkPirate, _ ) :: _ ->
                                Just ( T.factionSunkToast sunkPirate.faction model.language ++ " " ++ T.coinsGained model.language sunkCoins, 1.4 )

                            [] ->
                                if knocked then
                                    Just ( T.hullDamagedToast model.language (Hull.percent repairedHull), 1.2 )

                                else
                                    Nothing

        reefToast =
            case reefImpact of
                Just _ ->
                    Just ( T.reefScrapeToast model.language, 1.8 )

                Nothing ->
                    Nothing

        ghostToast =
            if ghostBanished then
                Just ( T.ghostShipBanishedToast model.language, 2.5 )

            else if ghostAppeared then
                Just ( T.ghostShipAppearedToast model.language, 2.0 )

            else
                Nothing

        toast =
            case List.filterMap identity [ ghostToast, reefToast, regalia.toast, hullToast, passToast, harbourToast, treasureToast, hitToast ] of
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

        -- Battle sounds from others' balls carry only `combatHearingRange`.
        nearBoat =
            withinEarshot state.position

        ownBallIds =
            model.cannonballs
                |> List.filter (\b -> b.sourceSlot == mySlot model)
                |> List.map .id

        heardSplash splash =
            List.member splash.id ownBallIds || nearBoat splash.position

        heardFall s =
            List.member s.id ownStructureHits || nearBoat (vec3 s.x 0 s.z)

        sfxCmds =
            [ case passed of
                Just _ ->
                    if progress.finished then
                        sfx Sound.Finish

                    else
                        sfx Sound.Bell

                Nothing ->
                    if finishedBySkip then
                        sfx Sound.Finish

                    else
                        Cmd.none

            -- What the boat's balls struck this frame, plus what others'
            -- balls struck within earshot (`combatHearingRange`): the
            -- biggest event speaks (a ship sunk over a pier smashed over a
            -- mere hit), and a splash only when nothing was hit.
            , if not (List.isEmpty sunkNow) then
                sfx Sound.ShipSunk

              else if not (List.isEmpty smashedHarbours) || List.any heardFall fallen.structures then
                sfx Sound.Collapse

              else if not (List.isEmpty pirateFlashes) then
                sfx Sound.HitShip

              else if not (List.isEmpty harbourFlashes) then
                sfx Sound.HitDock

              else if not (List.isEmpty structureFlashes) || List.any (.position >> nearBoat) otherStructureFlashes then
                sfx Sound.HitBuilding

              else if List.any heardSplash splashes then
                sfx (Sound.Splash 0.5)

              else
                Cmd.none

            -- A pirate volley is heard only within earshot of the boat.
            , if List.any (.position >> nearBoat) pirateShots then
                sfx Sound.EnemyCannon

              else
                Cmd.none
            , if sunkCoins + boardingCoins + treasureCoins > 0 then
                sfx (Sound.Coins (sunkCoins + boardingCoins + treasureCoins))

              else
                Cmd.none
            , if boltStruck && not model.audioMuted then
                sfx Sound.Thunder

              else
                Cmd.none
            , if state.bumped && not model.state.bumped then
                sfx Sound.Bump

              else
                Cmd.none
            , if justWrecked then
                sfx Sound.Shipwrecked

              else if knocked then
                sfx (Sound.HullHit (clamp 0.4 1 ((model.hull.health - hull.health) / 20)))

              else
                Cmd.none
            , if regalia.repatriated then
                sfx Sound.Finish

              else
                Cmd.none
            , if ghostBanished then
                sfx Sound.GhostBanish

              else if ghostAppeared then
                sfx Sound.GhostAppear

              else
                Cmd.none
            , if debrisCollected > 0 then
                sfx Sound.DebrisCollected

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

                   else if not (List.isEmpty rammedHarbours) then
                    0.6

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
                                { coinTarget = Coins.courseTarget model.course
                                , personalBest = Maybe.map .coins model.personalBest
                                }
                                progress
                            )

            else
                Nothing

        celebration =
            (if regalia.repatriated then
                Ending.confetti 1 state

             else
                []
            )
                ++ (case ending of
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
                   )

        crewClock =
            if roomVoyage model then
                model.crewClock + dt

            else
                model.crewClock

        -- This ship just moored on the room's voyage: tell the crew when.
        justMoored =
            roomVoyage model && progress.finished && not (Dict.member (mySlot model) model.moorings)

        moorings =
            if justMoored then
                Dict.insert (mySlot model) crewClock model.moorings

            else
                model.moorings

        mooredCmd =
            if justMoored then
                GamePorts.sendMultiplayerEvents [ Crew.mooredEvent crewClock state.position ]

            else
                Cmd.none

        -- This ship was just wrecked in a room: the crew sees it break up.
        wreckedCmd =
            if justWrecked && model.multiplayer /= Nothing then
                GamePorts.sendMultiplayerEvents [ Crew.wreckedEvent crewClock state.position state.heading ]

            else
                Cmd.none

        -- The latest mooring of the crew, once all are in port, is the
        -- crew's time.
        crewTime =
            case ( model.crewTime, model.multiplayer ) of
                ( Nothing, Just room ) ->
                    case Crew.ofRoom model.multiplayer progress.finished of
                        Just crew ->
                            if Crew.allInPort crew then
                                Just (Crew.sharedTime room moorings crewClock)

                            else
                                Nothing

                        Nothing ->
                            Nothing

                _ ->
                    model.crewTime

        ghostDebris =
            if ghostBanished then
                Wreck.breakUp Wreck.GhostShip updatedGhostShip.state

            else
                []

        newCooldown =
            max 0 (model.cannonCooldown - dt)
    in
    ( { model
        | state = state
        , progress = progress
        , phase = phase
        , follow = follow
        , simTime = time
        , weather = weather
        , cannonballs = afterStructures ++ othersAfterStructures ++ pirateShots
        , impacts = Physics.stepImpacts dt (model.impacts ++ splashes ++ pirateFlashes ++ harbourFlashes ++ boatFlashes ++ structureFlashes ++ otherStructureFlashes ++ fallen.flashes)
        , piratesSunk = model.piratesSunk ++ List.map (Tuple.first >> .id) sunkNow
        , pirateHits = pirateHitsNow
        , standards = regalia.standards
        , pirateCooldown =
            if boarding then
                Pirates.boardingCooldown

            else
                max 0 (model.pirateCooldown - dt)
        , cannonCooldown = newCooldown
        , sinceShot =
            if racing then
                model.sinceShot + dt

            else
                model.sinceShot
        , nextCannonId = nextShotId
        , pirateGuns = pirateGuns
        , toast = toast
        , hull = repairedHull
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
        , debris =
            debris
                ++ List.concatMap (\( p, ps ) -> Wreck.breakUp (pirateWreck p) ps) sunkNow
                ++ pierDebris
                ++ townPierDebris
                ++ ghostDebris
                ++ celebration
        , flyingCoins = Coins.step dt model.flyingCoins ++ newCoins
        , ending = ending
        , crewClock = crewClock
        , moorings = moorings
        , crewTime = crewTime
        , ghostShip = updatedGhostShip
        , harbourDamage = harbourDamage
        , structures = structures
        , damageMarks = damageMarks
        , combatTurnTarget =
            case model.combatTurnTarget of
                Just targetH ->
                    let
                        rawErr =
                            targetH - state.heading

                        err =
                            rawErr - 360.0 * toFloat (floor ((rawErr + 180.0) / 360.0))
                    in
                    if abs err < 3.0 || model.touchLeft || model.touchRight || model.keyLeft || model.keyRight || model.anchored || (model.tilt.enabled && model.tiltSteer /= 0) then
                        Nothing

                    else
                        Just targetH

                Nothing ->
                    Nothing
      }
    , Cmd.batch (audioCmd :: telemetryCmd :: mooredCmd :: wreckedCmd :: sfxCmds)
    )


{-| What a sunk patrol ship breaks up into: the parts of its own kind's
model (`Pirates.shipNameFor`), at its kind's size (`Pirates.shipScale`).
-}
pirateWreck : Pirate -> Wreck.Source
pirateWreck pirate =
    Wreck.Ship { name = Pirates.shipNameFor pirate, scale = Pirates.shipScale pirate.kind }


{-| The pirates in reach of the boat fire at it when their guns are ready
(`Pirates.reach`, `Pirates.reloadSeconds`): one lobbed ball each, from the
pirate's own gun slot. With the boat out of reach a pirate fires at the
nearest building in reach on a rival faction's island instead
(`Structures.npcTarget`). Returns the new balls, the next ball id and how
long each pirate's gun has left to reload.
-}
pirateVolleys : Float -> SailingState -> Structures.State -> Int -> List ( Pirate, SailingState ) -> Dict Int Float -> ( List Cannonball, Int, Dict Int Float )
pirateVolleys dt boat structures nextId pirates guns =
    List.foldl
        (\( pirate, ps ) ( balls, id, reload ) ->
            let
                left =
                    (Dict.get pirate.id reload |> Maybe.withDefault 0) - dt

                target =
                    if left <= 0 then
                        Structures.npcTarget Pirates.reach
                            ps.position
                            boat.position
                            (List.map Structures.aimPoint (Structures.rivalBuildings pirate.faction structures))

                    else
                        Nothing
            in
            case target of
                Just aim ->
                    let
                        shot =
                            Physics.fireCannons (Pirates.shipSlot pirate.id) ps [ aim ] id
                    in
                    ( balls ++ shot, id + List.length shot, Dict.insert pirate.id (Pirates.reloadSeconds pirate) reload )

                Nothing ->
                    ( balls, id, Dict.insert pirate.id (max 0 left) reload )
        )
        ( [], nextId, guns )
        pirates


subscriptions : SailModel -> Sub SailMsg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "code" Decode.string))
        , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "code" Decode.string))
        , Browser.Events.onVisibilityChange VisibilityChanged
        , GamePorts.tiltSteer TiltSteer
        , GamePorts.tiltOrientationReceived TiltOrientation
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
remoteBoat : List StandardState -> RemoteShipLookup -> Multiplayer.RemotePlayer -> RenderBoat
remoteBoat standards remoteShip p =
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
        , carriedStandard = standardForCarrier p.slot standards
        }
    , sailTint = Just (slotTint p.slot)
    , isGhost = False
    , anchored = False
    , scale = 1
    }


{-| A pirate ship as drawn: its kind's model (`Pirates.shipNameFor`) at its
kind's size (`Pirates.shipScale`), with its faction's sails.
-}
pirateBoat : List StandardState -> (String -> Maybe ShipMesh) -> ( Pirate, SailingState ) -> RenderBoat
pirateBoat standards meshFor ( pirate, state ) =
    { mesh = meshFor (Pirates.shipNameFor pirate)
    , state =
        case standardForCarrier (Pirates.shipSlot pirate.id) standards of
            Just f ->
                { state | carriedStandard = Just f }

            Nothing ->
                state
    , sailTint = Just (Pirates.sailTint pirate)
    , isGhost = False
    , anchored = False
    , scale = Pirates.shipScale pirate.kind
    }


{-| Draw the race. `meshFor` looks up a boat's brick geometry by boat name
(see `Main`); the player's own boat is looked up as `model.boat.name`.
-}
viewSail :
    SailModel
    ->
        { onReturnToShipyard : SailMsg
        , onRestartCourse : SailMsg
        , onNewMap : SailMsg
        , meshFor : String -> Maybe ShipMesh
        , remoteShip : RemoteShipLookup
        , props : Render.Props
        }
    -> Html SailMsg
viewSail model callbacks =
    viewRace model callbacks model.multiplayer


viewRace :
    SailModel
    -> { onReturnToShipyard : SailMsg, onRestartCourse : SailMsg, onNewMap : SailMsg, meshFor : String -> Maybe ShipMesh, remoteShip : RemoteShipLookup, props : Render.Props }
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
                [ { mesh = callbacks.meshFor model.boat.name, state = model.state, sailTint = Nothing, isGhost = False, anchored = model.anchored, scale = 1 } ]

        ghostBoat =
            if model.ghostShip.active then
                [ Render.ghostBoat (callbacks.meshFor "Schooner") model.ghostShip.state ]

            else
                []

        boats =
            ownBoat
                ++ List.map (remoteBoat model.standards callbacks.remoteShip) (remotePlayers model)
                ++ List.map (pirateBoat model.standards callbacks.meshFor) (afloatPirates model)
                ++ ghostBoat

        loadingShips =
            remotePlayers model
                |> List.filter (\p -> (callbacks.remoteShip p.vehicleId).loading)

        countdown =
            case model.phase of
                WaitingForRoom ->
                    case maybeRoom |> Maybe.map .status of
                        Just (Multiplayer.Countdown n) ->
                            Just n

                        _ ->
                            Nothing

                Racing ->
                    Nothing

        -- The weather of this frame, worked out once for the scene and the HUD.
        conditions =
            Weather.conditions model.course.wind model.weather

        shipMeshOf source =
            case source of
                Wreck.Ship ship ->
                    callbacks.meshFor ship.name

                Wreck.RemoteShip vehicleId ->
                    (callbacks.remoteShip vehicleId).mesh

                Wreck.GhostShip ->
                    callbacks.meshFor "Schooner"

                Wreck.HarbourPier ->
                    callbacks.meshFor (Scenery.key Scenery.pier)

        worldArgs =
            { course = model.course
            , nextIndex =
                if model.progress.finished then
                    model.progress.totalWaypoints

                else
                    model.progress.currentWaypointIndex
            , boats = boats
            , guide = nextHarbour model |> Maybe.map (\n -> { from = model.state.position, to = n })
            , balls = model.cannonballs
            , impacts = model.impacts
            , time = model.simTime
            , weather = conditions
            , scenery = callbacks.meshFor
            , treasureFound = model.progress.treasureFound
            , props = callbacks.props
            , islandMeshes = Structures.islandsFor model.structures model.islandMeshes
            , debris = model.debris
            , coins = model.flyingCoins
            , shipMeshOf = shipMeshOf
            , standards = model.standards
            , harbourDamage = model.harbourDamage
            , structures = model.structures
            , damageMarks = model.damageMarks
            }
    in
    div
        [ Attr.class "is-racing relative w-full h-full overflow-hidden bg-brand select-none font-sans"

        -- A tap anywhere in the race starts tilt steering where the phone
        -- still has to ask for the sensor (iOS), see tilt-steer.js.
        , Attr.attribute "data-tilt-start" ""
        ]
        [ -- Alone or in an online room, the player's own follow view fills
          -- the screen; the other players' boats sail in it. Keyed by the
          -- graphics generation, see `GraphicsRestored`.
          Keyed.node "div"
            [ Attr.class "absolute inset-0 z-0"
            , on "pointerdown"
                (Decode.map3 (\x y pt -> PointerFire { x = x, y = y, pointerType = pt })
                    (Decode.field "clientX" Decode.float)
                    (Decode.field "clientY" Decode.float)
                    (Decode.at [ "pointerType" ] Decode.string)
                )
            ]
            [ ( "world-" ++ String.fromInt model.graphics.generation
              , Render.renderWorld graphicsEvents
                    (CameraRig.followView model.follow)
                    worldArgs
                    (FrameGuard.scaled model.frameGuard model.viewportWidth)
                    (FrameGuard.scaled model.frameGuard model.viewportHeight)
              )
            ]
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
                , touchShoot = model.touchShoot
                , audioMuted = model.audioMuted
                , tilt = model.tilt
                , countdown = countdown
                , toast =
                    if model.graphics.lost then
                        Just (T.graphicsRestoring model.language)

                    else
                        Maybe.map Tuple.first model.toast
                , sailLevel = model.sailLevel
                , wind = conditions.wind
                , weather = Weather.label model.weather
                , flash = conditions.flash
                , hull = model.hull
                , hazards = { rocks = model.rockTimer > 0, pounding = model.seaStrain > poundingStrain }
                , stopped = abs model.state.speed < stoppedSpeed
                , nextHarbourDistance =
                    nextHarbour model
                        |> Maybe.map (\n -> Vec3.distance (Vec3.setY 0 n) (Vec3.setY 0 model.state.position))
                , courseBearing = backToCourse model
                , language = model.language
                , canAnchor = Physics.canDropAnchor model.course.waypoints model.progress model.state
                , isAnchored = model.anchored
                , showHelp = model.showHelp
                , goals = buildHudGoals model
                , hintVisible = HUD.hintVisible { sinceShot = model.sinceShot, anchored = model.anchored }
                , actions =
                    { onSteer = Steer
                    , onRaiseSail = RaiseSail
                    , onLowerSail = LowerSail
                    , onToggleMute = ToggleMute
                    , onToggleHelp = ToggleHelp
                    , onRestart = callbacks.onRestartCourse
                    , onExit = callbacks.onReturnToShipyard
                    , onDropAnchor = DropAnchor
                    , onFire = FireCannons
                    }
                }
        , hostHeader model.language (crewDone model) maybeRoom
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
                    , crew =
                        crewOf model
                            |> Maybe.map
                                (\crew ->
                                    { inPort = crew.inPort
                                    , total = crew.total
                                    , clock = model.crewClock
                                    , crewTime = model.crewTime
                                    }
                                )
                    , language = model.language
                    , actions =
                        { onSailAgain = callbacks.onRestartCourse
                        , onNewMap = callbacks.onNewMap
                        , onBackToMenu = callbacks.onReturnToShipyard
                        }
                    }

            _ ->
                text ""
        ]


{-| Listeners on the 3D canvas for its WebGL context. A lost context is
`preventDefault`ed so the browser may restore it (mobile browsers drop the
context under memory pressure or when the tab is backgrounded).
-}
graphicsEvents : List (Html.Attribute SailMsg)
graphicsEvents =
    [ preventDefaultOn "webglcontextlost" (onShownCanvas ( GraphicsLost, True ))
    , on "webglcontextrestored" (onShownCanvas GraphicsRestored)
    ]


{-| Only for the canvas on the page: `webgl-release.js` loses the context of
a canvas Elm has dropped (a voyage left, a restored context replaced by a
fresh canvas), and that old canvas still carries these handlers.
-}
onShownCanvas : a -> Decode.Decoder a
onShownCanvas msg =
    -- A browser without `isConnected` (none still in use) counts as shown.
    Decode.oneOf [ Decode.at [ "target", "isConnected" ] Decode.bool, Decode.succeed True ]
        |> Decode.andThen
            (\connected ->
                if connected then
                    Decode.succeed msg

                else
                    Decode.fail "canvas no longer shown"
            )


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


{-| The whole crew of the room is in port: its voyage is complete.
-}
crewDone : SailModel -> Bool
crewDone model =
    model.crewTime /= Nothing


{-| Host controls (room code, player count, copy link, start) for the host
of a room. Once the whole crew is in port (`done`) the host can start the
crew's next voyage. In portrait it sits below the HUD's status panel and the
countdown, which share the top of a narrow screen.
-}
hostHeader : Language -> Bool -> Maybe RoomState -> Html SailMsg
hostHeader language done maybeRoom =
    case maybeRoom of
        Just room ->
            case room.role of
                HostCommentator ->
                    div
                        [ Attr.class "absolute top-[200px] landscape:top-3 inset-x-0 mx-auto w-fit max-w-[calc(100%-16px)] z-40 flex flex-wrap justify-center items-center gap-2 landscape:gap-3 px-3 landscape:px-4 py-2 rounded-2xl bg-brand/90 backdrop-blur-md border border-white/10 shadow-2xl pointer-events-auto"
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
                        , case ( room.status, done ) of
                            ( Lobby, _ ) ->
                                startButton language

                            ( Multiplayer.Countdown n, _ ) ->
                                statusChip "text-amber-400 bg-amber-500/10 border-amber-500/20"
                                    (T.raceStatus language { countdown = Just n, finished = False })

                            ( _, True ) ->
                                span [ Attr.class "contents", Attr.attribute "data-testid" "crew-done" ]
                                    [ statusChip "text-purple-400 bg-purple-500/10 border-purple-500/20"
                                        (T.raceStatus language { countdown = Nothing, finished = True })
                                    , startButton language
                                    ]

                            ( _, False ) ->
                                statusChip "text-emerald-400 bg-emerald-500/10 border-emerald-500/20"
                                    (T.raceStatus language { countdown = Nothing, finished = False })
                        ]

                JoinedPlayer ->
                    text ""

        Nothing ->
            text ""


startButton : Language -> Html SailMsg
startButton language =
    button
        [ Attr.type_ "button"
        , onClick StartMultiplayerCountdown
        , Attr.class "text-xs font-black px-3.5 py-1 rounded-xl bg-gradient-to-r from-emerald-500 to-teal-500 hover:brightness-110 active:scale-95 text-slate-950 shadow-lg shadow-emerald-500/25 whitespace-nowrap"
        ]
        [ text (T.startRace language) ]


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
