module Main exposing (Flags, Model, Msg, main)

{-| Top-level Elm Architecture entry point for Brick Sails (`brick-sails`).
Deployed to <https://ohoi.palikkaharrastajat.fi>.

Two screens: the boat picker (`Pages.Shipyard`) and the race (`Pages.Sail`).

The interface is in Finnish unless the `language` flag (a `?lang=` link or a
saved choice, read by the shared boot code) asks for English. The FI / EN
switch on the boat picker changes it and saves the choice.

Every boat is a brick model. `Main` owns the part cache: it parses each boat's
LDraw document once, fetches the parts it needs from the catalog
(`LDraw.Resolve`, one pre-bundled MPD per part), and builds each boat's
geometry once all of its parts are in (`Sails.ShipMesh`). The meshes are kept
in `meshes` by boat name and handed to the views.

A ship built in the brick building apps can be loaded from a build link in
the page URL, a pasted link or an opened model file (`Sails.Ingest`). It
appears as an extra "Your ship" entry in the boat picker.

A ship can also be built here, in the ship editor (`Pages.Build`, open while
`builder` is set): the player picks one of the starter hulls and builds on
it. "Use ship" loads the build exactly like a build link, as "Your ship", and
puts its link in the URL; "Edit ship" opens a ship standing on a starter hull
again.

In a multiplayer room every player shares their ship once: a built-in boat by
name, a loaded build as a `#b/...` build link. Room updates only name it by
`vehicleId`; the JS runtime fetches each ship once and hands it over through
`GamePorts.multiplayerVehicleLoaded`. A shared build goes through the same checks
and geometry pipeline as "Your ship" and is kept in `documents` and `meshes`
under `remoteKey id`, so it is built once per id.

Play in a room is cooperative: the players are one crew on one course, and
the room's result is the crew's shared time (`Sails.Crew`, `Pages.Sail`), so
only solo runs go to the local leaderboard.

-}

import Browser
import Browser.Dom as Dom
import Browser.Events
import Dict exposing (Dict)
import Game.BrickProps as BrickProps
import Game.MapParams as MapParams exposing (Biome, MapParams, Pick(..), Season, Selection, WeatherKind)
import Game.Multiplayer as Multiplayer
import Game.PartLoading as PartLoading
import Game.Ports as GamePorts
import Html exposing (Html)
import Http
import I18n exposing (Language(..))
import Json.Decode as Decode
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, ResolverConfig)
import Pages.Build as Build
import Pages.Sail as Sail
import Pages.Shipyard as Shipyard
import Ports
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.Editor as Editor
import Sails.Handling as Handling
import Sails.I18n as SailsI18n
import Sails.Ingest as Ingest
import Sails.NpcShips as NpcShips
import Sails.Records as Records
import Sails.Render as Render
import Sails.Room as SailsRoom
import Sails.Scenery as Scenery
import Sails.ShipMesh as ShipMesh exposing (ShipMesh)
import Sails.Types exposing (Boat, BoatKind(..), Course, ViewMode(..))
import Task


type alias Flags =
    { savedAudioMuted : Bool
    , savedLeaderboardJson : String
    , ldrawBase : String
    , ldrawFallbackBase : String
    , initialHash : String
    , predecodedInitialBytes : Maybe (List Int)
    , language : String
    , supportsPointerEvents : Bool
    }


{-| What we know about a ship another player shared, by vehicle id.
-}
type RemoteShip
    = RemoteBoat String
    | RemoteBuild
    | RemoteUnusable


type alias Model =
    { resolver : ResolverConfig
    , partCache : PartCache
    , documents : Dict String Document
    , meshes : Dict String ShipMesh
    , props : Render.Props
    , customShip : Maybe Boat
    , remoteShips : Dict String RemoteShip
    , loadError : Maybe String
    , boat : Boat
    , selection : Selection
    , seedText : String
    , seedEditing : Bool
    , menuTab : Shipyard.MenuTab
    , lastMap : Maybe MapParams
    , course : Course
    , viewMode : ViewMode
    , sailModel : Maybe Sail.SailModel
    , audioMuted : Bool
    , leaderboard : Records.Board
    , tilt : { available : Bool, enabled : Bool }
    , shipyardTime : Float
    , viewportWidth : Int
    , viewportHeight : Int
    , roomCodeInput : String
    , language : Language
    , ldrawBase : String
    , ldrawFallbackBase : String
    , supportsPointerEvents : Bool
    , builder : Maybe Build.Model
    }


type Msg
    = StartSailing
    | StartHostMultiplayer Multiplayer.SessionMode
    | JoinMultiplayer String
    | RoomCodeChanged String
    | MultiplayerRoomChanged Decode.Value
    | MultiplayerError String
    | MultiplayerVehicleLoaded { id : String, payload : String }
    | ReturnToShipyard
    | ToggleMute
    | TiltStatusChanged { available : Bool, enabled : Bool }
    | ShipyardTick Float
    | ViewportResized { width : Int, height : Int }
    | SailMessage Sail.SailMsg
    | PartFetched String (Result Http.Error String)
    | HashChanged String
    | StartBuild String
    | EditShip
    | BuildMsg Build.Msg
    | PickBiome (Pick Biome)
    | PickSeason (Pick Season)
    | PickWeather (Pick WeatherKind)
    | SelectMenuTab Shipyard.MenuTab
    | EditSeed
    | SeedTyped String
    | CommitSeed
    | CopyMapLink
    | RollWorld
    | UsePreviousMap
    | NoOp


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
        board =
            Records.tidy (Records.fromJson flags.savedLeaderboardJson)

        previous =
            Records.lastMap board

        -- A link's map, else the map sailed last, else every pick left to
        -- chance with seed 1.
        selection =
            case ( Ingest.mapFromHash flags.initialHash, previous ) of
                ( Just linked, _ ) ->
                    exactSelection linked

                ( Nothing, Just last ) ->
                    exactSelection last

                ( Nothing, Nothing ) ->
                    MapParams.anySelection

        base =
            { resolver = PartLoading.resolverFromFlags flags
            , partCache = Resolve.initCache
            , documents =
                (Boats.all |> List.map (\b -> ( b.name, Document.parse b.ldraw )))
                    ++ (NpcShips.all |> List.map (\( name, ldraw ) -> ( name, Document.parse ldraw )))
                    ++ (Scenery.all |> List.map (\piece -> ( Scenery.key piece, Document.parse piece.ldraw )))
                    |> Dict.fromList
            , meshes = Dict.empty
            , props = Dict.empty
            , customShip = Nothing
            , remoteShips = Dict.empty
            , loadError = Nothing
            , boat = Boats.default
            , selection = selection
            , seedText = MapParams.toCode (MapParams.resolve selection)
            , seedEditing = False
            , menuTab = Shipyard.WorldTab
            , lastMap = previous
            , course = courseOf selection
            , viewMode = ViewShipyard
            , sailModel = Nothing
            , audioMuted = flags.savedAudioMuted
            , leaderboard = board
            , tilt = { available = False, enabled = False }
            , shipyardTime = 0
            , viewportWidth = 1024
            , viewportHeight = 768
            , roomCodeInput = ""
            , language = SailsI18n.fromFlag flags.language
            , ldrawBase = flags.ldrawBase
            , ldrawFallbackBase = flags.ldrawFallbackBase
            , supportsPointerEvents = flags.supportsPointerEvents
            , builder = Nothing
            }

        hasPayload =
            flags.predecodedInitialBytes /= Nothing || Ingest.hasShipPayload flags.initialHash
    in
    if hasPayload then
        loadShip flags.predecodedInitialBytes flags.initialHash base

    else
        useFirstShip base


{-| Start a new player on `Editor.firstShip` (a rigged starter hull) as "Your
ship", so the builder button opens it; the built-in default boat stays when
it cannot be made.
-}
useFirstShip : Model -> ( Model, Cmd Msg )
useFirstShip model =
    case Editor.firstShip of
        Just ( base, doc ) ->
            case Ingest.validateShip doc of
                Ok spec ->
                    useShip (Ok ( { document = doc, title = hullName model.language base }, spec )) model

                Err _ ->
                    requestParts model

        Nothing ->
            requestParts model


{-| The first seat's course of the map a selection resolves to.
-}
courseOf : Selection -> Course
courseOf selection =
    Archipelago.courseFor (Archipelago.generate (MapParams.resolve selection)) 0


{-| A selection with every part fixed to the map's values, so a shared link
or the previous map shows its concrete map.
-}
exactSelection : MapParams -> Selection
exactSelection map =
    { biome = Exactly map.biome
    , season = Exactly map.season
    , weather = Exactly map.weather
    , seed = map.seed
    }


{-| Set the picker selection and the course of the map it resolves to.
-}
withSelection : Selection -> Model -> Model
withSelection selection model =
    let
        course =
            if MapParams.resolve selection == model.course.map then
                model.course

            else
                courseOf selection
    in
    { model | selection = selection, seedText = MapParams.toCode (MapParams.resolve selection), course = course }


{-| Change the selection from the map picker and write the map (and the
player's build) into the page link.
-}
applySelection : (Selection -> Selection) -> Model -> ( Model, Cmd Msg )
applySelection change model =
    let
        next =
            withSelection (change model.selection) model

        ( refreshed, partsCmd ) =
            syncCourseProps model.course.map next
    in
    ( refreshed, Cmd.batch [ partsCmd, GamePorts.setUrlHash (mapLink refreshed) ] )


{-| Rebuild prop sources when the active map changes so only the active
island palette is loaded and stale palettes are evicted.
-}
syncCourseProps : MapParams -> Model -> ( Model, Cmd Msg )
syncCourseProps prevMap model =
    if model.course.map /= prevMap then
        requestParts model

    else
        ( model, Cmd.none )


{-| The page link of the shipyard: the map, and "Your ship" when selected.
-}
mapLink : Model -> String
mapLink model =
    Ingest.encodeMapHash model.course.map
        (if model.boat.kind == CustomShip then
            Dict.get model.boat.name model.documents

         else
            Nothing
        )


{-| Close the seed field and use what was typed in it: a map code (as in a
shared link) sets the whole map, a number sets the seed, anything else is
dropped.
-}
commitSeed : Model -> ( Model, Cmd Msg )
commitSeed model =
    let
        closed =
            { model | seedEditing = False, seedText = MapParams.toCode (MapParams.resolve model.selection) }

        trimmed =
            String.trim model.seedText

        maybeMap =
            case MapParams.fromCode trimmed of
                Just pasted ->
                    Just pasted

                Nothing ->
                    Ingest.mapFromHash trimmed
    in
    case maybeMap of
        Just pasted ->
            applySelection (\_ -> exactSelection pasted) closed

        Nothing ->
            case String.toInt (String.replace "#" "" trimmed) of
                Just n ->
                    applySelection (\sel -> { sel | seed = clamp 1 999999 n }) closed

                Nothing ->
                    ( closed, Cmd.none )


{-| The next seed from the dice button, 1 to 999999 (as Brick Racer's).
-}
nextRandomSeed : Int -> Int
nextRandomSeed seed =
    modBy 999999 (abs (seed * 1664525 + 1013904223)) + 1


personalBest : Model -> Maybe Sails.Types.LeaderboardEntry
personalBest model =
    Records.bestForCourse model.course.id model.leaderboard


newSail : Maybe Multiplayer.RoomState -> Model -> Sail.SailModel
newSail room model =
    Sail.initSail model.language model.boat model.course model.audioMuted (personalBest model) room model.tilt model.viewportWidth model.viewportHeight


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
        PartFetched name result ->
            requestParts { model | partCache = Resolve.updateCache name result Parser.parseFile model.partCache }

        HashChanged hash ->
            if model.viewMode == ViewShipyard then
                let
                    mapped =
                        case Ingest.mapFromHash hash of
                            Just linked ->
                                withSelection (exactSelection linked) model

                            Nothing ->
                                model

                    ( synced, mapCmd ) =
                        syncCourseProps model.course.map mapped
                in
                if Ingest.hasShipPayload hash then
                    let
                        ( loaded, loadCmd ) =
                            loadShip Nothing hash synced
                    in
                    ( loaded, Cmd.batch [ mapCmd, loadCmd ] )

                else
                    ( synced, mapCmd )

            else
                ( model, Cmd.none )

        PickBiome pick ->
            applySelection (\sel -> { sel | biome = pick }) model

        PickSeason pick ->
            applySelection (\sel -> { sel | season = pick }) model

        PickWeather pick ->
            applySelection (\sel -> { sel | weather = pick }) model

        SelectMenuTab tab ->
            ( { model | menuTab = tab }, Cmd.none )

        EditSeed ->
            ( { model | seedEditing = True, seedText = MapParams.toCode (MapParams.resolve model.selection) }
            , Task.attempt (\_ -> NoOp) (Dom.focus Shipyard.seedInputId)
            )

        SeedTyped raw ->
            ( { model | seedText = raw }, Cmd.none )

        CommitSeed ->
            if model.seedEditing then
                commitSeed model

            else
                ( model, Cmd.none )

        CopyMapLink ->
            let
                ( next, cmd ) =
                    if model.seedEditing then
                        commitSeed model

                    else
                        ( model, Cmd.none )
            in
            ( next, Cmd.batch [ cmd, GamePorts.copyShareUrl (MapParams.toCode (MapParams.resolve next.selection)) ] )

        RollWorld ->
            -- A new seed with every pick left to chance, so the dice shows
            -- every kind of world.
            let
                chance =
                    MapParams.anySelection
            in
            applySelection (\sel -> { chance | seed = nextRandomSeed sel.seed }) model

        NoOp ->
            ( model, Cmd.none )

        UsePreviousMap ->
            case model.lastMap of
                Just previous ->
                    applySelection (\_ -> exactSelection previous) model

                Nothing ->
                    ( model, Cmd.none )

        StartSailing ->
            ( { model | viewMode = ViewSailing, sailModel = Just (newSail Nothing model) }
            , GamePorts.calibrateTilt ()
            )

        StartHostMultiplayer mode ->
            ( model
            , GamePorts.createMultiplayerRoom
                { roomId = ""
                , courseId = MapParams.toCode model.course.map
                , name = "Host"
                , vehicleHash = sharedShip model
                , maxPlayers = Multiplayer.sessionCapacity mode
                }
            )

        RoomCodeChanged code ->
            ( { model | roomCodeInput = Multiplayer.roomCodeFrom code }, Cmd.none )

        JoinMultiplayer code ->
            ( model
            , GamePorts.joinMultiplayerRoom
                { roomId = Multiplayer.roomCodeFrom code
                , name = "Sailor"
                , vehicleHash = sharedShip model
                }
            )

        MultiplayerRoomChanged rawJson ->
            case SailsRoom.fromPort rawJson of
                SailsRoom.NoRoom ->
                    -- Left the room (or never in one): stay where we are, in no room.
                    ( { model | sailModel = model.sailModel |> Maybe.map (\s -> { s | multiplayer = Nothing }) }, Cmd.none )

                SailsRoom.InRoom room ->
                    let
                        -- A joiner sails the host's map.
                        hostMap =
                            if room.role == Multiplayer.JoinedPlayer then
                                MapParams.fromCode room.courseId
                                    |> Maybe.andThen
                                        (\m ->
                                            if m /= model.course.map then
                                                Just m

                                            else
                                                Nothing
                                        )

                            else
                                Nothing
                    in
                    case ( model.sailModel, hostMap ) of
                        ( Just sModel, Just m ) ->
                            if Sail.waitingForRoom sModel then
                                let
                                    switched =
                                        withSelection (exactSelection m) model
                                in
                                requestParts { switched | sailModel = Just (newSail (Just room) switched) }

                            else
                                let
                                    ( nextSail, sCmd ) =
                                        Sail.update (Sail.UpdateMultiplayerRoom room) sModel
                                in
                                ( { model | sailModel = Just nextSail }, Cmd.map SailMessage sCmd )

                        ( Just sModel, Nothing ) ->
                            let
                                ( nextSail, sCmd ) =
                                    Sail.update (Sail.UpdateMultiplayerRoom room) sModel
                            in
                            ( { model | sailModel = Just nextSail }, Cmd.map SailMessage sCmd )

                        ( Nothing, _ ) ->
                            let
                                joined =
                                    case hostMap of
                                        Just m ->
                                            withSelection (exactSelection m) model

                                        Nothing ->
                                            model

                                ( refreshed, partsCmd ) =
                                    syncCourseProps model.course.map
                                        { joined | viewMode = ViewSailing, sailModel = Just (newSail (Just room) joined) }
                            in
                            ( refreshed
                            , Cmd.batch
                                [ partsCmd
                                , GamePorts.setUrlHash ("#room=" ++ room.roomId)
                                , GamePorts.calibrateTilt ()

                                -- Joined from a room link: share the selected ship.
                                , if room.role == Multiplayer.JoinedPlayer then
                                    GamePorts.setMultiplayerVehicle (sharedShip model)

                                  else
                                    Cmd.none
                                ]
                            )

                SailsRoom.Unreadable ->
                    ( model, Cmd.none )

        MultiplayerError err ->
            ( { model
                | loadError = Just err
                , sailModel = model.sailModel |> Maybe.map (\s -> { s | toast = Just ( err, 4 ) })
              }
            , Cmd.none
            )

        MultiplayerVehicleLoaded vehicle ->
            if Dict.member vehicle.id model.remoteShips then
                ( model, Cmd.none )

            else
                case Ingest.decodeSharedShip vehicle.payload of
                    Ingest.SharedBoat name ->
                        ( { model | remoteShips = Dict.insert vehicle.id (RemoteBoat name) model.remoteShips }, Cmd.none )

                    Ingest.SharedBuild decoded ->
                        requestParts
                            { model
                                | remoteShips = Dict.insert vehicle.id RemoteBuild model.remoteShips
                                , documents = Dict.insert (remoteKey vehicle.id) decoded.document model.documents
                            }

                    Ingest.SharedUnusable _ ->
                        ( { model | remoteShips = Dict.insert vehicle.id RemoteUnusable model.remoteShips }, Cmd.none )

        ReturnToShipyard ->
            ( { model | viewMode = ViewShipyard, sailModel = Nothing }
            , Cmd.batch [ GamePorts.leaveMultiplayerRoom (), silenceAudio ]
            )

        ToggleMute ->
            ( { model
                | audioMuted = not model.audioMuted
                , sailModel = model.sailModel |> Maybe.map (\s -> { s | audioMuted = not model.audioMuted })
              }
            , GamePorts.setAudioMuted (not model.audioMuted)
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
            let
                sized =
                    { model
                        | viewportWidth = dims.width
                        , viewportHeight = dims.height
                        , sailModel = model.sailModel |> Maybe.map (\s -> { s | viewportWidth = dims.width, viewportHeight = dims.height })
                    }
            in
            case model.builder of
                Just build ->
                    let
                        ( nextBuild, buildCmd ) =
                            Build.resize dims.width dims.height build
                    in
                    ( { sized | builder = Just nextBuild }, Cmd.map BuildMsg buildCmd )

                Nothing ->
                    ( sized, Cmd.none )

        StartBuild hullId ->
            case List.filter (\b -> b.id == hullId) Editor.bases |> List.head of
                Just base ->
                    openBuilder base base.document model

                Nothing ->
                    ( model, Cmd.none )

        EditShip ->
            case editableShip model of
                Just ( base, doc ) ->
                    openBuilder base doc model

                Nothing ->
                    ( model, Cmd.none )

        BuildMsg buildMsg ->
            case model.builder of
                Just build ->
                    let
                        ( nextBuild, buildCmd, outcome ) =
                            Build.update buildMsg build
                    in
                    case outcome of
                        Build.Stay ->
                            ( { model | builder = Just nextBuild }, Cmd.map BuildMsg buildCmd )

                        Build.Quit ->
                            ( { model | builder = Nothing }, Cmd.none )

                        Build.Finished doc ->
                            let
                                ( next, cmd ) =
                                    useShip (Ingest.validateShip doc |> Result.map (\spec -> ( { document = doc, title = hullName model.language nextBuild.base }, spec ))) model
                            in
                            case next.loadError of
                                Nothing ->
                                    ( { next | builder = Nothing }
                                    , Cmd.batch [ cmd, GamePorts.setUrlHash (Ingest.encodeMapHash next.course.map (Just doc)) ]
                                    )

                                Just _ ->
                                    -- Not a boat (cannot happen with a whole hull): stay in the editor.
                                    ( { model | builder = Just nextBuild }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SailMessage Sail.ReturnToShipyard ->
            update ReturnToShipyard model

        SailMessage Sail.ToggleMute ->
            update ToggleMute model

        SailMessage Sail.NewMap ->
            let
                sel =
                    model.selection

                ( next, _ ) =
                    update ReturnToShipyard (withSelection { sel | seed = nextRandomSeed sel.seed } model)

                ( refreshed, partsCmd ) =
                    syncCourseProps model.course.map next
            in
            ( refreshed
            , Cmd.batch [ partsCmd, GamePorts.leaveMultiplayerRoom (), silenceAudio, GamePorts.setUrlHash (mapLink refreshed) ]
            )

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
                        -- The ship's built cannons, so shots leave from
                        -- their muzzles; the mesh may arrive after the
                        -- voyage starts, so it is looked up on every message.
                        cannons =
                            Dict.get sModel.boat.name model.meshes
                                |> Maybe.map .cannons
                                |> Maybe.withDefault []

                        ( nextSail, sCmd ) =
                            Sail.update sMsg { sModel | cannons = cannons }

                        justFinished =
                            nextSail.progress.finished && not sModel.progress.finished
                    in
                    -- Only a solo run is a personal record: a room's
                    -- result is the crew's (see `Sails.Crew`).
                    if justFinished && nextSail.multiplayer == Nothing then
                        recordFinish nextSail { model | sailModel = Just nextSail } (Cmd.map SailMessage sCmd)

                    else
                        ( { model | sailModel = Just nextSail }, Cmd.map SailMessage sCmd )

                Nothing ->
                    ( model, Cmd.none )


{-| What this player shares as their ship in a room: a built-in boat by name,
a loaded build as a build link.
-}
sharedShip : Model -> String
sharedShip model =
    if model.boat.kind == CustomShip then
        Dict.get model.boat.name model.documents
            |> Maybe.map Ingest.encodeShipLink
            |> Maybe.withDefault Boats.default.name

    else
        model.boat.name


{-| Key of another player's shared build in `documents` and `meshes`.
-}
remoteKey : String -> String
remoteKey id =
    "vehicle:" ++ id


{-| Geometry of another player's ship by vehicle id, and whether it is still
on its way (the default boat is drawn meanwhile).
-}
remoteShipFor : Model -> String -> { mesh : Maybe ShipMesh, loading : Bool }
remoteShipFor model id =
    let
        fallback loading =
            { mesh = Dict.get Boats.default.name model.meshes, loading = loading }
    in
    if String.isEmpty id then
        fallback False

    else
        case Dict.get id model.remoteShips of
            Just (RemoteBoat name) ->
                { mesh = Dict.get name model.meshes, loading = False }

            Just RemoteBuild ->
                case Dict.get (remoteKey id) model.meshes of
                    Just mesh ->
                        { mesh = Just mesh, loading = False }

                    Nothing ->
                        fallback True

            Just RemoteUnusable ->
                fallback False

            Nothing ->
                fallback True


{-| Decode, check and add a ship from a build link, LDraw text, or bytes the
JS runtime already inflated. A valid ship becomes "Your ship" and is
selected; an invalid one leaves the boats as they were and shows why.
-}
loadShip : Maybe (List Int) -> String -> Model -> ( Model, Cmd Msg )
loadShip bytes input model =
    useShip
        (Ingest.decodeShipInput bytes input
            |> Result.andThen (\decoded -> Ingest.validateShip decoded.document |> Result.map (\spec -> ( decoded, spec )))
        )
        model


{-| Make a checked build "Your ship" and select it, or show why it is not a
ship.
-}
useShip : Result String ( Ingest.DecodedShip, Sails.Types.ShipSpec ) -> Model -> ( Model, Cmd Msg )
useShip checked model =
    case checked of
        Ok ( decoded, spec ) ->
            let
                ship =
                    Boats.custom decoded.title (Handling.fromSpec spec)
            in
            requestParts
                { model
                    | customShip = Just ship
                    , boat = ship
                    , documents = Dict.insert ship.name decoded.document model.documents
                    , meshes = Dict.remove ship.name model.meshes
                    , loadError = Nothing
                }

        Err err ->
            ( { model | loadError = Just err }, Cmd.none )


{-| Open the ship editor on a build standing on `base`.
-}
openBuilder : Editor.Base -> Document -> Model -> ( Model, Cmd Msg )
openBuilder base doc model =
    let
        ( build, buildCmd ) =
            Build.init
                { ldrawBase = model.ldrawBase
                , ldrawFallbackBase = model.ldrawFallbackBase
                , supportsPointerEvents = model.supportsPointerEvents
                , language = model.language
                , width = model.viewportWidth
                , height = model.viewportHeight
                }
                base
                doc
    in
    ( { model | builder = Just build, loadError = Nothing }, Cmd.map BuildMsg buildCmd )


{-| A hull's name in the player's language.
-}
hullName : Language -> Editor.Base -> String
hullName lang base =
    case lang of
        Fi ->
            base.nameFi

        En ->
            base.nameEn


{-| A hull's name on its shipyard button: without the word "hull".
-}
shortHullName : Language -> Editor.Base -> String
shortHullName lang base =
    hullName lang base
        |> String.replace " Hull" ""
        |> String.replace " runko" ""


{-| "Your ship", when it is selected and stands on a starter hull, so the
editor can open it again.
-}
editableShip : Model -> Maybe ( Editor.Base, Document )
editableShip model =
    if model.boat.kind == CustomShip then
        Dict.get model.boat.name model.documents
            |> Maybe.andThen (\doc -> Editor.baseOf doc |> Maybe.map (\base -> ( base, doc )))

    else
        Nothing


{-| Fetch every part a boat, the island scenery or a course prop still needs,
and build the geometry of each model and prop whose parts are all in. Called at
start-up and after every fetched part.
-}
requestParts : Model -> ( Model, Cmd Msg )
requestParts model =
    let
        progress =
            model.documents
                |> Dict.filter (\name _ -> not (Dict.member name model.meshes))
                |> Dict.map (\_ doc -> ShipMesh.partProgress doc model.partCache)

        needed =
            Dict.values progress
                |> List.concatMap .needed
                |> PartLoading.uniqueNames

        ready name p =
            List.isEmpty p.needed && p.inFlight == 0 && not (Dict.member name model.meshes)

        built =
            Dict.foldl
                (\name p acc ->
                    if ready name p then
                        case Dict.get name model.documents of
                            Just doc ->
                                Dict.insert name (buildMesh name doc model) acc

                            Nothing ->
                                acc

                    else
                        acc
                )
                model.meshes
                progress

        scenery =
            BrickProps.refreshWith Render.propDetailOptions (Render.propSources model.course.map) (Resolve.markLoading needed model.partCache) (Dict.filter (\k _ -> not (String.startsWith "island-" k)) model.props)
    in
    ( { model
        | partCache = scenery.cache
        , meshes = built
        , props = scenery.props
      }
    , PartLoading.fetchParts model.resolver PartFetched (needed ++ scenery.fetch)
    )


{-| Geometry of a boat or a piece of island scenery.
-}
buildMesh : String -> Document -> Model -> ShipMesh
buildMesh name doc model =
    if Scenery.isScenery name then
        ShipMesh.buildScenery Scenery.metersPerLdu doc model.partCache

    else
        ShipMesh.build (sailRule name model) doc model.partCache


sailRule : String -> Model -> Document.Placement -> Bool
sailRule name model =
    if String.startsWith "vehicle:" name then
        -- Another player's build: sails are found like on "Your ship".
        ShipMesh.sailByPartName Ingest.isSailPart

    else if NpcShips.isNpcShip name then
        -- The computer's frigates and sloops: their white bricks are sails.
        ShipMesh.sailByColour NpcShips.sailColour

    else
        sailRuleForBoat name model


sailRuleForBoat : String -> Model -> Document.Placement -> Bool
sailRuleForBoat name model =
    let
        boat =
            case model.customShip of
                Just ship ->
                    if ship.name == name then
                        ship

                    else
                        Boats.byName name

                Nothing ->
                    Boats.byName name
    in
    case boat.sailColour of
        Just code ->
            ShipMesh.sailByColour code

        Nothing ->
            ShipMesh.sailByPartName Ingest.isSailPart


{-| Brick loading progress of a boat, while it is not built yet.
-}
loadingOf : Model -> Boat -> Maybe { loaded : Int, total : Int }
loadingOf model boat =
    if Dict.member boat.name model.meshes then
        Nothing

    else
        Dict.get boat.name model.documents
            |> Maybe.map
                (\doc ->
                    let
                        p =
                            ShipMesh.partProgress doc model.partCache
                    in
                    { loaded = p.settled, total = p.total }
                )


{-| Save the finished run to the local leaderboard.
-}
recordFinish : Sail.SailModel -> Model -> Cmd Msg -> ( Model, Cmd Msg )
recordFinish sail model cmd =
    let
        board =
            Records.recordRun
                { courseId = model.course.id
                , player = "You"
                , coins = sail.progress.coins
                , timeSeconds = sail.progress.elapsedSeconds
                , isoDate = ""
                , vehicleName = sail.boat.name
                }
                model.leaderboard
    in
    ( { model | leaderboard = Records.tidy board, lastMap = Just model.course.map }
    , Cmd.batch [ cmd, GamePorts.saveLeaderboard (Records.toJson (Records.tidy board)) ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ GamePorts.viewportResized ViewportResized
        , GamePorts.multiplayerRoomStateChanged MultiplayerRoomChanged
        , GamePorts.multiplayerError MultiplayerError
        , GamePorts.multiplayerVehicleLoaded MultiplayerVehicleLoaded
        , GamePorts.tiltStatus TiltStatusChanged
        , GamePorts.hashChanged HashChanged
        , case ( model.builder, model.viewMode, model.sailModel ) of
            ( Just build, _, _ ) ->
                Sub.map BuildMsg (Build.subscriptions build)

            ( Nothing, ViewSailing, Just sModel ) ->
                Sub.map SailMessage (Sail.subscriptions sModel)

            _ ->
                Browser.Events.onAnimationFrameDelta ShipyardTick
        ]


view : Model -> Html Msg
view model =
    case ( model.builder, model.viewMode, model.sailModel ) of
        ( Just build, _, _ ) ->
            Html.map BuildMsg (Build.view build)

        ( Nothing, ViewSailing, Just sModel ) ->
            Html.map SailMessage
                (Sail.viewSail sModel
                    { onReturnToShipyard = Sail.ReturnToShipyard
                    , onRestartCourse = Sail.RestartCourse
                    , onNewMap = Sail.NewMap
                    , meshFor = \name -> Dict.get name model.meshes
                    , remoteShip = remoteShipFor model
                    , props = model.props
                    }
                )

        _ ->
            Shipyard.viewShipyard
                { boat = model.boat
                , mesh = Dict.get model.boat.name model.meshes
                , loading = loadingOf model model.boat
                , loadError = model.loadError
                , course = model.course
                , personalBest = personalBest model
                , time = model.shipyardTime
                , audioMuted = model.audioMuted
                , viewportWidth = model.viewportWidth
                , viewportHeight = model.viewportHeight
                , roomCode = model.roomCodeInput
                , language = model.language
                , hulls = List.map (\b -> { id = b.id, name = shortHullName model.language b }) Editor.bases
                , canEditShip = editableShip model /= Nothing
                , selection = model.selection
                , seedText = model.seedText
                , seedEditing = model.seedEditing
                , menuTab = model.menuTab
                , actions =
                    { onStartSailing = StartSailing
                    , onStartHostMultiplayer = StartHostMultiplayer
                    , onJoinMultiplayer = JoinMultiplayer
                    , onRoomCodeChanged = RoomCodeChanged
                    , onToggleMute = ToggleMute
                    , onStartBuild = StartBuild
                    , onEditShip = EditShip
                    , onPickBiome = PickBiome
                    , onPickSeason = PickSeason
                    , onPickWeather = PickWeather
                    , onSelectTab = SelectMenuTab
                    , onEditSeed = EditSeed
                    , onSeedInput = SeedTyped
                    , onCommitSeed = CommitSeed
                    , onCopyMapLink = CopyMapLink
                    , onKeepFocus = NoOp
                    , onRollWorld = RollWorld
                    , onUsePreviousMap =
                        if model.lastMap /= Nothing && model.lastMap /= Just model.course.map then
                            Just UsePreviousMap

                        else
                            Nothing
                    }
                }
