module Pages.Shipyard exposing
    ( MenuTab(..)
    , ShipyardActions
    , seedInputId
    , viewShipyard
    )

{-| Start screen: the shared app header (title and mute), a framed preview of
the boat bobbing on the water with its name and loading state, the two main
buttons (build or edit your own ship, set sail), a track randomization section
(a dice button that rolls a new world and three icons for biome, season and
weather that each step through their values on click, with the right-aligned
map code + seed and copy button below), and online play as icon buttons (host a
room of two or eight, or join one by code). Every player sails on their own
screen.
-}

import Component.Navbar as Navbar
import DesignTokens.Colors as Brand
import FeatherIcons
import Game.MapParams as MapParams exposing (Biome, Pick(..), Season, Selection, WeatherKind)
import Game.Multiplayer as Multiplayer exposing (SessionMode(..))
import Html exposing (Html, button, div, input, p, section, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onBlur, onClick, onInput)
import I18n exposing (Language)
import Json.Decode as Decode
import Sails.I18n as T
import Sails.MenuIcons as MenuIcons
import Sails.Render as Render
import Sails.ShipMesh exposing (ShipMesh)
import Sails.Types exposing (Boat, Course, LeaderboardEntry)


{-| The two panels of the menu card: the course, or the world it is on.
-}
type MenuTab
    = CourseTab
    | WorldTab


type alias ShipyardActions msg =
    { onStartSailing : msg
    , onStartHostMultiplayer : SessionMode -> msg
    , onJoinMultiplayer : String -> msg
    , onRoomCodeChanged : String -> msg
    , onToggleMute : msg
    , onStartBuild : String -> msg
    , onEditShip : msg
    , onPickBiome : Pick Biome -> msg
    , onPickSeason : Pick Season -> msg
    , onPickWeather : Pick WeatherKind -> msg
    , onSelectTab : MenuTab -> msg
    , onEditSeed : msg
    , onSeedInput : String -> msg
    , onCommitSeed : msg
    , onCopyMapLink : msg
    , onKeepFocus : msg
    , onRollWorld : msg
    , onUsePreviousMap : Maybe msg
    }


{-| Id of the seed field, focused when it opens.
-}
seedInputId : String
seedInputId =
    "sails-seed-input"


{-| Loading state of the selected boat's bricks.
-}
type alias Loading =
    { loaded : Int, total : Int }


viewShipyard :
    { boat : Boat
    , mesh : Maybe ShipMesh
    , loading : Maybe Loading
    , loadError : Maybe String
    , course : Course
    , personalBest : Maybe LeaderboardEntry
    , time : Float
    , audioMuted : Bool
    , viewportWidth : Int
    , viewportHeight : Int
    , roomCode : String
    , language : Language
    , hulls : List { id : String, name : String }
    , canEditShip : Bool
    , selection : Selection
    , seedText : String
    , seedEditing : Bool
    , menuTab : MenuTab
    , actions : ShipyardActions msg
    }
    -> Html msg
viewShipyard cfg =
    div
        [ Attr.class "fixed inset-0 overflow-y-auto overflow-x-hidden text-white font-sans select-none"
        , Attr.style "touch-action" "pan-y"
        , Attr.style "background" ("linear-gradient(170deg, " ++ Brand.legoBlack ++ " 0%, #08263a 50%, " ++ Brand.legoBlack ++ " 100%)")
        , Attr.attribute "data-testid" "shipyard-page"
        ]
        [ viewHeader cfg
        , div
            [ Attr.class "w-full flex flex-col items-center px-4 py-4"
            , Attr.style "padding-left" "max(16px, env(safe-area-inset-left))"
            , Attr.style "padding-right" "max(16px, env(safe-area-inset-right))"
            , Attr.style "padding-bottom" "max(16px, env(safe-area-inset-bottom))"
            ]
            [ div [ Attr.class "w-full max-w-5xl flex flex-col gap-5", Attr.attribute "data-testid" "main-content" ]
                [ viewHero cfg
                , viewActionButtons cfg
                , case cfg.loadError of
                    Just err ->
                        p [ Attr.attribute "role" "alert", Attr.class "-mt-2 text-[11px] text-rose-300 font-bold text-center" ] [ text (T.shipLoadError cfg.language err) ]

                    Nothing ->
                        text ""
                , div
                    [ Attr.attribute "data-testid" "options-row"
                    , Attr.class "w-full grid grid-cols-1 md:grid-cols-2 gap-4 items-stretch"
                    ]
                    [ viewTrackSection cfg
                    , viewCrew cfg.language cfg.roomCode cfg.actions
                    ]
                ]
            ]
        ]


{-| The shared app header (as Brick Racer's): the title, then the mute
button.
-}
viewHeader : { a | audioMuted : Bool, language : Language, actions : ShipyardActions msg } -> Html msg
viewHeader { audioMuted, language, actions } =
    div
        [ Attr.class "bg-brand"
        , Attr.attribute "data-testid" "header-navbar"
        , Attr.style "padding-top" "env(safe-area-inset-top)"
        , Attr.style "padding-left" "env(safe-area-inset-left)"
        , Attr.style "padding-right" "env(safe-area-inset-right)"
        ]
        [ Navbar.viewAppHeader
            { brand =
                Navbar.viewBrand
                    { href = "#"
                    , iconSrc = "/favicon.svg"
                    , title = T.title language
                    , onClick = Nothing
                    }
            , navLabel = T.title language
            , links = []
            , trailing =
                [ div [ Attr.class "flex gap-1.5" ]
                    [ button
                        [ Attr.type_ "button"
                        , onClick actions.onToggleMute
                        , Attr.title (T.muteLabel language audioMuted)
                        , Attr.attribute "aria-label" (T.muteLabel language audioMuted)
                        , Attr.class "w-11 h-11 flex items-center justify-center rounded-lg text-white cursor-pointer hover:bg-brand-yellow hover:text-brand"
                        ]
                        [ (if audioMuted then
                            FeatherIcons.volumeX

                           else
                            FeatherIcons.volume2
                          )
                            |> FeatherIcons.withSize 20
                            |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                        ]
                    ]
                ]
            , menuToggle = Nothing
            , sticky = False
            }
        ]


{-| The framed preview of the selected boat on the water of the map's sea,
with its name and loading state at the top left and its speed and turning
bars at the bottom left.
-}
viewHero :
    { a
        | boat : Boat
        , mesh : Maybe ShipMesh
        , loading : Maybe Loading
        , course : Course
        , time : Float
        , viewportWidth : Int
        , viewportHeight : Int
        , language : Language
    }
    -> Html msg
viewHero { boat, mesh, loading, course, time, viewportWidth, viewportHeight, language } =
    let
        heroWidth =
            clamp 288 1024 (viewportWidth - 32)

        heroHeight =
            clamp 240 500 (round (toFloat viewportHeight * 0.46))
    in
    div
        [ Attr.class "relative w-full shrink-0 rounded-2xl overflow-hidden border border-white/10 bg-brand"
        , Attr.style "height" (String.fromInt heroHeight ++ "px")
        , Attr.attribute "data-testid" "hero-preview"
        ]
        [ Render.renderShipyard course.map mesh time heroWidth heroHeight
        , div [ Attr.class "absolute top-3 left-3 flex items-center gap-2 pointer-events-none" ]
            [ span [ Attr.class "px-3 py-1.5 rounded-xl bg-brand/90 border border-white/20 text-xs font-bold" ]
                [ text (T.boatName language boat.name) ]
            , case ( mesh, loading ) of
                ( Nothing, Just l ) ->
                    span [ Attr.class "px-3 py-1.5 rounded-xl bg-sky-600/90 text-xs font-semibold" ]
                        [ text (T.loadingBricks language l.loaded (max l.loaded l.total)) ]

                _ ->
                    text ""
            ]
        , div [ Attr.class "absolute bottom-2 left-2 flex items-center gap-3 px-2.5 py-1.5 rounded-lg bg-brand/70 text-[11px] pointer-events-none" ]
            [ statBar FeatherIcons.fastForward (T.speedLabel language) ((boat.topSpeed - 7) / 3.2)
            , statBar FeatherIcons.rotateCw (T.turnLabel language) ((boat.turnRate - 50) / 80)
            ]
        ]


{-| The two main buttons under the preview: open the ship editor (edit
"Your ship" when it stands on a starter hull, else build on the first hull),
and set sail.
-}
viewActionButtons :
    { a
        | language : Language
        , hulls : List { id : String, name : String }
        , canEditShip : Bool
        , actions : ShipyardActions msg
    }
    -> Html msg
viewActionButtons { language, hulls, canEditShip, actions } =
    let
        builderButton =
            if canEditShip then
                Just ( actions.onEditShip, T.editShipButton language )

            else
                List.head hulls |> Maybe.map (\hull -> ( actions.onStartBuild hull.id, T.buildOwnShip language ))
    in
    div [ Attr.class "flex flex-col sm:flex-row gap-3 w-full", Attr.attribute "data-testid" "action-buttons" ]
        [ case builderButton of
            Just ( msg, label ) ->
                button
                    [ Attr.type_ "button"
                    , onClick msg
                    , Attr.title label
                    , Attr.attribute "aria-label" label
                    , Attr.attribute "data-testid" "build-action-button"
                    , Attr.class "flex-1 min-h-[52px] flex items-center justify-center gap-2.5 py-3.5 px-6 rounded-xl bg-white/10 border border-white/25 text-white font-extrabold text-base cursor-pointer transition-all active:bg-white/20"
                    ]
                    [ FeatherIcons.tool |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml []
                    , span [] [ text label ]
                    ]

            Nothing ->
                text ""
        , button
            [ Attr.type_ "button"
            , onClick actions.onStartSailing
            , Attr.title (T.setSail language)
            , Attr.attribute "aria-label" (T.setSail language)
            , Attr.attribute "data-tilt-start" ""
            , Attr.attribute "data-testid" "sail-action-button"
            , Attr.class "flex-1 min-h-[52px] flex items-center justify-center gap-2.5 py-3.5 px-6 rounded-xl bg-brand-yellow text-brand font-extrabold text-base uppercase tracking-widest cursor-pointer transition-all shadow-lg shadow-brand-yellow/30 active:brightness-110"
            ]
            [ FeatherIcons.play |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml []
            , span [] [ text (T.setSail language) ]
            ]
        ]


{-| Track randomization: a dice icon to randomize track, visualized with three
icons representing the feature axes (biome, season, weather) that cycle when clicked.
Below the icon row is the right-aligned code + seed without explanation and a small
icon to copy it to clipboard. Clicking the code + seed enables inline editing.
-}
viewTrackSection :
    { a
        | language : Language
        , selection : Selection
        , seedText : String
        , seedEditing : Bool
        , actions : ShipyardActions msg
    }
    -> Html msg
viewTrackSection cfg =
    let
        labels =
            T.mapPickerLabels cfg.language

        world =
            MapParams.resolve cfg.selection

        codeText =
            MapParams.toCode world

        labelText =
            case cfg.language of
                I18n.Fi ->
                    "Maailma"

                I18n.En ->
                    "World"
    in
    div
        [ Attr.class "flex flex-row items-stretch gap-3 p-3 rounded-2xl bg-[#0d2235]/95 border border-white/10 w-full h-full"
        , Attr.attribute "data-testid" "track-section"
        ]
        [ div
            [ Attr.class "flex items-center justify-center select-none shrink-0 pr-3 border-r border-white/10"
            , Attr.attribute "aria-hidden" "true"
            ]
            [ span
                [ Attr.class "text-xs font-black uppercase tracking-widest text-brand-yellow"
                , Attr.style "writing-mode" "vertical-rl"
                , Attr.style "transform" "rotate(180deg)"
                ]
                [ text labelText ]
            ]
        , div
            [ Attr.class "flex-1 flex flex-col justify-between gap-2 min-w-0" ]
            [ div [ Attr.class "flex flex-wrap items-center justify-center sm:justify-start gap-3", Attr.attribute "data-testid" "track-axes-row" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick cfg.actions.onRollWorld
                    , Attr.title (T.rollWorld cfg.language)
                    , Attr.attribute "aria-label" (T.rollWorld cfg.language)
                    , Attr.attribute "data-testid" "track-dice-button"
                    , Attr.class "w-12 h-12 flex items-center justify-center rounded-xl bg-brand-yellow text-brand cursor-pointer active:brightness-110 shadow-md transition-all"
                    ]
                    [ MenuIcons.dice |> FeatherIcons.withSize 26 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]
                , span [ Attr.class "w-px h-8 bg-white/15 mx-1", Attr.attribute "aria-hidden" "true" ] []
                , worldButton cfg.language
                    { label = labels.biome
                    , valueName = labels.biomeName world.biome
                    , pick = cfg.selection.biome
                    , icon = MenuIcons.biome world.biome
                    , onPick = cfg.actions.onPickBiome (cycle MapParams.allBiomes cfg.selection.biome world.biome)
                    }
                , worldButton cfg.language
                    { label = labels.season
                    , valueName = labels.seasonName world.season
                    , pick = cfg.selection.season
                    , icon = MenuIcons.season world.season
                    , onPick = cfg.actions.onPickSeason (cycle MapParams.allSeasons cfg.selection.season world.season)
                    }
                , worldButton cfg.language
                    { label = labels.weather
                    , valueName = labels.weatherName world.weather
                    , pick = cfg.selection.weather
                    , icon = MenuIcons.weather world.weather
                    , onPick = cfg.actions.onPickWeather (cycle (MapParams.allowedWeathers world.biome world.season) cfg.selection.weather world.weather)
                    }
                ]
            , div [ Attr.class "flex items-center justify-end gap-1.5 pt-1", Attr.attribute "data-testid" "track-seed-row" ]
                [ if cfg.seedEditing then
                    input
                        [ Attr.type_ "text"
                        , Attr.id seedInputId
                        , Attr.value cfg.seedText
                        , onInput cfg.actions.onSeedInput
                        , onBlur cfg.actions.onCommitSeed
                        , Html.Events.on "keydown" (enterKey cfg.actions.onCommitSeed)
                        , Attr.attribute "aria-label" labels.seed
                        , Attr.attribute "autocomplete" "off"
                        , Attr.attribute "inputmode" "text"
                        , Attr.attribute "data-testid" "track-seed-input"
                        , Attr.class "min-w-[14rem] max-w-full min-h-[32px] px-2.5 rounded-md bg-slate-900/80 border border-slate-400/30 text-xs font-mono text-white text-right"
                        ]
                        []

                  else
                    button
                        [ Attr.type_ "button"
                        , onClick cfg.actions.onEditSeed
                        , Attr.title (T.editSeed cfg.language)
                        , Attr.attribute "aria-label" (T.editSeed cfg.language)
                        , Attr.attribute "data-testid" "track-seed-display"
                        , Attr.class "min-h-[32px] px-2 rounded-md text-xs font-mono text-slate-400 cursor-pointer hover:text-white transition-colors"
                        ]
                        [ text codeText ]
                , button
                    [ Attr.type_ "button"
                    , onClick cfg.actions.onCopyMapLink
                    , Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( cfg.actions.onKeepFocus, True ))
                    , Attr.title (T.copyLink cfg.language)
                    , Attr.attribute "aria-label" (T.copyLink cfg.language)
                    , Attr.attribute "data-testid" "track-seed-copy"
                    , Attr.class "w-8 h-8 flex items-center justify-center rounded-md text-slate-400 cursor-pointer hover:text-white transition-colors"
                    ]
                    [ FeatherIcons.copy |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]
                ]
            ]
        ]


{-| A world setting's button: the icon of the value the map has, marked when
it is chosen rather than left to chance. A press steps to the next value.
-}
worldButton :
    Language
    ->
        { label : String
        , valueName : String
        , pick : Pick a
        , icon : FeatherIcons.Icon
        , onPick : msg
        }
    -> Html msg
worldButton language { label, valueName, pick, icon, onPick } =
    let
        ( description, look ) =
            case pick of
                Any ->
                    ( label ++ ": " ++ valueName ++ ", " ++ T.byChance language
                    , "bg-white/5 border-white/15 border-dashed text-white"
                    )

                Exactly _ ->
                    ( label ++ ": " ++ valueName
                    , "bg-brand-yellow/20 border-brand-yellow/60 text-brand-yellow"
                    )
    in
    button
        [ Attr.type_ "button"
        , onClick onPick
        , Attr.title description
        , Attr.attribute "aria-label" description
        , Attr.class ("w-12 h-12 flex items-center justify-center rounded-xl border cursor-pointer transition-all " ++ look)
        ]
        [ icon |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]


{-| The next pick of a world setting: steps from the currently shown value to
the next value in the list, wrapping around to the first.
-}
cycle : List a -> Pick a -> a -> Pick a
cycle values _ shown =
    case dropUntil shown values of
        _ :: next :: _ ->
            Exactly next

        _ ->
            List.head values |> Maybe.map Exactly |> Maybe.withDefault Any


dropUntil : a -> List a -> List a
dropUntil value values =
    case values of
        [] ->
            []

        first :: rest ->
            if first == value then
                values

            else
                dropUntil value rest


enterKey : msg -> Decode.Decoder msg
enterKey msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "not Enter"
            )


{-| Online play as icon buttons: host a room of two or of eight, or type a
room code (or paste a link) and join it.
-}
viewCrew : Language -> String -> ShipyardActions msg -> Html msg
viewCrew language roomCode actions =
    let
        labelText =
            case language of
                I18n.Fi ->
                    "Miehistö"

                I18n.En ->
                    "Crew"
    in
    section
        [ Attr.attribute "aria-label" (T.crewHeading language)
        , Attr.attribute "data-testid" "crew-section"
        , Attr.class "flex flex-row items-stretch gap-3 p-3 rounded-2xl bg-[#0d2235]/95 border border-white/10 w-full h-full"
        ]
        [ div
            [ Attr.class "flex items-center justify-center select-none shrink-0 pr-3 border-r border-white/10"
            , Attr.attribute "aria-hidden" "true"
            ]
            [ span
                [ Attr.class "text-xs font-black uppercase tracking-widest text-brand-yellow"
                , Attr.style "writing-mode" "vertical-rl"
                , Attr.style "transform" "rotate(180deg)"
                ]
                [ text labelText ]
            ]
        , div
            [ Attr.class "flex-1 flex flex-col justify-between gap-2 min-w-0" ]
            [ div [ Attr.class "flex flex-wrap items-center gap-2", Attr.attribute "data-testid" "crew-controls-row" ]
                [ hostButton language actions TwoPlayers FeatherIcons.users
                , hostButton language actions GridSession FeatherIcons.tv
                , span [ Attr.class "w-px h-8 bg-white/15 mx-1", Attr.attribute "aria-hidden" "true" ] []
                , div [ Attr.class "flex flex-1 items-center gap-2 min-w-[180px]", Attr.attribute "data-testid" "crew-join-row" ]
                    [ input
                        [ Attr.type_ "text"
                        , Attr.value roomCode
                        , onInput actions.onRoomCodeChanged
                        , Attr.placeholder (T.roomCodePlaceholder language)
                        , Attr.attribute "aria-label" (T.roomCodeLabel language)
                        , Attr.attribute "autocomplete" "off"
                        , Attr.attribute "data-testid" "crew-room-input"
                        , Attr.class "flex-1 min-w-0 min-h-[44px] px-3 rounded-lg bg-slate-900/80 border border-slate-400/30 text-xs font-mono text-white placeholder:text-slate-500"
                        ]
                        []
                    , iconButton (actions.onJoinMultiplayer roomCode) (T.joinRace language) FeatherIcons.logIn
                    ]
                ]
            , viewCrewStatusRow language roomCode
            ]
        ]


{-| A written description of the chosen gameplay combination under the crew controls.
-}
viewCrewStatusRow : Language -> String -> Html msg
viewCrewStatusRow language roomCode =
    let
        desc =
            if not (String.isEmpty (String.trim roomCode)) then
                case language of
                    I18n.Fi ->
                        "Liittymässä huoneeseen: " ++ String.trim roomCode

                    I18n.En ->
                        "Joining room: " ++ String.trim roomCode

            else
                case language of
                    I18n.Fi ->
                        "Yksinpurjehdus (1 pelaaja) • Avaa huone moninpelille"

                    I18n.En ->
                        "Solo voyage (1 player) • Host room for multiplayer"
    in
    div
        [ Attr.class "flex items-center justify-end gap-1.5 pt-1 w-full"
        , Attr.attribute "data-testid" "crew-status-row"
        ]
        [ span
            [ Attr.class "min-h-[32px] px-2 flex items-center text-xs font-medium text-slate-400"
            ]
            [ text desc ]
        ]


{-| A host button for one session mode (a 2-player room or a grid session).
-}
hostButton : Language -> ShipyardActions msg -> SessionMode -> FeatherIcons.Icon -> Html msg
hostButton language actions mode icon =
    let
        label =
            T.hostTitle language (Multiplayer.sessionCapacity mode)
    in
    button
        [ Attr.type_ "button"
        , onClick (actions.onStartHostMultiplayer mode)
        , Attr.title label
        , Attr.attribute "aria-label" label
        , Attr.class "w-12 h-12 flex items-center justify-center rounded-xl bg-brand-yellow/20 border border-brand-yellow/40 text-brand-yellow cursor-pointer transition-all active:bg-brand-yellow/30"
        ]
        [ icon |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]


{-| A 44 px yellow icon-only button with its label for screen readers and as
its tooltip.
-}
iconButton : msg -> String -> FeatherIcons.Icon -> Html msg
iconButton msg label icon =
    button
        [ Attr.type_ "button"
        , onClick msg
        , Attr.title label
        , Attr.attribute "aria-label" label
        , Attr.class "w-11 h-11 shrink-0 flex items-center justify-center rounded-lg bg-brand-yellow text-brand cursor-pointer active:brightness-110"
        ]
        [ icon |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ] ]


{-| A small bar of a boat's stat, with its icon; the stat's name is its
tooltip.
-}
statBar : FeatherIcons.Icon -> String -> Float -> Html msg
statBar icon label v =
    div
        [ Attr.attribute "role" "img"
        , Attr.attribute "aria-label" label
        , Attr.title label
        , Attr.class "flex items-center gap-1 w-16 shrink-0 text-slate-300"
        ]
        [ icon |> FeatherIcons.withSize 12 |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
        , div [ Attr.class "flex-1 h-1.5 rounded-full bg-slate-700 overflow-hidden" ]
            [ div [ Attr.class "h-full bg-sky-400 rounded-full", Attr.style "width" (String.fromInt (round (clamp 0 1 v * 100)) ++ "%") ] [] ]
        ]
