module Pages.Shipyard exposing
    ( ShipyardActions
    , viewShipyard
    )

{-| Start screen, with the options of Brick Racer's setup screen in the same
order: the shared app header (title and mute), a framed preview of the boat
bobbing on the water with its name, loading state, description and stats,
the two main buttons (open the ship editor, set sail), the map with its
best time and the map picker (`Game.MapPicker`: biome, season, weather and
seed, each left to chance or chosen, and the previous map), and online play (host a room of two or eight, or join one).
Every player sails on their own screen.
-}

import Component.Navbar as Navbar
import DesignTokens.Colors as Brand
import FeatherIcons
import Game.Leaderboard as Leaderboard
import Game.MapParams exposing (Biome, Pick, Season, Selection, WeatherKind)
import Game.MapPicker as MapPicker
import Game.Multiplayer as Multiplayer exposing (SessionMode(..))
import Html exposing (Html, button, div, h2, input, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import I18n exposing (Language)
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.Render as Render
import Sails.ShipMesh exposing (ShipMesh)
import Sails.Types exposing (Boat, Course, LeaderboardEntry)


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
    , onChangeSeed : String -> msg
    , onRandomSeed : msg
    , onUsePreviousMap : Maybe msg
    }


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
    , tilt : { available : Bool, enabled : Bool }
    , viewportWidth : Int
    , viewportHeight : Int
    , roomCode : String
    , language : Language
    , hulls : List { id : String, name : String }
    , canEditShip : Bool
    , selection : Selection
    , seedText : String
    , actions : ShipyardActions msg
    }
    -> Html msg
viewShipyard cfg =
    div
        [ Attr.class "fixed inset-0 overflow-y-auto overflow-x-hidden text-white font-sans select-none"
        , Attr.style "touch-action" "pan-y"
        , Attr.style "background" ("linear-gradient(170deg, " ++ Brand.legoBlack ++ " 0%, #08263a 50%, " ++ Brand.legoBlack ++ " 100%)")
        ]
        [ viewHeader cfg
        , div
            [ Attr.class "w-full flex flex-col items-center px-4 py-4"
            , Attr.style "padding-left" "max(16px, env(safe-area-inset-left))"
            , Attr.style "padding-right" "max(16px, env(safe-area-inset-right))"
            , Attr.style "padding-bottom" "max(16px, env(safe-area-inset-bottom))"
            ]
            [ div [ Attr.class "w-full max-w-5xl flex flex-col gap-5" ]
                [ viewHero cfg
                , viewActionButtons cfg
                , p [ Attr.class "-mt-2 text-[11px] text-slate-400 text-center" ] [ text (T.controlsHint cfg.language cfg.tilt.enabled) ]
                , case cfg.loadError of
                    Just err ->
                        p [ Attr.attribute "role" "alert", Attr.class "-mt-2 text-[11px] text-rose-300 font-bold text-center" ] [ text (T.shipLoadError cfg.language err) ]

                    Nothing ->
                        text ""
                , viewCourse cfg
                , viewCrew cfg.language cfg.roomCode cfg.actions
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


{-| The framed preview of the selected boat on the water, with its name and
loading state at the top left, its stats at the bottom left and its
description at the bottom right.
-}
viewHero :
    { a
        | boat : Boat
        , mesh : Maybe ShipMesh
        , loading : Maybe Loading
        , time : Float
        , viewportWidth : Int
        , viewportHeight : Int
        , language : Language
    }
    -> Html msg
viewHero { boat, mesh, loading, time, viewportWidth, viewportHeight, language } =
    let
        heroWidth =
            clamp 288 1024 (viewportWidth - 32)

        heroHeight =
            clamp 240 500 (round (toFloat viewportHeight * 0.46))
    in
    div
        [ Attr.class "relative w-full shrink-0 rounded-2xl overflow-hidden border border-white/10 bg-brand"
        , Attr.style "height" (String.fromInt heroHeight ++ "px")
        ]
        [ Render.renderShipyard mesh time heroWidth heroHeight
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
            [ statBar (T.speedLabel language) ((boat.topSpeed - 7) / 3.2)
            , statBar (T.turnLabel language) ((boat.turnRate - 50) / 80)
            ]
        , span [ Attr.class "absolute bottom-2 right-2 max-w-[50%] px-2.5 py-1 rounded-lg bg-brand/70 text-xs text-slate-300 text-right pointer-events-none" ]
            [ text (T.boatDescription language boat.description) ]
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
    div [ Attr.class "flex flex-col sm:flex-row gap-3 w-full" ]
        [ case builderButton of
            Just ( msg, label ) ->
                button
                    [ Attr.type_ "button"
                    , onClick msg
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
            , Attr.attribute "data-tilt-start" ""
            , Attr.class "flex-1 min-h-[52px] flex items-center justify-center gap-2.5 py-3.5 px-6 rounded-xl bg-brand-yellow text-brand font-extrabold text-base uppercase tracking-widest cursor-pointer transition-all shadow-lg shadow-brand-yellow/30 active:brightness-110"
            ]
            [ FeatherIcons.play |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml []
            , span [] [ text (T.setSail language) ]
            ]
        ]


{-| The course card: the map's name and the best time on it with its stars,
or the three-star goal while there is no best time yet, then the map
picker.
-}
viewCourse :
    { a
        | course : Course
        , personalBest : Maybe LeaderboardEntry
        , language : Language
        , selection : Selection
        , seedText : String
        , actions : ShipyardActions msg
    }
    -> Html msg
viewCourse { course, personalBest, language, selection, seedText, actions } =
    div [ Attr.class "flex flex-col gap-3 p-3.5 rounded-xl bg-[#0d2235]/95 border-2 border-brand-yellow" ]
        [ div [ Attr.class "flex flex-wrap items-center justify-between gap-2" ] (courseLine course personalBest language)
        , div [ Attr.class "sails-map-picker" ]
            [ MapPicker.view
                { labels = T.mapPickerLabels language
                , selection = selection
                , seedText = seedText
                , onBiome = actions.onPickBiome
                , onSeason = actions.onPickSeason
                , onWeather = actions.onPickWeather
                , onSeedInput = actions.onChangeSeed
                , onRandomSeed = actions.onRandomSeed
                , onPrevious = actions.onUsePreviousMap
                }
            ]
        ]


{-| The map's name and the best time on it.
-}
courseLine : Course -> Maybe LeaderboardEntry -> Language -> List (Html msg)
courseLine course personalBest language =
    let
        ( bestLine, hasBest ) =
            case personalBest of
                Just pb ->
                    ( T.personalBestLine language (Leaderboard.formatRaceTime pb.timeSeconds) (Physics.starRating course.targetSeconds pb.timeSeconds), True )

                Nothing ->
                    ( T.bestLine language (Leaderboard.formatRaceTime course.targetSeconds), False )
    in
    [ span [ Attr.class "flex items-center gap-2 text-sm font-bold text-brand-yellow" ]
        [ FeatherIcons.map |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
        , text (T.mapTitle language course.map)
        ]
    , span
        [ Attr.class
            ("text-xs px-1.5 py-0.5 rounded font-mono bg-white/10 "
                ++ (if hasBest then
                        "text-brand-yellow"

                    else
                        "text-slate-400"
                   )
            )
        ]
        [ text ("⏱ " ++ bestLine) ]
    ]


{-| Online play, as in Brick Racer: a card for each room size with its own
host button, and a card to join a room by its code or link.
-}
viewCrew : Language -> String -> ShipyardActions msg -> Html msg
viewCrew language roomCode actions =
    div [ Attr.class "flex flex-col gap-3" ]
        [ sectionHeading (T.crewHeading language)
        , div [ Attr.class "grid grid-cols-1 sm:grid-cols-2 gap-3" ]
            [ hostCard language actions TwoPlayers FeatherIcons.users (T.twoPlayerHost language) (T.twoPlayerHostDesc language)
            , hostCard language actions GridSession FeatherIcons.tv (T.gridHost language) (T.gridHostDesc language)
            ]
        , div [ Attr.class "flex flex-col sm:flex-row sm:items-center justify-between gap-3 p-3.5 rounded-xl bg-slate-900/65 border border-white/10" ]
            [ div [ Attr.class "flex flex-col gap-0.5 min-w-0" ]
                [ div [ Attr.class "flex items-center gap-2 font-bold text-sm text-white" ]
                    [ FeatherIcons.logIn |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                    , text (T.joinRaceTitle language)
                    ]
                , p [ Attr.class "text-xs text-slate-400" ] [ text (T.joinRaceDesc language) ]
                ]
            , div [ Attr.class "flex items-center gap-2 w-full sm:w-auto" ]
                [ input
                    [ Attr.type_ "text"
                    , Attr.value roomCode
                    , onInput actions.onRoomCodeChanged
                    , Attr.placeholder (T.roomCodePlaceholder language)
                    , Attr.attribute "aria-label" (T.roomCodeLabel language)
                    , Attr.attribute "autocomplete" "off"
                    , Attr.class "flex-1 min-w-0 sm:min-w-[160px] min-h-[44px] px-3 rounded-lg bg-slate-900/80 border border-slate-400/30 text-xs font-mono text-white placeholder:text-slate-500"
                    ]
                    []
                , button
                    [ Attr.type_ "button"
                    , onClick (actions.onJoinMultiplayer roomCode)
                    , Attr.title (T.joinRaceTitle language)
                    , Attr.class "min-h-[44px] px-4 rounded-lg bg-brand-yellow active:brightness-110 text-brand font-bold text-xs whitespace-nowrap cursor-pointer"
                    ]
                    [ text (T.joinRace language) ]
                ]
            ]
        ]


sectionHeading : String -> Html msg
sectionHeading label =
    h2 [ Attr.class "text-sm font-bold uppercase tracking-wider text-brand-yellow" ] [ text label ]


statBar : String -> Float -> Html msg
statBar label v =
    div [ Attr.class "flex flex-col gap-0.5 w-14 shrink-0" ]
        [ span [ Attr.class "text-slate-400 font-bold uppercase text-[10px]" ] [ text label ]
        , div [ Attr.class "h-1.5 rounded-full bg-slate-700 overflow-hidden" ]
            [ div [ Attr.class "h-full bg-sky-400 rounded-full", Attr.style "width" (String.fromInt (round (clamp 0 1 v * 100)) ++ "%") ] [] ]
        ]


{-| A host card for one session mode (a 2-player room or a grid session):
its title, what it is, and its host button.
-}
hostCard : Language -> ShipyardActions msg -> SessionMode -> FeatherIcons.Icon -> String -> String -> Html msg
hostCard language actions mode icon label description =
    div [ Attr.class "flex flex-col justify-between gap-3 p-3.5 rounded-xl bg-slate-900/65 border border-white/10" ]
        [ div [ Attr.class "flex flex-col gap-1.5" ]
            [ div [ Attr.class "flex items-center gap-2 font-bold text-sm text-white" ]
                [ icon |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                , text label
                ]
            , p [ Attr.class "text-xs text-slate-400" ] [ text description ]
            ]
        , button
            [ Attr.type_ "button"
            , onClick (actions.onStartHostMultiplayer mode)
            , Attr.title (T.hostTitle language (Multiplayer.sessionCapacity mode))
            , Attr.class "w-full min-h-[44px] px-3 rounded-lg bg-brand-yellow/20 border border-brand-yellow/40 text-brand-yellow font-extrabold text-xs cursor-pointer transition-all active:bg-brand-yellow/30"
            ]
            [ text label ]
        ]
