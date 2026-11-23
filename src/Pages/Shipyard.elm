module Pages.Shipyard exposing
    ( ShipyardActions
    , viewShipyard
    )

{-| Start screen: pick one of four toy boats (or a ship loaded from a build
link), see it bob on the water, and set sail. A build link can be pasted or a
model file opened. Online play (host a split-screen room or join one) sits
below.
-}

import FeatherIcons
import Game.Leaderboard as Leaderboard
import Game.Multiplayer as Multiplayer exposing (SessionMode(..))
import Html exposing (Html, button, div, h1, input, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput, onSubmit)
import I18n exposing (Language)
import Sails.Boats as Boats
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.Render as Render
import Sails.ShipMesh exposing (ShipMesh)
import Sails.Types exposing (Boat, Course, LeaderboardEntry)


type alias ShipyardActions msg =
    { onSelectBoat : String -> msg
    , onStartSailing : msg
    , onStartHostMultiplayer : SessionMode -> msg
    , onJoinMultiplayer : String -> msg
    , onRoomCodeChanged : String -> msg
    , onToggleMute : msg
    , onPasteChanged : String -> msg
    , onLoadPasted : msg
    , onOpenFile : msg
    , onToggleLanguage : msg
    }


{-| Loading state of the selected boat's bricks.
-}
type alias Loading =
    { loaded : Int, total : Int }


viewShipyard :
    { boat : Boat
    , mesh : Maybe ShipMesh
    , loading : Maybe Loading
    , customShip : Maybe Boat
    , pasteText : String
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
    , actions : ShipyardActions msg
    }
    -> Html msg
viewShipyard cfg =
    div [ Attr.class "relative w-full h-full overflow-hidden bg-slate-950 font-sans select-none" ]
        [ div [ Attr.class "absolute inset-0 z-0" ]
            [ Render.renderShipyard cfg.mesh cfg.time cfg.viewportWidth cfg.viewportHeight ]
        , viewHeader cfg
        , viewLoading cfg.language cfg.mesh cfg.loading
        , viewPanel cfg
        ]


viewLoading : Language -> Maybe ShipMesh -> Maybe Loading -> Html msg
viewLoading lang mesh loading =
    case ( mesh, loading ) of
        ( Nothing, Just l ) ->
            div [ Attr.class "absolute z-10 left-0 right-0 top-1/4 flex justify-center pointer-events-none" ]
                [ span [ Attr.class "px-3 py-1.5 rounded-xl bg-slate-900/80 text-sky-100 text-xs font-bold" ]
                    [ text (T.loadingBricks lang l.loaded (max l.loaded l.total)) ]
                ]

        _ ->
            text ""


viewHeader : { a | audioMuted : Bool, tilt : { available : Bool, enabled : Bool }, language : Language, actions : ShipyardActions msg } -> Html msg
viewHeader { audioMuted, tilt, language, actions } =
    div
        [ Attr.class "absolute top-0 left-0 right-0 z-10 flex justify-between items-start gap-2 p-3 bg-gradient-to-b from-slate-950/70 to-transparent pointer-events-none"
        , Attr.style "padding-top" "max(12px, env(safe-area-inset-top))"
        , Attr.style "padding-left" "max(12px, env(safe-area-inset-left))"
        , Attr.style "padding-right" "max(12px, env(safe-area-inset-right))"
        ]
        [ div [ Attr.class "flex items-center gap-2.5" ]
            [ div [ Attr.class "w-10 h-10 rounded-2xl bg-sky-500 flex items-center justify-center text-white shadow-lg" ]
                [ FeatherIcons.navigation |> FeatherIcons.toHtml [] ]
            , div []
                [ h1 [ Attr.class "text-lg font-black tracking-wider text-white uppercase leading-tight" ] [ text (T.title language) ]
                , p [ Attr.class "text-xs text-sky-200" ] [ text (T.subtitle language) ]
                ]
            ]
        , div [ Attr.class "flex gap-1.5 pointer-events-auto" ]
            [ if tilt.available then
                button
                    [ Attr.attribute "data-tilt-toggle" ""
                    , Attr.title (T.tiltToggleTitle language)
                    , Attr.class
                        ("h-11 px-3 flex items-center gap-1.5 rounded-xl border border-white/10 text-xs font-bold "
                            ++ (if tilt.enabled then
                                    "bg-sky-500 text-slate-950"

                                else
                                    "bg-slate-900/80 text-white"
                               )
                        )
                    ]
                    [ FeatherIcons.smartphone |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                    , text (T.tiltLabel language tilt.enabled)
                    ]

              else
                text ""
            , button
                [ onClick actions.onToggleLanguage
                , Attr.title (T.languageToggleTitle language)
                , Attr.attribute "aria-label" (T.languageToggleTitle language)
                , Attr.attribute "data-language-toggle" ""
                , Attr.class "w-11 h-11 flex items-center justify-center rounded-xl bg-slate-900/80 border border-white/10 text-white text-xs font-black"
                ]
                [ text (T.languageToggle language) ]
            , button
                [ onClick actions.onToggleMute
                , Attr.title (T.muteLabel language audioMuted)
                , Attr.attribute "aria-label" (T.muteLabel language audioMuted)
                , Attr.class "w-11 h-11 flex items-center justify-center rounded-xl bg-slate-900/80 border border-white/10 text-white"
                ]
                [ (if audioMuted then
                    FeatherIcons.volumeX

                   else
                    FeatherIcons.volume2
                  )
                    |> FeatherIcons.toHtml []
                ]
            ]
        ]


viewPanel :
    { a
        | boat : Boat
        , course : Course
        , customShip : Maybe Boat
        , pasteText : String
        , loadError : Maybe String
        , personalBest : Maybe LeaderboardEntry
        , tilt : { available : Bool, enabled : Bool }
        , roomCode : String
        , language : Language
        , actions : ShipyardActions msg
    }
    -> Html msg
viewPanel { boat, course, customShip, pasteText, loadError, personalBest, tilt, roomCode, language, actions } =
    let
        bestLine =
            case personalBest of
                Just pb ->
                    T.personalBestLine language (Leaderboard.formatRaceTime pb.timeSeconds) (Physics.starRating course.targetSeconds pb.timeSeconds)

                Nothing ->
                    T.bestLine language (Leaderboard.formatRaceTime course.targetSeconds)

        controlsHint =
            T.controlsHint language tilt.enabled
    in
    div
        [ Attr.class "absolute z-10 left-0 right-0 bottom-0 p-3 landscape:left-auto landscape:top-0 landscape:pt-[68px] landscape:w-[360px] landscape:flex landscape:flex-col landscape:justify-end"
        , Attr.style "padding-bottom" "max(12px, env(safe-area-inset-bottom))"
        , Attr.style "padding-left" "max(12px, env(safe-area-inset-left))"
        , Attr.style "padding-right" "max(12px, env(safe-area-inset-right))"
        ]
        [ div [ Attr.class "w-full max-w-md mx-auto bg-slate-900/90 backdrop-blur-md p-3 rounded-3xl border border-white/10 shadow-2xl text-white flex flex-col gap-2.5 max-h-full overflow-y-auto" ]
            [ div [ Attr.class "flex justify-between items-baseline gap-2" ]
                [ span [ Attr.class "text-sm font-black" ] [ text (T.courseTitle language course.title) ]
                , span [ Attr.class "text-xs text-amber-300 font-bold" ] [ text bestLine ]
                ]
            , div [ Attr.class "grid grid-cols-4 gap-1.5" ]
                (List.map (boatChip language boat actions) Boats.all
                    ++ (case customShip of
                            Just c ->
                                [ div [ Attr.class "col-span-4 grid" ] [ boatChip language boat actions c ] ]

                            Nothing ->
                                []
                       )
                )
            , div [ Attr.class "flex items-center gap-3 text-[11px]" ]
                [ span [ Attr.class "text-slate-300 flex-1 min-w-0" ] [ text (T.boatDescription language boat.description) ]
                , statBar (T.speedLabel language) ((boat.topSpeed - 7) / 3.2)
                , statBar (T.turnLabel language) ((boat.turnRate - 50) / 80)
                ]
            , button
                [ onClick actions.onStartSailing
                , Attr.attribute "data-tilt-start" ""
                , Attr.class "w-full min-h-[52px] px-4 rounded-2xl bg-gradient-to-r from-sky-500 to-teal-400 active:brightness-110 text-slate-950 font-black text-base uppercase tracking-widest shadow-xl shadow-sky-500/30 flex items-center justify-center gap-2"
                ]
                [ FeatherIcons.play |> FeatherIcons.toHtml []
                , text (T.setSail language)
                ]
            , p [ Attr.class "text-[11px] text-slate-400 text-center landscape:hidden lg:landscape:block" ] [ text controlsHint ]
            , viewLoadShip language pasteText loadError actions
            , div [ Attr.class "flex flex-col gap-1.5 pt-1 border-t border-slate-800" ]
                [ div [ Attr.class "flex gap-1.5" ]
                    [ hostButton language actions TwoPlayers (T.twoPlayerHost language)
                    , hostButton language actions GridSession (T.gridHost language)
                    ]
                , div [ Attr.class "flex gap-1.5" ]
                    [ input
                        [ Attr.type_ "text"
                        , Attr.value roomCode
                        , onInput actions.onRoomCodeChanged
                        , Attr.placeholder (T.roomCodePlaceholder language)
                        , Attr.attribute "aria-label" (T.roomCodeLabel language)
                        , Attr.attribute "autocomplete" "off"
                        , Attr.class "flex-1 min-w-0 min-h-[44px] px-3 rounded-xl bg-slate-800 border border-white/10 text-xs font-mono text-white placeholder:text-slate-500"
                        ]
                        []
                    , button
                        [ onClick (actions.onJoinMultiplayer roomCode)
                        , Attr.title (T.joinRaceTitle language)
                        , Attr.class "min-h-[44px] px-3 rounded-xl bg-slate-800 active:bg-slate-700 text-slate-200 font-bold text-xs flex items-center justify-center gap-1.5"
                        ]
                        [ FeatherIcons.users |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
                        , text (T.joinRace language)
                        ]
                    ]
                ]
            ]
        ]


{-| Paste a build link (or open a model file) to sail your own ship.
-}
viewLoadShip : Language -> String -> Maybe String -> ShipyardActions msg -> Html msg
viewLoadShip language pasteText loadError actions =
    div [ Attr.class "flex flex-col gap-1" ]
        [ Html.form [ onSubmit actions.onLoadPasted, Attr.class "flex gap-1.5" ]
            [ input
                [ Attr.type_ "text"
                , Attr.value pasteText
                , onInput actions.onPasteChanged
                , Attr.placeholder (T.buildLinkPlaceholder language)
                , Attr.attribute "aria-label" (T.buildLinkLabel language)
                , Attr.attribute "data-ship-link" ""
                , Attr.attribute "autocomplete" "off"
                , Attr.class "flex-1 min-w-0 min-h-[44px] px-3 rounded-xl bg-slate-800 border border-white/10 text-sm text-white placeholder:text-slate-500"
                ]
                []
            , button
                [ Attr.type_ "submit"
                , Attr.attribute "data-ship-load" ""
                , Attr.class "min-h-[44px] px-3 rounded-xl bg-slate-800 active:bg-slate-700 text-sky-300 font-bold text-xs"
                ]
                [ text (T.loadButton language) ]
            , button
                [ Attr.type_ "button"
                , onClick actions.onOpenFile
                , Attr.title (T.openModelFile language)
                , Attr.class "w-11 min-h-[44px] flex items-center justify-center rounded-xl bg-slate-800 active:bg-slate-700 text-slate-200"
                ]
                [ FeatherIcons.folder |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
            ]
        , case loadError of
            Just err ->
                p [ Attr.attribute "role" "alert", Attr.class "text-[11px] text-rose-300 font-bold" ] [ text (T.shipLoadError language err) ]

            Nothing ->
                text ""
        ]


boatChip : Language -> Boat -> ShipyardActions msg -> Boat -> Html msg
boatChip language selected actions b =
    button
        [ onClick (actions.onSelectBoat b.name)
        , Attr.attribute "aria-pressed"
            (if b.name == selected.name then
                "true"

             else
                "false"
            )
        , Attr.class
            ("min-h-[44px] px-1 rounded-xl text-xs font-bold truncate "
                ++ (if b.name == selected.name then
                        "bg-sky-500 text-slate-950"

                    else
                        "bg-slate-800 active:bg-slate-700 text-slate-200"
                   )
            )
        ]
        [ text (T.boatName language b.name) ]


statBar : String -> Float -> Html msg
statBar label v =
    div [ Attr.class "flex flex-col gap-0.5 w-14 shrink-0" ]
        [ span [ Attr.class "text-slate-400 font-bold uppercase text-[10px]" ] [ text label ]
        , div [ Attr.class "h-1.5 rounded-full bg-slate-700 overflow-hidden" ]
            [ div [ Attr.class "h-full bg-sky-400 rounded-full", Attr.style "width" (String.fromInt (round (clamp 0 1 v * 100)) ++ "%") ] [] ]
        ]


{-| A host button for one session mode (a 2-player room or a grid session).
-}
hostButton : Language -> ShipyardActions msg -> SessionMode -> String -> Html msg
hostButton language actions mode label =
    button
        [ onClick (actions.onStartHostMultiplayer mode)
        , Attr.title (T.hostTitle language (Multiplayer.sessionCapacity mode))
        , Attr.class "flex-1 min-h-[44px] px-2 rounded-xl bg-slate-800 active:bg-slate-700 text-sky-300 font-bold text-xs flex items-center justify-center gap-1.5"
        ]
        [ FeatherIcons.tv |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
        , text label
        ]
