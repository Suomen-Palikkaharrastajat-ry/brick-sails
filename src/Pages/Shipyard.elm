module Pages.Shipyard exposing
    ( ShipyardActions
    , viewShipyard
    )

{-| Harbor Shipyard Inspector view:
3D inspection plinth, ship seaworthiness analysis, preset selector,
course challenge picker, external Brick Studio / Brick Play links, and "Set Sail" launcher.
-}

import FeatherIcons
import Html exposing (Html, a, button, div, h1, h2, h3, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Math.Vector3 as Vec3
import Render.Camera exposing (Camera)
import Render.Scene exposing (Scene)
import Sails.Analyze as Analyze exposing (AnalysisReport, Seaworthiness(..), StabilityRating(..))
import Sails.Ingest as Ingest
import Sails.Render as Render
import Sails.Types
    exposing
        ( Course
        , CourseId(..)
        , ShipSpec
        )


type alias ShipyardActions msg =
    { onSelectPreset : String -> msg
    , onSelectCourse : CourseId -> msg
    , onStartSailing : msg
    , onStartHostMultiplayer : msg
    , onJoinMultiplayer : msg
    , onCopyShareUrl : msg
    , onToggleMute : msg
    }


viewShipyard :
    { spec : ShipSpec
    , scene : Scene
    , camera : Camera
    , report : AnalysisReport
    , selectedCourse : CourseId
    , audioMuted : Bool
    , currentHash : String
    , viewportWidth : Int
    , viewportHeight : Int
    , actions : ShipyardActions msg
    }
    -> Html msg
viewShipyard { spec, scene, camera, report, selectedCourse, audioMuted, currentHash, viewportWidth, viewportHeight, actions } =
    div [ Attr.class "relative w-full h-full overflow-hidden bg-slate-950 font-sans" ]
        [ -- 1. Fullscreen WebGL 3D Ship Inspection Plinth
          div [ Attr.class "absolute inset-0 z-0" ]
            [ Render.renderShipyardViewport camera spec scene viewportWidth viewportHeight
            ]
        , -- 2. Top Header Bar
          div [ Attr.class "absolute top-0 left-0 right-0 z-10 flex justify-between items-center p-4 bg-gradient-to-b from-slate-950/80 to-transparent pointer-events-none" ]
            [ div [ Attr.class "flex items-center gap-3 pointer-events-auto" ]
                [ div [ Attr.class "w-10 h-10 rounded-2xl bg-sky-500 flex items-center justify-center text-white shadow-lg shadow-sky-500/30" ]
                    [ FeatherIcons.compass |> FeatherIcons.toHtml [] ]
                , div []
                    [ h1 [ Attr.class "text-lg font-black tracking-wider text-white uppercase flex items-center gap-2" ]
                        [ text "Brick Sails"
                        , span [ Attr.class "text-xs font-normal px-2 py-0.5 rounded-full bg-sky-500/20 text-sky-400 border border-sky-500/30" ]
                            [ text "Palikkapurjehdus" ]
                        ]
                    , p [ Attr.class "text-xs text-slate-300" ] [ text "3D LEGO® Sailing Simulator & Regatta" ]
                    ]
                ]
            , div [ Attr.class "flex items-center gap-2 pointer-events-auto" ]
                [ button
                    [ onClick actions.onToggleMute
                    , Attr.title (if audioMuted then "Unmute audio" else "Mute audio")
                    , Attr.class "p-2.5 rounded-2xl bg-slate-900/80 hover:bg-slate-800 text-slate-300 border border-white/10 backdrop-blur-md transition-all shadow-md"
                    ]
                    [ if audioMuted then
                        FeatherIcons.volumeX |> FeatherIcons.toHtml []

                      else
                        FeatherIcons.volume2 |> FeatherIcons.toHtml []
                    ]
                , button
                    [ onClick actions.onCopyShareUrl
                    , Attr.title "Copy share link"
                    , Attr.class "flex items-center gap-2 px-3.5 py-2 rounded-2xl bg-slate-900/80 hover:bg-slate-800 text-slate-200 border border-white/10 backdrop-blur-md text-xs font-bold transition-all shadow-md"
                    ]
                    [ FeatherIcons.share2 |> FeatherIcons.toHtml []
                    , span [ Attr.class "hidden sm:inline" ] [ text "Share Ship" ]
                    ]
                ]
            ]
        , -- 3. Bottom Shipyard Controls & Analysis Panel
          div [ Attr.class "absolute bottom-0 left-0 right-0 z-10 p-4 bg-gradient-to-t from-slate-950 via-slate-950/80 to-transparent flex flex-col md:flex-row justify-between items-end gap-4 pointer-events-none" ]
            [ -- Left Card: Ship Specifications & Seaworthiness Report
              div [ Attr.class "w-full md:max-w-md bg-slate-900/90 backdrop-blur-md p-4 rounded-3xl border border-white/10 shadow-2xl text-white pointer-events-auto flex flex-col gap-3" ]
                [ div [ Attr.class "flex justify-between items-center" ]
                    [ div []
                        [ h2 [ Attr.class "text-base font-black tracking-wide" ] [ text spec.name ]
                        , p [ Attr.class "text-xs text-slate-400" ]
                            [ text (String.fromInt spec.totalPartCount ++ " parts (" ++ String.fromInt spec.hullPartCount ++ " hull, " ++ String.fromInt spec.sailPartCount ++ " sail)") ]
                        ]
                    , viewSeaworthinessBadge report.seaworthiness
                    ]
                , -- Specs Grid
                  div [ Attr.class "grid grid-cols-4 gap-2 text-center text-xs" ]
                    [ div [ Attr.class "p-2 rounded-xl bg-slate-800/60" ]
                        [ span [ Attr.class "block text-slate-400 text-[10px] uppercase font-bold" ] [ text "Length" ]
                        , span [ Attr.class "font-bold text-sky-400" ] [ text (String.fromFloat (toFloat (round (spec.lengthMeters * 10.0)) / 10.0) ++ "m") ]
                        ]
                    , div [ Attr.class "p-2 rounded-xl bg-slate-800/60" ]
                        [ span [ Attr.class "block text-slate-400 text-[10px] uppercase font-bold" ] [ text "Beam" ]
                        , span [ Attr.class "font-bold text-sky-400" ] [ text (String.fromFloat (toFloat (round (spec.beamMeters * 10.0)) / 10.0) ++ "m") ]
                        ]
                    , div [ Attr.class "p-2 rounded-xl bg-slate-800/60" ]
                        [ span [ Attr.class "block text-slate-400 text-[10px] uppercase font-bold" ] [ text "Sail Area" ]
                        , span [ Attr.class "font-bold text-amber-400" ] [ text (String.fromInt (round spec.sailAreaSqMeters) ++ " m²") ]
                        ]
                    , div [ Attr.class "p-2 rounded-xl bg-slate-800/60" ]
                        [ span [ Attr.class "block text-slate-400 text-[10px] uppercase font-bold" ] [ text "Stability" ]
                        , span [ Attr.class "font-bold text-emerald-400" ] [ text ("GM " ++ String.fromFloat (toFloat (round (spec.metacentricHeight * 10.0)) / 10.0)) ]
                        ]
                    ]
                , -- Advisory Notes
                  div [ Attr.class "flex flex-col gap-1 text-[11px] text-slate-300 bg-slate-950/40 p-2.5 rounded-xl border border-white/5" ]
                    (List.map (\note -> div [ Attr.class "flex items-start gap-1.5" ] [ span [ Attr.class "text-sky-400 mt-0.5" ] [ text "•" ], text note ]) report.notes)
                , -- External Ecosystem Links
                  div [ Attr.class "flex items-center gap-2 pt-1 border-t border-slate-800 text-xs" ]
                    [ a
                        [ Attr.href ("https://palikkaharrastajat.fi/" ++ currentHash)
                        , Attr.target "_blank"
                        , Attr.rel "noopener noreferrer"
                        , Attr.class "flex-1 py-1.5 px-2 rounded-xl bg-slate-800/80 hover:bg-slate-700 text-center font-semibold text-slate-300 transition-colors"
                        ]
                        [ text "Edit in Studio" ]
                    , a
                        [ Attr.href ("https://leiki.palikkaharrastajat.fi/" ++ currentHash)
                        , Attr.target "_blank"
                        , Attr.rel "noopener noreferrer"
                        , Attr.class "flex-1 py-1.5 px-2 rounded-xl bg-slate-800/80 hover:bg-slate-700 text-center font-semibold text-slate-300 transition-colors"
                        ]
                        [ text "Play in Brick Play" ]
                    ]
                ]
            , -- Right Card: Preset Switcher, Course Selector & "SET SAIL"
              div [ Attr.class "w-full md:max-w-xs bg-slate-900/90 backdrop-blur-md p-4 rounded-3xl border border-white/10 shadow-2xl text-white pointer-events-auto flex flex-col gap-3" ]
                [ -- Presets
                  div []
                    [ span [ Attr.class "block text-[11px] font-bold text-slate-400 uppercase tracking-wider mb-1.5" ] [ text "Choose Vessel" ]
                    , div [ Attr.class "grid grid-cols-2 gap-1.5" ]
                        (List.map
                            (\p ->
                                button
                                    [ onClick (actions.onSelectPreset p.hash)
                                    , Attr.class
                                        ("px-2.5 py-1.5 rounded-xl text-xs font-bold transition-all text-left truncate "
                                            ++ (if p.name == spec.name then
                                                    "bg-sky-500 text-white shadow-md shadow-sky-500/25"

                                                else
                                                    "bg-slate-800 hover:bg-slate-700 text-slate-300"
                                               )
                                        )
                                    ]
                                    [ text p.name ]
                            )
                            Ingest.presets
                        )
                    ]
                , -- Multiplayer Section
                  div [ Attr.class "flex flex-col gap-1.5 pt-1 border-t border-slate-800" ]
                    [ span [ Attr.class "block text-[10px] font-bold text-sky-400 uppercase tracking-wider" ] [ text "Online Multiplayer" ]
                    , div [ Attr.class "flex gap-1.5" ]
                        [ button
                            [ onClick actions.onStartHostMultiplayer
                            , Attr.title "Create 4-player room with commentator split-screen view"
                            , Attr.class "flex-1 py-2 px-2.5 rounded-xl bg-slate-800 hover:bg-slate-700 text-sky-300 font-bold text-xs transition-colors flex items-center justify-center gap-1.5 border border-sky-500/20 shadow-sm"
                            ]
                            [ FeatherIcons.tv |> FeatherIcons.toHtml []
                            , text "Host (4-Split)"
                            ]
                        , button
                            [ onClick actions.onJoinMultiplayer
                            , Attr.title "Join existing regatta room with single mobile view"
                            , Attr.class "flex-1 py-2 px-2.5 rounded-xl bg-slate-800 hover:bg-slate-700 text-slate-200 font-bold text-xs transition-colors flex items-center justify-center gap-1.5 border border-white/5"
                            ]
                            [ FeatherIcons.users |> FeatherIcons.toHtml []
                            , text "Join Regatta"
                            ]
                        ]
                    ]
                , -- Launch "SET SAIL" Button
                  button
                    [ onClick actions.onStartSailing
                    , Attr.class "w-full py-3.5 px-4 rounded-2xl bg-gradient-to-r from-sky-500 via-sky-400 to-teal-400 hover:brightness-110 active:scale-[0.98] transition-all text-slate-950 font-black text-sm uppercase tracking-widest shadow-xl shadow-sky-500/30 flex items-center justify-center gap-2"
                    ]
                    [ FeatherIcons.navigation |> FeatherIcons.toHtml []
                    , text "SET SAIL (Solo)"
                    ]
                ]
            ]
        ]


viewSeaworthinessBadge : Seaworthiness -> Html msg
viewSeaworthinessBadge status =
    case status of
        Seaworthy ->
            span [ Attr.class "px-2.5 py-1 rounded-full bg-emerald-500/20 text-emerald-400 border border-emerald-500/30 text-xs font-black tracking-wider uppercase flex items-center gap-1" ]
                [ FeatherIcons.checkCircle |> FeatherIcons.toHtml []
                , text "Seaworthy"
                ]

        MinorAdvisory msg ->
            span [ Attr.class "px-2.5 py-1 rounded-full bg-amber-500/20 text-amber-400 border border-amber-500/30 text-xs font-bold tracking-wider uppercase flex items-center gap-1" ]
                [ FeatherIcons.alertTriangle |> FeatherIcons.toHtml []
                , text "Advisory"
                ]

        Unseaworthy msg ->
            span [ Attr.class "px-2.5 py-1 rounded-full bg-red-500/20 text-red-400 border border-red-500/30 text-xs font-bold tracking-wider uppercase flex items-center gap-1" ]
                [ FeatherIcons.xCircle |> FeatherIcons.toHtml []
                , text "Unseaworthy"
                ]
