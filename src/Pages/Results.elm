module Pages.Results exposing
    ( ResultsActions
    , viewResults
    )

{-| Regatta Challenge finish & course leaderboard results screen.
-}

import FeatherIcons
import Game.Leaderboard as Leaderboard
import Html exposing (Html, button, div, h1, h2, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Sails.Types
    exposing
        ( Course
        , CourseProgress
        , LeaderboardEntry
        , ShipSpec
        )


type alias ResultsActions msg =
    { onSailAgain : msg
    , onReturnToShipyard : msg
    , onCopyShareUrl : msg
    }


viewResults :
    { course : Course
    , progress : CourseProgress
    , spec : ShipSpec
    , personalBest : Maybe LeaderboardEntry
    , actions : ResultsActions msg
    }
    -> Html msg
viewResults { course, progress, spec, personalBest, actions } =
    let
        finalTime =
            Leaderboard.formatRaceTime progress.elapsedSeconds

        isNewRecord =
            case personalBest of
                Just pb ->
                    progress.elapsedSeconds <= pb.timeSeconds

                Nothing ->
                    True
    in
    div [ Attr.class "fixed inset-0 z-30 flex items-center justify-center p-4 bg-slate-950/85 backdrop-blur-lg select-none font-sans" ]
        [ div [ Attr.class "w-full max-w-md bg-slate-900 border border-white/10 rounded-3xl p-6 shadow-2xl text-white flex flex-col items-center text-center gap-4 animate-in fade-in zoom-in-95 duration-200" ]
            [ -- Trophy Icon
              div [ Attr.class "w-16 h-16 rounded-full bg-amber-500/20 text-amber-400 border border-amber-500/30 flex items-center justify-center shadow-lg shadow-amber-500/20" ]
                [ FeatherIcons.award |> FeatherIcons.toHtml [] ]
            , -- Header
              div []
                [ h1 [ Attr.class "text-2xl font-black uppercase tracking-wider text-amber-400" ]
                    [ text "Regatta Complete!" ]
                , p [ Attr.class "text-sm text-slate-300 font-medium" ]
                    [ text (course.title ++ " • " ++ spec.name) ]
                ]
            , -- Big Final Time Card
              div [ Attr.class "w-full py-4 px-6 rounded-2xl bg-slate-950/60 border border-white/10 flex flex-col items-center" ]
                [ span [ Attr.class "text-xs font-bold text-slate-400 uppercase tracking-widest" ] [ text "Final Race Time" ]
                , span [ Attr.class "font-mono text-4xl font-black text-sky-400 tracking-wider my-1" ] [ text finalTime ]
                , if isNewRecord then
                    span [ Attr.class "px-3 py-0.5 rounded-full bg-amber-500/20 text-amber-400 text-xs font-bold uppercase border border-amber-500/30 tracking-wider" ]
                        [ text "★ New Personal Best!" ]

                  else
                    span [ Attr.class "text-xs text-slate-400 font-medium" ]
                        [ text
                            (case personalBest of
                                Just pb ->
                                    "Personal Best: " ++ Leaderboard.formatRaceTime pb.timeSeconds

                                Nothing ->
                                    ""
                            )
                        ]
                ]
            , -- Action Buttons
              div [ Attr.class "flex flex-col sm:flex-row gap-2.5 w-full mt-2" ]
                [ button
                    [ onClick actions.onSailAgain
                    , Attr.class "flex-1 py-3 px-4 rounded-2xl bg-sky-500 hover:bg-sky-400 text-slate-950 font-black text-xs uppercase tracking-wider transition-all shadow-lg shadow-sky-500/25 flex items-center justify-center gap-1.5"
                    ]
                    [ FeatherIcons.refreshCw |> FeatherIcons.toHtml []
                    , text "Sail Again"
                    ]
                , button
                    [ onClick actions.onReturnToShipyard
                    , Attr.class "flex-1 py-3 px-4 rounded-2xl bg-slate-800 hover:bg-slate-700 text-white font-bold text-xs uppercase tracking-wider transition-colors flex items-center justify-center gap-1.5"
                    ]
                    [ FeatherIcons.anchor |> FeatherIcons.toHtml []
                    , text "Shipyard"
                    ]
                ]
            ]
        ]
