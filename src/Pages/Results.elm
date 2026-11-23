module Pages.Results exposing
    ( ResultsActions
    , viewResults
    )

{-| Finish screen: star rating, time with the target bonus, personal best,
and a big "Sail again" button.
-}

import FeatherIcons
import Game.Leaderboard as Leaderboard
import Html exposing (Html, button, div, h1, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import I18n exposing (Language)
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.Types exposing (Course, CourseProgress, LeaderboardEntry)


type alias ResultsActions msg =
    { onSailAgain : msg
    , onReturnToShipyard : msg
    }


viewResults :
    { course : Course
    , progress : CourseProgress
    , boatName : String
    , personalBest : Maybe LeaderboardEntry
    , language : Language
    , actions : ResultsActions msg
    }
    -> Html msg
viewResults { course, progress, boatName, personalBest, language, actions } =
    let
        score =
            Physics.finalTime progress

        stars =
            Physics.starRating course.targetSeconds score

        isNewRecord =
            case personalBest of
                Just pb ->
                    score < pb.timeSeconds

                Nothing ->
                    True

        star i =
            span
                [ Attr.class
                    (if i <= stars then
                        "text-amber-300 drop-shadow-[0_2px_6px_rgba(251,191,36,0.6)]"

                     else
                        "text-slate-700"
                    )
                ]
                [ FeatherIcons.star |> FeatherIcons.withSize 44 |> FeatherIcons.toHtml [ Attr.attribute "fill" "currentColor" ] ]

        headline =
            T.headline language stars

        nextGoal =
            if stars < 3 then
                T.nextGoal language (Leaderboard.formatRaceTime course.targetSeconds)

            else
                ""
    in
    div [ Attr.class "fixed inset-0 z-30 flex items-center justify-center p-3 bg-slate-950/70 backdrop-blur-sm select-none overflow-y-auto" ]
        [ div [ Attr.class "w-full max-w-sm landscape:max-w-xl bg-slate-900 border border-white/10 rounded-3xl p-4 sm:p-5 shadow-2xl text-white flex flex-col landscape:flex-row items-center text-center gap-3 landscape:gap-5" ]
            [ div [ Attr.class "flex flex-col items-center gap-1" ]
                [ div [ Attr.class "flex gap-1", Attr.attribute "aria-label" (T.starsAria language stars) ] (List.map star [ 1, 2, 3 ])
                , h1 [ Attr.class "text-2xl font-black uppercase tracking-wider text-amber-300 whitespace-nowrap" ] [ text headline ]
                , p [ Attr.class "text-xs text-slate-400 font-medium" ] [ text (T.courseTitle language course.title ++ " · " ++ T.boatName language boatName) ]
                ]
            , div [ Attr.class "w-full flex flex-col gap-3" ]
                [ div [ Attr.class "w-full py-3 px-4 rounded-2xl bg-slate-950/60 border border-white/10 flex flex-col items-center" ]
                    [ span [ Attr.class "font-mono text-4xl font-black text-sky-300 tracking-wider" ] [ text (Leaderboard.formatRaceTime score) ]
                    , span [ Attr.class "text-xs text-slate-400" ]
                        [ text
                            (T.resultBreakdown language
                                { sailing = Leaderboard.formatRaceTime progress.elapsedSeconds
                                , hitBonus = round (Physics.bonusPerHit * toFloat progress.targetsHit)
                                , hits = progress.targetsHit
                                , treasureBonus =
                                    if List.isEmpty progress.treasureFound then
                                        Nothing

                                    else
                                        Just (round progress.treasureBonus)
                                }
                            )
                        ]
                    , if isNewRecord then
                        span [ Attr.class "mt-1 px-3 py-0.5 rounded-full bg-amber-500/20 text-amber-300 text-xs font-bold uppercase border border-amber-500/30" ]
                            [ text (T.newPersonalBest language) ]

                      else
                        span [ Attr.class "mt-1 text-xs text-slate-400" ]
                            [ text (T.personalBestShort language (personalBest |> Maybe.map (.timeSeconds >> Leaderboard.formatRaceTime) |> Maybe.withDefault "")) ]
                    , if String.isEmpty nextGoal then
                        text ""

                      else
                        span [ Attr.class "text-[11px] text-slate-500" ] [ text nextGoal ]
                    ]
                , div [ Attr.class "flex gap-2 w-full" ]
                    [ button
                        [ onClick actions.onSailAgain
                        , Attr.attribute "data-retry" ""
                        , Attr.class "flex-[2] min-h-[52px] px-4 rounded-2xl bg-sky-500 active:bg-sky-400 text-slate-950 font-black text-sm uppercase tracking-wider shadow-lg shadow-sky-500/25 flex items-center justify-center gap-2"
                        ]
                        [ FeatherIcons.refreshCw |> FeatherIcons.toHtml []
                        , text (T.sailAgain language)
                        ]
                    , button
                        [ onClick actions.onReturnToShipyard
                        , Attr.class "flex-1 min-h-[52px] px-3 rounded-2xl bg-slate-800 active:bg-slate-700 text-white font-bold text-xs uppercase tracking-wider flex items-center justify-center gap-1.5"
                        ]
                        [ FeatherIcons.anchor |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                        , text (T.boatsButton language)
                        ]
                    ]
                ]
            ]
        ]
