module Pages.Results exposing
    ( CrewResult
    , ResultsActions
    , viewResults
    )

{-| Finish screen: star rating, time with the target bonus, personal best,
and a big "Sail again" button.

In an online room (`crew`) the result is the crew's, not this player's:
while crewmates are still at sea it says how many ships are in port and runs
the crew clock; once the whole crew is in port it shows the crew's shared
time and stars. There is no personal best, rank or winner in a room.

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


{-| The crew's side of the result in an online room: ships in port of the
crew's `total`, the crew clock, and the crew's time once all are in port.
-}
type alias CrewResult =
    { inPort : Int
    , total : Int
    , clock : Float
    , crewTime : Maybe Float
    }


type alias ResultsActions msg =
    { onSailAgain : msg
    , onReturnToShipyard : msg
    }


viewResults :
    { course : Course
    , progress : CourseProgress
    , boatName : String
    , personalBest : Maybe LeaderboardEntry
    , crew : Maybe CrewResult
    , language : Language
    , actions : ResultsActions msg
    }
    -> Html msg
viewResults { course, progress, boatName, personalBest, crew, language, actions } =
    let
        score =
            case crew of
                Just c ->
                    Maybe.withDefault c.clock c.crewTime

                Nothing ->
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
                        "text-brand-yellow drop-shadow-[0_2px_6px_rgba(250,200,10,0.6)]"

                     else
                        "text-slate-700"
                    )
                ]
                [ FeatherIcons.star |> FeatherIcons.withSize 44 |> FeatherIcons.toHtml [ Attr.attribute "fill" "currentColor" ] ]

        crewWaiting =
            case crew of
                Just c ->
                    c.crewTime == Nothing

                Nothing ->
                    False

        headline =
            case crew of
                Just c ->
                    if crewWaiting then
                        T.crewWaiting language c.inPort c.total

                    else
                        T.crewHeadline language stars

                Nothing ->
                    T.headline language stars

        nextGoal =
            if crewWaiting then
                ""

            else if stars < 3 then
                T.nextGoal language (Leaderboard.formatRaceTime course.targetSeconds)

            else
                ""
    in
    div [ Attr.class "fixed inset-0 z-30 flex items-center justify-center p-3 bg-brand/80 backdrop-blur-sm select-none overflow-y-auto" ]
        [ div [ Attr.class "w-full max-w-sm landscape:max-w-xl bg-brand border border-white/10 rounded-3xl p-4 sm:p-5 shadow-2xl text-white flex flex-col landscape:flex-row items-center text-center gap-3 landscape:gap-5" ]
            [ div [ Attr.class "flex flex-col items-center gap-1" ]
                [ if crewWaiting then
                    div [ Attr.class "flex gap-1 text-sky-300" ] [ FeatherIcons.anchor |> FeatherIcons.withSize 44 |> FeatherIcons.toHtml [] ]

                  else
                    div [ Attr.class "flex gap-1", Attr.attribute "aria-label" (T.starsAria language stars) ] (List.map star [ 1, 2, 3 ])
                , h1
                    [ Attr.class
                        (if crew == Nothing then
                            "text-2xl font-black uppercase tracking-wider text-brand-yellow whitespace-nowrap"

                         else
                            -- A crew's headline is longer: let it wrap.
                            "text-2xl font-black uppercase tracking-wider text-brand-yellow text-balance"
                        )
                    ]
                    [ text headline ]
                , p [ Attr.class "text-xs text-slate-400 font-medium" ] [ text (T.courseTitle language course.title ++ " · " ++ T.boatName language boatName) ]
                ]
            , div [ Attr.class "w-full flex flex-col gap-3" ]
                [ div [ Attr.class "w-full py-3 px-4 rounded-2xl bg-brand/60 border border-white/10 flex flex-col items-center" ]
                    [ span [ Attr.class "font-mono text-4xl font-black text-sky-300 tracking-wider", Attr.attribute "data-testid" "result-time" ] [ text (Leaderboard.formatRaceTime score) ]
                    , span [ Attr.class "text-xs text-slate-400" ]
                        [ text
                            (case crew of
                                Just c ->
                                    T.crewTimeLabel language c.inPort c.total

                                Nothing ->
                                    T.resultBreakdown language
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
                    , if crew /= Nothing then
                        -- A crew's result: no personal record to set or beat.
                        text ""

                      else if isNewRecord then
                        span [ Attr.class "mt-1 px-3 py-0.5 rounded-full bg-brand-yellow/20 text-brand-yellow text-xs font-bold uppercase border border-brand-yellow/30" ]
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
                        , Attr.class "flex-[2] min-h-[52px] px-4 rounded-2xl bg-brand-yellow active:brightness-110 text-brand font-black text-sm uppercase tracking-wider shadow-lg shadow-brand-yellow/25 flex items-center justify-center gap-2"
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
