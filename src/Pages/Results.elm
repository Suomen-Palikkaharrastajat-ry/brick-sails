module Pages.Results exposing
    ( CrewResult
    , ResultsActions
    , viewResults
    )

{-| The end of a voyage, as Brick Racer's finish celebration in Brick Sails'
own look: while bricks burst over the boat (`Sails.Ending`), a card rises
with a bobbing boat, "Land ho!", the stars popping in one by one, the
headline, the final time with its sailing time, hits and treasure, the new
record badge, and the ways on: sail the same map again, roll a new map with
the same picks (sailing alone), or back to the main menu.

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
import Sails.Ending as Ending exposing (Ending)
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
    , onNewMap : msg
    , onBackToMenu : msg
    }


viewResults :
    { course : Course
    , progress : CourseProgress
    , boatName : String
    , personalBest : Maybe LeaderboardEntry
    , ending : Ending
    , crew : Maybe CrewResult
    , language : Language
    , actions : ResultsActions msg
    }
    -> Html msg
viewResults { course, progress, boatName, personalBest, ending, crew, language, actions } =
    let
        t =
            ending.timer

        -- In a room the time and stars are the crew's: the running crew
        -- clock until the whole crew is in port, then the crew's time.
        score =
            case crew of
                Just c ->
                    Maybe.withDefault c.clock c.crewTime

                Nothing ->
                    ending.score

        stars =
            case crew of
                Just _ ->
                    Physics.starRating course.targetSeconds score

                Nothing ->
                    ending.stars

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

        -- The card rises and fades in over the first 0.4 s.
        rise =
            clamp 0 1 (t / 0.4)

        shown =
            Ending.starsShown { ending | stars = stars }

        star i =
            let
                -- Each star pops: it grows past full size, then settles.
                age =
                    t - (0.4 + 0.35 * toFloat (i - 1))

                scale =
                    if i > shown then
                        1

                    else if age < 0.15 then
                        0.4 + 0.6 * (age / 0.15) * 1.3

                    else
                        1 + 0.3 * max 0 (1 - (age - 0.15) / 0.2)
            in
            span
                [ Attr.class
                    (if i <= shown then
                        "text-brand-yellow drop-shadow-[0_2px_6px_rgba(250,200,10,0.6)]"

                     else
                        "text-slate-700"
                    )
                , Attr.style "display" "inline-block"
                , Attr.style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
                , Attr.attribute "data-star"
                    (if i <= shown then
                        "on"

                     else
                        "off"
                    )
                ]
                [ FeatherIcons.star |> FeatherIcons.withSize 44 |> FeatherIcons.toHtml [ Attr.attribute "fill" "currentColor" ] ]

        nextGoal =
            if crewWaiting then
                ""

            else if stars < 3 then
                T.nextGoal language (Leaderboard.formatRaceTime course.targetSeconds)

            else
                ""

        statCard icon label value =
            div [ Attr.class "flex-1 min-w-[84px] flex flex-col items-center gap-0.5 px-2 py-2 rounded-xl bg-white/5 border border-white/10" ]
                [ span [ Attr.class "text-lg leading-none" ] [ text icon ]
                , span [ Attr.class "text-[10px] font-bold uppercase tracking-wider text-slate-400" ] [ text label ]
                , span [ Attr.class "font-mono text-sm font-black text-white" ] [ text value ]
                ]

        hitBonus =
            round (Physics.bonusPerHit * toFloat progress.targetsHit)
    in
    div
        [ Attr.class "fixed inset-0 z-30 flex items-center justify-center p-3 bg-brand/70 backdrop-blur-sm select-none overflow-y-auto"
        , Attr.attribute "data-testid" "ending"
        , Attr.attribute "role" "dialog"
        , Attr.attribute "aria-label" (T.finishToast language)
        ]
        [ div
            [ Attr.class "w-full max-w-sm landscape:max-w-xl bg-brand border-2 border-brand-yellow/60 rounded-3xl p-4 sm:p-5 shadow-2xl shadow-brand-yellow/20 text-white flex flex-col items-center text-center gap-3"
            , Attr.attribute "data-testid" "results-card"
            , Attr.style "opacity" (String.fromFloat rise)
            , Attr.style "transform" ("translateY(" ++ String.fromFloat (24 * (1 - rise)) ++ "px)")
            ]
            [ div
                [ Attr.class "text-5xl leading-none drop-shadow-[0_4px_12px_rgba(250,200,10,0.5)]"
                , Attr.style "transform" ("translateY(" ++ String.fromFloat (4 * sin (t * 3)) ++ "px) rotate(" ++ String.fromFloat (6 * sin (t * 2)) ++ "deg)")
                , Attr.attribute "aria-hidden" "true"
                ]
                [ text "⛵" ]
            , h1 [ Attr.class "text-3xl font-black uppercase tracking-wider text-brand-yellow" ] [ text (T.finishToast language) ]
            , if crewWaiting then
                div [ Attr.class "flex gap-1 text-sky-300" ] [ FeatherIcons.anchor |> FeatherIcons.withSize 44 |> FeatherIcons.toHtml [] ]

              else
                div [ Attr.class "flex gap-1", Attr.attribute "aria-label" (T.starsAria language stars) ] (List.map star [ 1, 2, 3 ])
            , p [ Attr.class "text-lg font-extrabold text-white text-balance" ] [ text headline ]
            , p [ Attr.class "text-xs text-slate-400 font-medium" ] [ text (T.mapTitle language course.map ++ " · " ++ T.boatName language boatName) ]
            , div [ Attr.class "w-full py-3 px-4 rounded-2xl bg-brand/60 border border-white/10 flex flex-col items-center gap-2", Attr.attribute "data-testid" "results-stats" ]
                [ if crew == Nothing then
                    span [ Attr.class "text-[10px] font-bold uppercase tracking-wider text-slate-400" ] [ text (T.finalTimeLabel language) ]

                  else
                    text ""
                , span [ Attr.class "font-mono text-4xl font-black text-sky-300 tracking-wider", Attr.attribute "data-testid" "ending-time" ] [ text (Leaderboard.formatRaceTime score) ]
                , case crew of
                    Just c ->
                        span [ Attr.class "text-xs text-slate-400" ] [ text (T.crewTimeLabel language c.inPort c.total) ]

                    Nothing ->
                        div [ Attr.class "w-full flex flex-wrap gap-2" ]
                            [ statCard "🧭" (T.sailingStat language) (Leaderboard.formatRaceTime progress.elapsedSeconds)
                            , statCard "🎯" (T.hitsStat language) (String.fromInt progress.targetsHit ++ " (−" ++ String.fromInt hitBonus ++ " s)")
                            , statCard "💰" (T.treasureStat language) (String.fromInt (List.length progress.treasureFound) ++ " (−" ++ String.fromInt (round progress.treasureBonus) ++ " s)")
                            ]
                , if crew /= Nothing then
                    -- A crew's result: no personal record to set or beat.
                    text ""

                  else if ending.isNewRecord then
                    span [ Attr.class "mt-1 px-3 py-0.5 rounded-full bg-brand-yellow/20 text-brand-yellow text-xs font-bold uppercase border border-brand-yellow/30 animate-pulse" ]
                        [ text ("🏆 " ++ T.newPersonalBest language) ]

                  else
                    span [ Attr.class "mt-1 text-xs text-slate-400" ]
                        [ text (T.personalBestShort language (personalBest |> Maybe.map (.timeSeconds >> Leaderboard.formatRaceTime) |> Maybe.withDefault "")) ]
                , if String.isEmpty nextGoal then
                    text ""

                  else
                    span [ Attr.class "text-[11px] text-slate-500" ] [ text nextGoal ]
                ]
            , div [ Attr.class "flex gap-2 w-full", Attr.attribute "data-testid" "results-actions" ]
                [ button
                    [ Attr.type_ "button"
                    , onClick actions.onSailAgain
                    , Attr.attribute "data-retry" ""
                    , Attr.attribute "data-testid" "ending-sail-again"
                    , Attr.class "flex-[2] min-h-[52px] px-4 rounded-2xl bg-brand-yellow active:brightness-110 text-brand font-black text-sm uppercase tracking-wider shadow-lg shadow-brand-yellow/25 flex items-center justify-center gap-2"
                    ]
                    [ FeatherIcons.refreshCw |> FeatherIcons.toHtml []
                    , text (T.sailAgain language)
                    ]
                , if crew == Nothing then
                    button
                        [ Attr.type_ "button"
                        , onClick actions.onNewMap
                        , Attr.attribute "data-testid" "ending-new-map"
                        , Attr.class "flex-1 min-h-[52px] px-3 rounded-2xl bg-slate-800 active:bg-slate-700 text-white font-bold text-xs uppercase tracking-wider flex items-center justify-center gap-1.5"
                        ]
                        [ FeatherIcons.map |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                        , text (T.newMapButton language)
                        ]

                  else
                    text ""
                , button
                    [ Attr.type_ "button"
                    , onClick actions.onBackToMenu
                    , Attr.attribute "data-testid" "ending-menu"
                    , Attr.class "flex-1 min-h-[52px] px-3 rounded-2xl bg-slate-800 active:bg-slate-700 text-white font-bold text-xs uppercase tracking-wider flex items-center justify-center gap-1.5"
                    ]
                    [ FeatherIcons.home |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                    , text (T.mainMenuButton language)
                    ]
                ]
            ]
        ]
