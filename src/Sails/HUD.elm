module Sails.HUD exposing
    ( HudActions
    , Side(..)
    , viewHud
    )

{-| Touch-first race HUD.

Top left: timer, buoy progress and wind. Top right: small icon buttons.
Bottom left: big left/right steering buttons. Bottom right: a big FIRE
button. The middle of the screen stays clear for the boat in both portrait
and landscape. Steering buttons react to pointer down/up so they can be held.

-}

import FeatherIcons
import Game.Leaderboard as Leaderboard
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Sails.Physics as Physics
import Sails.Types exposing (Course, CourseProgress, SailingState)


type Side
    = Left
    | Right


type alias HudActions msg =
    { onSteer : Side -> Bool -> msg
    , onFire : msg
    , onToggleMute : msg
    , onRestart : msg
    , onExit : msg
    }


viewHud :
    { progress : CourseProgress
    , course : Course
    , state : SailingState
    , cameraYaw : Float
    , cannonReady : Bool
    , targetInRange : Bool
    , audioMuted : Bool
    , tilt : { available : Bool, enabled : Bool }
    , countdown : Maybe Int
    , toast : Maybe String
    , actions : HudActions msg
    }
    -> Html msg
viewHud cfg =
    div
        [ Attr.class "absolute inset-0 z-20 pointer-events-none select-none overflow-hidden text-white"
        , Attr.style "padding" "max(8px, env(safe-area-inset-top)) max(8px, env(safe-area-inset-right)) max(10px, env(safe-area-inset-bottom)) max(8px, env(safe-area-inset-left))"
        ]
        [ div [ Attr.class "relative w-full h-full" ]
            [ div [ Attr.class "absolute top-0 left-0 right-0 flex justify-between items-start gap-2" ]
                [ viewStatus cfg
                , viewTopButtons cfg
                ]
            , viewToast cfg.toast
            , viewCountdown cfg.countdown
            , div [ Attr.class "absolute bottom-0 left-0 flex gap-3 pointer-events-auto" ]
                [ steerButton cfg.actions Left
                , steerButton cfg.actions Right
                ]
            , div [ Attr.class "absolute bottom-0 right-0 pointer-events-auto" ]
                [ fireButton cfg ]
            ]
        ]


panel : String
panel =
    "bg-slate-900/80 backdrop-blur-md border border-white/10 shadow-lg rounded-2xl"


viewStatus :
    { a
        | progress : CourseProgress
        , course : Course
        , state : SailingState
        , cameraYaw : Float
    }
    -> Html msg
viewStatus { progress, course, state, cameraYaw } =
    let
        total =
            progress.totalWaypoints

        dot i =
            span
                [ Attr.class
                    ("inline-block w-2.5 h-2.5 rounded-full "
                        ++ (if i < progress.currentWaypointIndex then
                                "bg-emerald-400"

                            else if i == progress.currentWaypointIndex then
                                "bg-amber-300 ring-2 ring-amber-200/60"

                            else
                                "bg-slate-600"
                           )
                    )
                ]
                []

        -- Screen angle of the wind arrow: camera yaw minus the direction the wind blows to.
        windRotation =
            cameraYaw - course.wind.direction

        powerLabel =
            if state.sailPower > 0.9 then
                ( "Fast", "text-emerald-300" )

            else if state.sailPower > 0.75 then
                ( "Good", "text-sky-300" )

            else
                ( "Into wind", "text-amber-300" )
    in
    div [ Attr.class (panel ++ " px-3 py-2 flex flex-col gap-1 min-w-0") ]
        [ span [ Attr.class "font-mono text-2xl font-black tracking-wide text-amber-300 leading-none" ]
            [ text (Leaderboard.formatRaceTime progress.elapsedSeconds) ]
        , div [ Attr.class "flex items-center gap-1.5", Attr.attribute "aria-label" "Buoys" ]
            (span [ Attr.class "text-[11px] font-bold text-slate-300 mr-0.5" ]
                [ text ("Buoy " ++ String.fromInt (min total (progress.currentWaypointIndex + 1)) ++ "/" ++ String.fromInt total) ]
                :: List.map dot (List.range 0 (total - 1))
            )
        , div [ Attr.class "flex items-center gap-2 text-[11px] font-bold" ]
            [ span
                [ Attr.class "inline-flex text-sky-300"
                , Attr.style "transform" ("rotate(" ++ String.fromFloat windRotation ++ "deg)")
                , Attr.title "Wind direction"
                ]
                [ FeatherIcons.arrowUp |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
            , span [ Attr.class (Tuple.second powerLabel) ] [ text (Tuple.first powerLabel) ]
            , if progress.targetsHit > 0 then
                span [ Attr.class "text-rose-300" ]
                    [ text ("Targets " ++ String.fromInt progress.targetsHit ++ " (-" ++ String.fromInt (round (Physics.bonusPerHit * toFloat progress.targetsHit)) ++ "s)") ]

              else
                text ""
            ]
        ]


iconButton : List (Html.Attribute msg) -> String -> FeatherIcons.Icon -> Html msg
iconButton attrs label icon =
    button
        ([ Attr.title label
         , Attr.attribute "aria-label" label
         , Attr.class "w-11 h-11 flex items-center justify-center rounded-xl bg-slate-900/80 border border-white/10 backdrop-blur-md active:bg-slate-700 text-white"
         ]
            ++ attrs
        )
        [ icon |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [] ]


viewTopButtons :
    { a
        | audioMuted : Bool
        , tilt : { available : Bool, enabled : Bool }
        , actions : HudActions msg
    }
    -> Html msg
viewTopButtons { audioMuted, tilt, actions } =
    div [ Attr.class "flex flex-wrap justify-end gap-1.5 pointer-events-auto shrink-0 max-w-[100px] sm:max-w-none" ]
        [ if tilt.available then
            -- Handled in tilt-steer.js so the permission prompt runs inside the tap.
            button
                [ Attr.attribute "data-tilt-toggle" ""
                , Attr.title
                    (if tilt.enabled then
                        "Tilt steering on (tap for buttons)"

                     else
                        "Tilt steering off (tap to tilt)"
                    )
                , Attr.attribute "aria-pressed"
                    (if tilt.enabled then
                        "true"

                     else
                        "false"
                    )
                , Attr.class
                    ("w-11 h-11 flex items-center justify-center rounded-xl border border-white/10 backdrop-blur-md "
                        ++ (if tilt.enabled then
                                "bg-sky-500 text-slate-950"

                            else
                                "bg-slate-900/80 text-white"
                           )
                    )
                ]
                [ FeatherIcons.smartphone |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [] ]

          else
            text ""
        , iconButton [ Events.onClick actions.onToggleMute ]
            (if audioMuted then
                "Unmute"

             else
                "Mute"
            )
            (if audioMuted then
                FeatherIcons.volumeX

             else
                FeatherIcons.volume2
            )
        , iconButton [ Events.onClick actions.onRestart ] "Restart race" FeatherIcons.rotateCcw
        , iconButton [ Events.onClick actions.onExit ] "Back to boats" FeatherIcons.anchor
        ]


holdEvents : msg -> msg -> List (Html.Attribute msg)
holdEvents press release =
    let
        prevent m =
            Events.preventDefaultOn m << Decode.succeed << (\x -> ( x, True ))
    in
    [ prevent "pointerdown" press
    , Events.on "pointerup" (Decode.succeed release)
    , Events.on "pointercancel" (Decode.succeed release)
    , Events.on "pointerleave" (Decode.succeed release)
    , Events.on "lostpointercapture" (Decode.succeed release)
    , Events.preventDefaultOn "contextmenu" (Decode.succeed ( release, True ))
    ]


steerButton : HudActions msg -> Side -> Html msg
steerButton actions side =
    let
        ( label, icon ) =
            case side of
                Left ->
                    ( "Steer left", FeatherIcons.chevronLeft )

                Right ->
                    ( "Steer right", FeatherIcons.chevronRight )
    in
    button
        ([ Attr.title label
         , Attr.attribute "aria-label" label
         , Attr.attribute "data-steer"
            (case side of
                Left ->
                    "left"

                Right ->
                    "right"
            )
         , Attr.style "touch-action" "none"
         , Attr.class "w-[72px] h-[72px] flex items-center justify-center rounded-2xl bg-slate-900/75 border-2 border-white/20 backdrop-blur-md active:bg-sky-600 active:border-sky-300 text-white shadow-lg"
         ]
            ++ holdEvents (actions.onSteer side True) (actions.onSteer side False)
        )
        [ icon |> FeatherIcons.withSize 40 |> FeatherIcons.toHtml [] ]


fireButton : { a | cannonReady : Bool, targetInRange : Bool, actions : HudActions msg } -> Html msg
fireButton { cannonReady, targetInRange, actions } =
    button
        [ Attr.title "Fire cannons (Space)"
        , Attr.attribute "aria-label" "Fire cannons"
        , Attr.attribute "data-fire" ""
        , Attr.style "touch-action" "none"
        , Events.preventDefaultOn "pointerdown" (Decode.succeed ( actions.onFire, True ))
        , Attr.class
            ("w-[84px] h-[84px] rounded-full flex flex-col items-center justify-center font-black text-xs tracking-wider border-4 shadow-xl transition-transform active:scale-95 "
                ++ (if not cannonReady then
                        "bg-slate-800/90 border-slate-600 text-slate-400"

                    else if targetInRange then
                        "bg-rose-500 border-amber-300 text-white animate-pulse"

                    else
                        "bg-rose-600/90 border-rose-300/60 text-white"
                   )
            )
        ]
        [ FeatherIcons.target |> FeatherIcons.withSize 28 |> FeatherIcons.toHtml []
        , text
            (if cannonReady then
                "FIRE"

             else
                "LOAD"
            )
        ]


viewToast : Maybe String -> Html msg
viewToast toast =
    case toast of
        Just msg ->
            div [ Attr.class "absolute left-0 right-0 top-[38%] flex justify-center" ]
                [ span [ Attr.class "px-4 py-1.5 rounded-full bg-slate-950/70 text-amber-300 font-black text-lg tracking-wide shadow-lg" ]
                    [ text msg ]
                ]

        Nothing ->
            text ""


viewCountdown : Maybe Int -> Html msg
viewCountdown countdown =
    case countdown of
        Just n ->
            div [ Attr.class "absolute inset-x-0 top-[18%] landscape:top-[26%] flex flex-col items-center gap-2 text-center" ]
                [ span [ Attr.class "text-7xl font-black text-amber-300 drop-shadow-[0_4px_8px_rgba(0,0,0,0.6)] font-mono" ]
                    [ text
                        (if n > 0 then
                            String.fromInt n

                         else
                            "GO!"
                        )
                    ]
                , span [ Attr.class "px-3 py-1.5 rounded-xl bg-slate-950/70 text-sm font-bold max-w-[280px]" ]
                    [ text "Sail through the glowing buoys in order. Get close to a target and FIRE for -2 s." ]
                ]

        Nothing ->
            text ""
