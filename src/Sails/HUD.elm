module Sails.HUD exposing
    ( HudActions
    , PointOfSail(..)
    , Side(..)
    , pointOfSail
    , showSteerButtons
    , viewHud
    , viewWrecked
    )

{-| Touch-first race HUD.

Top left: timer, buoy progress, the hull's health, the point of sail and
how far the boat heels. Top right: small icon buttons. Bottom left: a small
tilt button in the corner and, while tilt steering is off, big left/right
steering buttons. Bottom right: the sail control (raise / lower, with the
sail set shown between) and a big FIRE button. The middle of the screen
stays clear for the boat in both portrait and landscape. Steering buttons
react to pointer down/up so they can be held. A boat far off the course gets
an arrow back to it.

-}

import FeatherIcons
import Game.Leaderboard as Leaderboard
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events as Events
import I18n exposing (Language)
import Json.Decode as Decode
import Sails.Hull as Hull exposing (Hull)
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.Types exposing (Course, CourseProgress, SailingState, Wind)


type Side
    = Left
    | Right


type alias HudActions msg =
    { onSteer : Side -> Bool -> msg
    , onFire : msg
    , onRaiseSail : msg
    , onLowerSail : msg
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
    , sailLevel : Int
    , wind : Wind
    , weather : Maybe String
    , flash : Float
    , hull : Hull
    , courseBearing : Maybe Float
    , language : Language
    , actions : HudActions msg
    }
    -> Html msg
viewHud cfg =
    div
        [ Attr.class "absolute inset-0 z-20 pointer-events-none select-none overflow-hidden text-white"
        , Attr.style "padding" "max(8px, env(safe-area-inset-top)) max(8px, env(safe-area-inset-right)) max(10px, env(safe-area-inset-bottom)) max(8px, env(safe-area-inset-left))"
        ]
        [ viewFlash cfg.flash
        , div [ Attr.class "relative w-full h-full" ]
            [ div [ Attr.class "absolute top-0 left-0 right-0 flex justify-between items-start gap-2" ]
                [ viewStatus cfg
                , viewTopButtons cfg
                ]
            , viewToast cfg.toast
            , viewCountdown cfg.language cfg.countdown
            , viewBackToCourse cfg.language cfg.cameraYaw cfg.courseBearing
            , div [ Attr.class "absolute bottom-0 left-0 flex flex-col items-start gap-2 pointer-events-auto" ]
                [ tiltButton cfg.language cfg.tilt
                , if showSteerButtons cfg.tilt then
                    div [ Attr.class "flex gap-3" ]
                        [ steerButton cfg.language cfg.actions Left
                        , steerButton cfg.language cfg.actions Right
                        ]

                  else
                    text ""
                ]
            , div [ Attr.class "absolute bottom-0 right-0 flex items-end gap-2 pointer-events-auto" ]
                [ sailControl cfg
                , fireButton cfg
                ]
            ]
        ]


panel : String
panel =
    "bg-slate-900/80 backdrop-blur-md border border-white/10 shadow-lg rounded-2xl"


viewStatus :
    { a
        | progress : CourseProgress
        , state : SailingState
        , cameraYaw : Float
        , wind : Wind
        , weather : Maybe String
        , hull : Hull
        , language : Language
    }
    -> Html msg
viewStatus { progress, state, cameraYaw, wind, weather, hull, language } =
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
            cameraYaw - wind.direction

        point =
            pointOfSail wind state.heading

        powerLabel =
            ( T.pointOfSailText language
                { inIrons = point == InIrons
                , closeHauled = point == CloseHauled
                , reaching = point == Reaching
                }
            , case point of
                InIrons ->
                    "text-amber-300"

                CloseHauled ->
                    "text-sky-300"

                Reaching ->
                    "text-emerald-300"

                Running ->
                    "text-sky-300"
            )
    in
    div [ Attr.class (panel ++ " px-3 py-2 flex flex-col gap-1 min-w-0") ]
        [ span [ Attr.class "font-mono text-2xl font-black tracking-wide text-amber-300 leading-none" ]
            [ text (Leaderboard.formatRaceTime progress.elapsedSeconds) ]
        , viewHealth language hull
        , div [ Attr.class "flex items-center gap-1.5", Attr.attribute "aria-label" (T.buoyAria language) ]
            (span [ Attr.class "text-[11px] font-bold text-slate-300 mr-0.5" ]
                [ text (T.buoyCount language (min total (progress.currentWaypointIndex + 1)) total) ]
                :: List.map dot (List.range 0 (total - 1))
            )
        , div [ Attr.class "flex items-center gap-2 text-[11px] font-bold" ]
            [ span
                [ Attr.class "inline-flex text-sky-300"
                , Attr.style "transform" ("rotate(" ++ String.fromFloat windRotation ++ "deg)")
                , Attr.title (T.windDirection language)
                ]
                [ FeatherIcons.arrowUp |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
            , span [ Attr.class (Tuple.second powerLabel), Attr.attribute "data-testid" "point-of-sail" ] [ text (Tuple.first powerLabel) ]
            , if List.isEmpty progress.treasureFound then
                text ""

              else
                span [ Attr.class "text-amber-300", Attr.attribute "data-testid" "treasure" ]
                    [ text (T.treasureLabel language (List.length progress.treasureFound) (round progress.treasureBonus)) ]
            , if progress.targetsHit > 0 then
                span [ Attr.class "text-rose-300" ]
                    [ text (T.targetsLabel language progress.targetsHit (round (Physics.bonusPerHit * toFloat progress.targetsHit))) ]

              else
                text ""
            ]
        , viewHeel language state.heel
        , case weather of
            Just name ->
                div
                    [ Attr.class "flex items-center gap-1.5 text-[11px] font-black uppercase tracking-wide text-violet-200"
                    , Attr.attribute "data-testid" "weather"
                    ]
                    [ (if name == "Thunderstorm" then
                        FeatherIcons.cloudLightning

                       else if name == "Squall" then
                        FeatherIcons.cloudRain

                       else
                        FeatherIcons.cloud
                      )
                        |> FeatherIcons.withSize 14
                        |> FeatherIcons.toHtml []
                    , text (T.weatherName language name)
                    , span [ Attr.class "text-slate-300 normal-case font-bold" ]
                        [ text (String.fromInt (round (wind.speed * 1.94)) ++ " kn") ]
                    ]

            Nothing ->
                text ""
        ]


{-| The whole screen lights up for a moment when lightning strikes.
-}
viewFlash : Float -> Html msg
viewFlash flash =
    if flash > 0.02 then
        div
            [ Attr.class "absolute inset-0 bg-white"
            , Attr.style "opacity" (String.fromFloat (0.45 * flash))
            ]
            []

    else
        text ""


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
        , language : Language
        , actions : HudActions msg
    }
    -> Html msg
viewTopButtons { audioMuted, language, actions } =
    div [ Attr.class "flex flex-wrap justify-end gap-1.5 pointer-events-auto shrink-0 max-w-[100px] sm:max-w-none" ]
        [ iconButton [ Events.onClick actions.onToggleMute ]
            (T.muteLabel language audioMuted)
            (if audioMuted then
                FeatherIcons.volumeX

             else
                FeatherIcons.volume2
            )
        , iconButton [ Events.onClick actions.onRestart ] (T.restartRace language) FeatherIcons.rotateCcw
        , iconButton [ Events.onClick actions.onExit ] (T.backToBoats language) FeatherIcons.anchor
        ]


{-| The on-screen steering buttons show unless tilt steering is on (and
the phone has the sensor for it).
-}
showSteerButtons : { available : Bool, enabled : Bool } -> Bool
showSteerButtons tilt =
    not (tilt.available && tilt.enabled)


{-| Small corner button switching between tilt and button steering.
Handled in tilt-steer.js so the iOS permission prompt runs inside the tap.
-}
tiltButton : Language -> { available : Bool, enabled : Bool } -> Html msg
tiltButton language tilt =
    if tilt.available then
        button
            [ Attr.attribute "data-tilt-toggle" ""
            , Attr.attribute "data-testid" "tilt-toggle"
            , Attr.title (T.tiltSteeringTitle language tilt.enabled)
            , Attr.attribute "aria-label" (T.tiltSteeringLabel language)
            , Attr.attribute "aria-pressed"
                (if tilt.enabled then
                    "true"

                 else
                    "false"
                )
            , Attr.class
                ("h-9 px-2.5 flex items-center gap-1 rounded-full border text-[10px] font-black uppercase tracking-wide backdrop-blur-md shadow "
                    ++ (if tilt.enabled then
                            "bg-sky-500/90 border-sky-200/60 text-slate-950"

                        else
                            "bg-slate-900/75 border-white/15 text-slate-200"
                       )
                )
            ]
            [ FeatherIcons.smartphone |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml []
            , text (T.tiltLabel language True)
            ]

    else
        text ""


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


steerButton : Language -> HudActions msg -> Side -> Html msg
steerButton language actions side =
    let
        ( label, icon ) =
            case side of
                Left ->
                    ( T.steerLabel language True, FeatherIcons.chevronLeft )

                Right ->
                    ( T.steerLabel language False, FeatherIcons.chevronRight )
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


fireButton : { a | cannonReady : Bool, targetInRange : Bool, language : Language, actions : HudActions msg } -> Html msg
fireButton { cannonReady, targetInRange, language, actions } =
    button
        [ Attr.title (T.fireTitle language ++ " (" ++ T.spaceKey language ++ ")")
        , Attr.attribute "aria-label" (T.fireTitle language)
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
        , text (T.fireButton language cannonReady)
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


viewCountdown : Language -> Maybe Int -> Html msg
viewCountdown language countdown =
    case countdown of
        Just n ->
            -- The number above the boat and the hint below it, so the boat
            -- itself stays in view at the start line. In portrait the hint
            -- sits above the steering and sail buttons (and a room's pill).
            div []
                [ div [ Attr.class "absolute inset-x-0 top-[14%] landscape:top-[4%] flex justify-center" ]
                    [ span [ Attr.class "text-7xl font-black text-amber-300 drop-shadow-[0_4px_8px_rgba(0,0,0,0.6)] font-mono" ]
                        [ text
                            (if n > 0 then
                                String.fromInt n

                             else
                                T.goBanner language
                            )
                        ]
                    ]
                , div [ Attr.class "absolute inset-x-0 bottom-[208px] landscape:bottom-1 flex justify-center" ]
                    [ span [ Attr.class "px-3 py-1.5 rounded-xl bg-slate-950/70 text-sm font-bold max-w-[280px] text-center" ]
                        [ text (T.countdownHint language) ]
                    ]
                ]

        Nothing ->
            text ""


{-| Raise and lower the sails one level per tap (W / S on a keyboard). The
level shows as a row of pips with its name; an overpowered sail flashes a
warning to reef.
-}
sailControl : { a | sailLevel : Int, state : SailingState, wind : Wind, language : Language, actions : HudActions msg } -> Html msg
sailControl { sailLevel, state, wind, language, actions } =
    let
        levels =
            List.length Physics.sailLevels

        ( name, _ ) =
            Physics.sailLevel sailLevel

        overpowered =
            Physics.overpower state.sail wind > 1.2

        tap label key icon msg enabled =
            button
                [ Attr.title (label ++ " (" ++ key ++ ")")
                , Attr.attribute "aria-label" label
                , Attr.attribute "data-sail" key
                , Attr.disabled (not enabled)
                , Attr.style "touch-action" "none"
                , Events.preventDefaultOn "pointerdown" (Decode.succeed ( msg, True ))
                , Attr.class
                    ("w-[60px] h-11 flex items-center justify-center rounded-xl border-2 backdrop-blur-md shadow-lg "
                        ++ (if enabled then
                                "bg-slate-900/75 border-white/20 active:bg-sky-600 active:border-sky-300 text-white"

                            else
                                "bg-slate-900/40 border-white/5 text-slate-600"
                           )
                    )
                ]
                [ icon |> FeatherIcons.withSize 24 |> FeatherIcons.toHtml [] ]

        pip i =
            span
                [ Attr.class
                    ("inline-block w-1.5 h-2.5 rounded-sm "
                        ++ (if i <= sailLevel && sailLevel > 0 then
                                if overpowered then
                                    "bg-rose-400"

                                else
                                    "bg-sky-300"

                            else
                                "bg-slate-600"
                           )
                    )
                ]
                []
    in
    div [ Attr.class "flex flex-col items-center gap-1", Attr.attribute "data-testid" "sail-control" ]
        [ tap (T.sailStep language True) "W" FeatherIcons.chevronsUp actions.onRaiseSail (sailLevel < levels - 1)
        , div
            [ Attr.class
                ("w-[60px] px-1 py-1 rounded-lg flex flex-col items-center gap-0.5 border "
                    ++ (if overpowered then
                            "bg-rose-600/80 border-rose-300 animate-pulse"

                        else
                            "bg-slate-900/80 border-white/10"
                       )
                )
            , Attr.title
                (if overpowered then
                    T.overpoweredTitle language

                 else
                    T.sailLevelTitle language name
                )
            ]
            [ span [ Attr.class "flex gap-0.5 items-end" ] (List.map pip (List.range 1 (levels - 1)))
            , span [ Attr.class "text-[10px] font-black uppercase tracking-wide leading-none" ]
                [ text
                    (if overpowered then
                        T.reefWarning language

                     else
                        T.sailLevelName language name
                    )
                ]
            ]
        , tap (T.sailStep language False) "S" FeatherIcons.chevronsDown actions.onLowerSail (sailLevel > 0)
        ]


{-| The hull's health as a bar with its percentage, green while sound,
amber when damaged and red when critical.
-}
viewHealth : Language -> Hull -> Html msg
viewHealth language hull =
    let
        pct =
            Hull.percent hull

        ( barColour, textColour ) =
            case Hull.band hull of
                Hull.Sound ->
                    ( "bg-emerald-400", "text-emerald-200" )

                Hull.Damaged ->
                    ( "bg-amber-400", "text-amber-200" )

                Hull.Critical ->
                    ( "bg-rose-500 animate-pulse", "text-rose-200" )
    in
    div
        [ Attr.class "flex items-center gap-1.5 text-[11px] font-black"
        , Attr.attribute "data-testid" "hull"
        , Attr.attribute "aria-label" (T.hullLabel language ++ " " ++ String.fromInt pct ++ " %")
        , Attr.title (T.hullTitle language)
        ]
        [ span [ Attr.class "text-slate-300 uppercase tracking-wide" ] [ text (T.hullLabel language) ]
        , span [ Attr.class "relative w-20 h-2.5 rounded-full bg-slate-700 overflow-hidden" ]
            [ span
                [ Attr.class ("absolute inset-y-0 left-0 rounded-full " ++ barColour)
                , Attr.style "width" (String.fromInt pct ++ "%")
                ]
                []
            ]
        , span [ Attr.class ("font-mono " ++ textColour) ] [ text (String.fromInt pct ++ "%") ]
        ]


{-| How far the boat heels, as a level that leans with it: green while it
sails upright, amber when pressed, red when the sail is overpowering it.
-}
viewHeel : Language -> Float -> Html msg
viewHeel language heel =
    let
        a =
            abs heel

        colour =
            if a < 12 then
                "text-emerald-300"

            else if a < 22 then
                "text-amber-300"

            else
                "text-rose-300"
    in
    div
        [ Attr.class ("flex items-center gap-1.5 text-[11px] font-bold " ++ colour)
        , Attr.attribute "data-testid" "heel"
        , Attr.title (T.heelTitle language)
        ]
        [ span
            [ Attr.class "inline-block w-6 h-1 rounded-full bg-current"
            , Attr.style "transform" ("rotate(" ++ String.fromFloat (clamp -40 40 heel) ++ "deg)")
            ]
            []
        , text (T.heelLabel language (round a) (a >= 22))
        ]


{-| Where the wind is coming from, seen from the boat.
-}
type PointOfSail
    = InIrons
    | CloseHauled
    | Reaching
    | Running


{-| The point of sail on a heading: within 40 degrees of the wind the boat
is pinching into it (slow), up to 75 close-hauled, then a reach (the
fastest) to 145, and running before the wind beyond.
-}
pointOfSail : Wind -> Float -> PointOfSail
pointOfSail wind heading =
    let
        offWind =
            abs (Physics.angleDiff heading (wind.direction + 180))
    in
    if offWind < 40 then
        InIrons

    else if offWind < 75 then
        CloseHauled

    else if offWind < 145 then
        Reaching

    else
        Running


{-| An arrow back to the course, while the boat is far off it (the current
carries it back too, see `Sails.Physics.courseCurrent`).
-}
viewBackToCourse : Language -> Float -> Maybe Float -> Html msg
viewBackToCourse language cameraYaw bearing =
    case bearing of
        Just b ->
            div [ Attr.class "absolute left-0 right-0 top-[24%] flex justify-center" ]
                [ span
                    [ Attr.class "flex items-center gap-2 px-3 py-1.5 rounded-full bg-amber-400/90 text-slate-950 font-black text-sm shadow-lg"
                    , Attr.attribute "data-testid" "back-to-course"
                    ]
                    [ span
                        [ Attr.class "inline-flex"
                        , Attr.style "transform" ("rotate(" ++ String.fromFloat (cameraYaw - b) ++ "deg)")
                        ]
                        [ FeatherIcons.arrowUp |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [] ]
                    , text (T.backToCourse language)
                    ]
                ]

        Nothing ->
            text ""


{-| The boat broke up: the run is lost. Sail again or pick another boat.
-}
viewWrecked : Language -> { onRestart : msg, onExit : msg } -> Html msg
viewWrecked language actions =
    div
        [ Attr.class "absolute inset-0 z-30 flex items-center justify-center bg-slate-950/45 pointer-events-auto select-none"
        , Attr.attribute "data-testid" "wrecked"
        , Attr.attribute "role" "alertdialog"
        , Attr.attribute "aria-label" (T.shipwrecked language)
        ]
        [ div [ Attr.class (panel ++ " mx-4 px-5 py-4 flex flex-col items-center gap-3 text-center text-white max-w-sm") ]
            [ span [ Attr.class "text-3xl font-black text-rose-300" ] [ text (T.shipwrecked language) ]
            , span [ Attr.class "text-sm font-bold text-slate-200" ]
                [ text (T.shipwreckedText language) ]
            , div [ Attr.class "flex gap-2" ]
                [ button
                    [ Events.onClick actions.onRestart
                    , Attr.class "px-4 h-11 rounded-xl bg-sky-500 text-slate-950 font-black"
                    ]
                    [ text (T.sailAgain language) ]
                , button
                    [ Events.onClick actions.onExit
                    , Attr.class "px-4 h-11 rounded-xl bg-slate-800 border border-white/10 text-white font-bold"
                    ]
                    [ text (T.backToBoats language) ]
                ]
            ]
        ]
