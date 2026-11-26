module Sails.HUD exposing
    ( HudActions
    , PointOfSail(..)
    , Side(..)
    , formatCounter
    , mapBadgeText
    , pointOfSail
    , showSteerButtons
    , viewHud
    , viewWrecked
    )

{-| Touch-first race HUD, a glanceable dashboard as in Brick Racer.

Top left: the course title with its map code under it, small (the map
code means the same map in Brick Racer), the race timer as a counter of digit tiles (`mm:ss.d`), the
boat's speed in knots with the metres to the next harbour (for a few
seconds after docking, that harbour's time in their place, and "Mooring…"
while the boat moors), harbour progress, the hull's
health with single-word hazard warnings, the point of sail and how far the
boat heels. Top right: the icon-only sound toggle; there is no pause. Once
the boat has stopped (sails down, under 1 km/h), an Exit button and a
restart button show under it, the way out of a race. Bottom left: while
tilt steering is off (no sensor, permission refused, or not a phone), big
left/right steering buttons; phones always steer by tilt. Bottom right: the
sail control (raise / lower, with the sail set shown between) and a hint: tapping or clicking the sea fires. The middle of the screen stays clear for the boat in both
portrait and landscape. Steering buttons react to pointer down/up so they
can be held. A boat far off the course gets an arrow back to it.

-}

import FeatherIcons
import Game.MapParams as MapParams
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
    , onRaiseSail : msg
    , onLowerSail : msg
    , onToggleMute : msg
    , onToggleHelp : msg
    , onRestart : msg
    , onExit : msg
    , onDropAnchor : msg
    }


viewHud :
    { progress : CourseProgress
    , course : Course
    , state : SailingState
    , cameraYaw : Float
    , cannonReady : Bool
    , touchShoot : Bool
    , audioMuted : Bool
    , tilt : { available : Bool, enabled : Bool }
    , countdown : Maybe Int
    , toast : Maybe String
    , sailLevel : Int
    , wind : Wind
    , weather : Maybe String
    , flash : Float
    , hull : Hull
    , hazards : { rocks : Bool, pounding : Bool }
    , stopped : Bool
    , nextHarbourDistance : Maybe Float
    , courseBearing : Maybe Float
    , language : Language
    , canAnchor : Bool
    , isAnchored : Bool
    , showHelp : Bool
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
            [ div [ Attr.class "absolute top-0 left-0 right-0 z-30 flex justify-between items-start gap-2 pointer-events-none" ]
                [ viewTopLeftButtons cfg
                , viewTopRightButtons cfg
                ]
            , viewToast cfg.toast
            , viewCountdown cfg.language cfg.countdown
            , div [ Attr.class "absolute bottom-0 left-0 flex flex-col items-start gap-2 pointer-events-auto" ]
                [ if showSteerButtons cfg.tilt then
                    div [ Attr.class "flex gap-3" ]
                        [ steerButton cfg.language cfg.actions Left
                        , steerButton cfg.language cfg.actions Right
                        ]

                  else
                    text ""
                ]
            , div [ Attr.class "absolute bottom-0 right-0 flex items-end gap-2 pointer-events-auto" ]
                [ anchorButton cfg
                ]
            , viewHelpModal cfg.showHelp cfg.actions.onToggleHelp
            ]
        ]


{-| Sleek, minimal graphical health bar shown in top-left only when the hull is damaged.
-}
viewMinimalHealth : Hull -> Html msg
viewMinimalHealth hull =
    let
        hp =
            hull.health / Hull.maxHealth
    in
    if hp < 0.99 then
        div
            [ Attr.class "flex items-center gap-2 px-3 py-1.5 rounded-2xl bg-slate-900/80 border border-white/10 backdrop-blur-md shadow-lg" ]
            [ FeatherIcons.shield
                |> FeatherIcons.withSize 16
                |> FeatherIcons.toHtml
                    [ Attr.class
                        (if hp < 0.35 then
                            "text-red-400 animate-pulse"

                         else
                            "text-sky-300"
                        )
                    ]
            , div [ Attr.class "w-20 h-2 rounded-full bg-slate-700 overflow-hidden" ]
                [ div
                    [ Attr.class
                        ("h-full transition-all duration-300 "
                            ++ (if hp < 0.35 then
                                    "bg-red-500"

                                else if hp < 0.65 then
                                    "bg-amber-400"

                                else
                                    "bg-emerald-400"
                               )
                        )
                    , Attr.style "width" (String.fromFloat (hp * 100) ++ "%")
                    ]
                    []
                ]
            ]

    else
        text ""


panel : String
panel =
    "bg-brand/85 backdrop-blur-md border border-white/10 shadow-lg rounded-2xl"


viewStatus :
    { a
        | progress : CourseProgress
        , course : Course
        , state : SailingState
        , cameraYaw : Float
        , wind : Wind
        , weather : Maybe String
        , hull : Hull
        , hazards : { rocks : Bool, pounding : Bool }
        , nextHarbourDistance : Maybe Float
        , language : Language
        , canAnchor : Bool
    }
    -> Html msg
viewStatus { progress, course, state, cameraYaw, wind, weather, hull, hazards, nextHarbourDistance, language, canAnchor } =
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
                                "bg-brand-yellow ring-2 ring-brand-yellow/60"

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
        [ viewMapBadge language course
        , viewCounter (formatCounter progress.elapsedSeconds)
        , viewMetrics language progress state nextHarbourDistance canAnchor
        , viewHealth language hull
        , viewHazards language hazards
        , div [ Attr.class "flex items-center gap-1.5", Attr.attribute "aria-label" (T.harbourAria language) ]
            (span [ Attr.class "text-[11px] font-bold text-slate-300 mr-0.5" ]
                [ text (T.harbourCount language (min total (progress.currentWaypointIndex + 1)) total) ]
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


{-| The course title and the map code of the badge, as Brick Racer's race
HUD shows them: for example ( "Boreal · Winter · Snow #48213",
"boreal-winter-snow-48213" ).
-}
mapBadgeText : Language -> Course -> ( String, String )
mapBadgeText language course =
    ( T.mapTitle language course.map, MapParams.toCode course.map )


{-| The small map badge at the top of the status panel: the course title
with the map code under it. It takes no width of its own (width 0, at
least the panel's), so a long title is cut short with an ellipsis instead
of widening the panel on a phone.
-}
viewMapBadge : Language -> Course -> Html msg
viewMapBadge language course =
    let
        ( title, code ) =
            mapBadgeText language course
    in
    div
        [ Attr.class "flex flex-col leading-tight text-[10px]"
        , Attr.style "width" "0"
        , Attr.style "min-width" "100%"
        , Attr.attribute "data-testid" "map-badge"
        , Attr.title (title ++ " (" ++ code ++ ")")
        ]
        [ span [ Attr.class "font-bold text-slate-200 truncate" ] [ text title ]
        , span [ Attr.class "font-mono text-slate-400 truncate" ] [ text code ]
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
        ([ Attr.type_ "button"
         , Attr.title label
         , Attr.attribute "aria-label" label
         , Attr.style "touch-action" "manipulation"
         , Attr.style "-webkit-user-select" "none"
         , Attr.style "user-select" "none"
         , Attr.class "w-11 h-11 flex items-center justify-center rounded-xl bg-slate-900/80 border border-white/10 backdrop-blur-md active:bg-slate-700 text-white cursor-pointer pointer-events-auto shadow-md"
         ]
            ++ attrs
        )
        [ icon |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [] ]


{-| Top left: quick exit back to boats, restart race, and minimal hull health.
Always visible and touch-ready on mobile.
-}
viewTopLeftButtons :
    { a
        | language : Language
        , hull : Hull
        , actions : HudActions msg
    }
    -> Html msg
viewTopLeftButtons { language, hull, actions } =
    div [ Attr.class "flex items-center gap-1.5 pointer-events-auto shrink-0" ]
        [ iconButton
            [ Events.onClick actions.onExit
            , Events.on "pointerdown" (Decode.succeed actions.onExit)
            ]
            (T.backToBoats language)
            FeatherIcons.arrowLeft
        , iconButton
            [ Events.onClick actions.onRestart
            , Events.on "pointerdown" (Decode.succeed actions.onRestart)
            ]
            (T.restartRace language)
            FeatherIcons.rotateCcw
        , viewMinimalHealth hull
        ]


{-| Top right: mute toggle and keyboard/touch control help (? / H).
-}
viewTopRightButtons :
    { a
        | audioMuted : Bool
        , language : Language
        , actions : HudActions msg
    }
    -> Html msg
viewTopRightButtons { audioMuted, language, actions } =
    div [ Attr.class "flex items-center gap-1.5 pointer-events-auto shrink-0" ]
        [ iconButton
            [ Events.onClick actions.onToggleMute
            , Events.on "pointerdown" (Decode.succeed actions.onToggleMute)
            ]
            (T.muteLabel language audioMuted)
            (if audioMuted then
                FeatherIcons.volumeX

             else
                FeatherIcons.volume2
            )
        , iconButton
            [ Events.onClick actions.onToggleHelp
            , Events.on "pointerdown" (Decode.succeed actions.onToggleHelp)
            ]
            "Controls (? / H)"
            FeatherIcons.helpCircle
        ]


{-| The on-screen steering buttons show unless tilt steering is on (and
the phone has the sensor for it): phones always steer by tilt, and the
buttons are the fallback when the sensor is missing or permission is
refused.
-}
showSteerButtons : { available : Bool, enabled : Bool } -> Bool
showSteerButtons tilt =
    not (tilt.available && tilt.enabled)


{-| The race time as the HUD's counter shows it, minutes, seconds and
tenths: `mm:ss.d`.
-}
formatCounter : Float -> String
formatCounter seconds =
    let
        tenths =
            floor (max 0 seconds * 10)

        pad2 n =
            String.padLeft 2 '0' (String.fromInt n)
    in
    pad2 (tenths // 600) ++ ":" ++ pad2 (modBy 60 (tenths // 10)) ++ "." ++ String.fromInt (modBy 10 tenths)


{-| The race timer as a mechanical counter: one dark tile per digit, as in
Brick Racer.
-}
viewCounter : String -> Html msg
viewCounter timeText =
    div [ Attr.class "flex items-center gap-0.5", Attr.attribute "data-testid" "race-timer" ]
        (List.map counterChar (String.toList timeText))


counterChar : Char -> Html msg
counterChar c =
    if Char.isDigit c then
        span
            [ Attr.class "inline-block text-center font-mono font-bold text-amber-300 leading-tight"
            , Attr.style "min-width" "1.05rem"
            , Attr.style "padding" "0.05rem 0.2rem"
            , Attr.style "border-radius" "0.3rem"
            , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
            , Attr.style "background" "#020617"
            , Attr.style "box-shadow" "inset 0 2px 4px rgba(0, 0, 0, 0.6)"
            , Attr.style "font-size" "1.05rem"
            ]
            [ text (String.fromChar c) ]

    else
        span [ Attr.class "font-bold text-slate-400", Attr.style "font-size" "1.1rem" ]
            [ text (String.fromChar c) ]


{-| Seconds a harbour's time stays in place of the metres figure.
-}
harbourTimeSeconds : Float
harbourTimeSeconds =
    3


{-| Under the counter: the boat's speed in knots and the metres to the next
harbour; while the boat moors, a mooring hint; for a few seconds after
docking, that harbour's time instead.
-}
viewMetrics : Language -> CourseProgress -> SailingState -> Maybe Float -> Bool -> Html msg
viewMetrics language progress state nextHarbourDistance canAnchor =
    let
        lastSplit =
            List.reverse progress.splits |> List.head

        recentSplit =
            lastSplit
                |> Maybe.andThen
                    (\t ->
                        if progress.elapsedSeconds - t < harbourTimeSeconds then
                            Just t

                        else
                            Nothing
                    )

        distanceSlot =
            case recentSplit of
                Just t ->
                    span
                        [ Attr.class "text-emerald-300"
                        , Attr.title (T.harbourTimeTitle language (List.length progress.splits))
                        , Attr.attribute "data-testid" "harbour-time"
                        ]
                        [ text (formatCounter t) ]

                Nothing ->
                    if canAnchor then
                        span
                            [ Attr.class "text-amber-300 font-bold animate-pulse flex items-center gap-1"
                            , Attr.attribute "data-testid" "mooring"
                            ]
                            [ FeatherIcons.anchor |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml []
                            , text (T.dropAnchorHint language)
                            ]

                    else if progress.berthSeconds > 0 then
                        span
                            [ Attr.class "text-amber-300"
                            , Attr.attribute "data-testid" "mooring"
                            ]
                            [ text (T.mooringHint language) ]

                    else
                        case nextHarbourDistance of
                            Just d ->
                                span
                                    [ Attr.class "text-slate-200"
                                    , Attr.title (T.nextHarbourTitle language)
                                    , Attr.attribute "data-testid" "harbour-distance"
                                    ]
                                    [ text (String.fromInt (round d) ++ " m") ]

                            Nothing ->
                                text ""
    in
    div [ Attr.class "flex items-center gap-2 text-[11px] font-black font-mono" ]
        [ span
            [ Attr.class "text-sky-300"
            , Attr.title (T.speedKnotsTitle language)
            , Attr.attribute "data-testid" "speed"
            ]
            [ text (knots state.speed) ]
        , distanceSlot
        ]


{-| A speed in m/s as knots with one decimal, "4.2 kn".
-}
knots : Float -> String
knots speed =
    let
        tenths =
            round (abs speed * 1.94 * 10)
    in
    String.fromInt (tenths // 10) ++ "." ++ String.fromInt (modBy 10 tenths) ++ " kn"


{-| Single-word warnings under the hull bar while something is hurting it.
-}
viewHazards : Language -> { rocks : Bool, pounding : Bool } -> Html msg
viewHazards language hazards =
    let
        chip colour label =
            span
                [ Attr.class ("px-1.5 py-0.5 rounded text-[10px] font-black tracking-wide text-slate-950 " ++ colour)
                , Attr.attribute "data-testid" "hazard"
                ]
                [ text label ]

        chips =
            List.filterMap identity
                [ if hazards.rocks then
                    Just (chip "bg-rose-400" (T.hazardRocks language))

                  else
                    Nothing
                , if hazards.pounding then
                    Just (chip "bg-amber-400" (T.hazardPounding language))

                  else
                    Nothing
                ]
    in
    if List.isEmpty chips then
        text ""

    else
        div [ Attr.class "flex flex-wrap items-center gap-1" ] chips


holdEvents : msg -> msg -> List (Html.Attribute msg)
holdEvents press release =
    [ Events.preventDefaultOn "pointerdown" (Decode.succeed ( press, True ))
    , Events.on "pointerup" (Decode.succeed release)
    , Events.on "pointercancel" (Decode.succeed release)
    , Events.preventDefaultOn "touchstart" (Decode.succeed ( press, True ))
    , Events.on "touchend" (Decode.succeed release)
    , Events.on "touchcancel" (Decode.succeed release)
    , Events.preventDefaultOn "mousedown" (Decode.succeed ( press, True ))
    , Events.on "mouseup" (Decode.succeed release)
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
        ([ Attr.type_ "button"
         , Attr.title label
         , Attr.attribute "aria-label" label
         , Attr.attribute "data-steer"
            (case side of
                Left ->
                    "left"

                Right ->
                    "right"
            )
         , Attr.style "touch-action" "none"
         , Attr.style "-webkit-user-select" "none"
         , Attr.style "user-select" "none"
         , Attr.class "w-[72px] h-[72px] flex items-center justify-center rounded-2xl bg-slate-900/80 border-2 border-white/20 backdrop-blur-md active:bg-sky-600 active:border-sky-300 text-white shadow-xl pointer-events-auto cursor-pointer"
         ]
            ++ holdEvents (actions.onSteer side True) (actions.onSteer side False)
        )
        [ icon |> FeatherIcons.withSize 40 |> FeatherIcons.toHtml [] ]


{-| The hint that tapping (or clicking) the sea fires the cannons: "Tap to
shoot" on a touch screen, "Click to shoot" with a mouse. It dims while the
guns reload. It does not catch pointers, so they reach the sea underneath.
-}
shootHint : { a | cannonReady : Bool, touchShoot : Bool, language : Language } -> Html msg
shootHint { cannonReady, touchShoot, language } =
    span
        [ Attr.class
            ("pointer-events-none px-4 py-2 rounded-2xl font-black text-sm tracking-wide border-2 shadow-lg backdrop-blur-md "
                ++ (if cannonReady then
                        "bg-brand-red/90 border-brand-red/60 text-white"

                    else
                        "bg-slate-800/80 border-slate-600 text-slate-400"
                   )
            )
        , Attr.attribute "data-fire" ""
        ]
        [ text (T.shootHint language touchShoot) ]


{-| Anchor button in the bottom right corner for dropping and raising anchor.
Pulses with bright yellow ring when inside harbour mooring zone.
-}
anchorButton :
    { a
        | canAnchor : Bool
        , isAnchored : Bool
        , actions : HudActions msg
    }
    -> Html msg
anchorButton { canAnchor, isAnchored, actions } =
    button
        [ Attr.type_ "button"
        , Events.onClick actions.onDropAnchor
        , Events.on "pointerdown" (Decode.succeed actions.onDropAnchor)
        , Attr.attribute "data-testid" "drop-anchor"
        , Attr.style "touch-action" "manipulation"
        , Attr.style "-webkit-user-select" "none"
        , Attr.style "user-select" "none"
        , Attr.class
            ("w-16 h-16 flex items-center justify-center rounded-2xl border-2 font-black shadow-xl backdrop-blur-md transition-all active:scale-95 pointer-events-auto cursor-pointer "
                ++ (if isAnchored then
                        "bg-amber-500 text-white border-amber-300 shadow-amber-500/40 ring-4 ring-amber-400/50"

                    else if canAnchor then
                        "bg-brand-yellow text-brand border-brand-yellow ring-4 ring-brand-yellow/60 animate-pulse scale-105"

                    else
                        "bg-slate-900/80 border-white/20 text-white active:bg-slate-800"
                   )
            )
        ]
        [ FeatherIcons.anchor
            |> FeatherIcons.withSize 32
            |> FeatherIcons.toHtml []
        ]


{-| Graphic icon keybinding help modal toggled by ? or H or the top-right button.
-}
viewHelpModal : Bool -> msg -> Html msg
viewHelpModal isOpen onClose =
    if isOpen then
        div
            [ Attr.class "absolute inset-0 z-50 flex items-center justify-center bg-black/60 backdrop-blur-sm pointer-events-auto p-4"
            , Events.onClick onClose
            ]
            [ div
                [ Attr.class "relative max-w-sm w-full bg-slate-900/95 border border-white/15 rounded-3xl p-6 shadow-2xl text-white flex flex-col gap-5"
                , Attr.style "box-shadow" "0 25px 50px -12px rgba(0, 0, 0, 0.7)"
                ]
                [ div [ Attr.class "flex items-center justify-between border-b border-white/10 pb-3" ]
                    [ span [ Attr.class "text-lg font-black tracking-wide flex items-center gap-2" ]
                        [ FeatherIcons.compass |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml []
                        , text "Controls"
                        ]
                    , button
                        [ Attr.class "w-8 h-8 rounded-full bg-white/10 flex items-center justify-center active:bg-white/20"
                        , Events.onClick onClose
                        ]
                        [ FeatherIcons.x |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
                    ]
                , div [ Attr.class "grid grid-cols-2 gap-3 text-sm font-semibold" ]
                    [ div [ Attr.class "flex items-center justify-between p-3 rounded-2xl bg-white/5 border border-white/10" ]
                        [ span [ Attr.class "px-2 py-1 rounded-lg bg-white/15 font-mono text-xs font-bold" ] [ text "A / D" ]
                        , FeatherIcons.navigation |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [ Attr.class "text-sky-300" ]
                        ]
                    , div [ Attr.class "flex items-center justify-between p-3 rounded-2xl bg-white/5 border border-white/10" ]
                        [ span [ Attr.class "px-2 py-1 rounded-lg bg-white/15 font-mono text-xs font-bold" ] [ text "Q / E" ]
                        , FeatherIcons.wind |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [ Attr.class "text-sky-300" ]
                        ]
                    , div [ Attr.class "flex items-center justify-between p-3 rounded-2xl bg-white/5 border border-white/10" ]
                        [ span [ Attr.class "px-2 py-1 rounded-lg bg-white/15 font-mono text-xs font-bold" ] [ text "Space" ]
                        , FeatherIcons.anchor |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [ Attr.class "text-amber-300" ]
                        ]
                    , div [ Attr.class "flex items-center justify-between p-3 rounded-2xl bg-white/5 border border-white/10" ]
                        [ span [ Attr.class "px-2 py-1 rounded-lg bg-white/15 font-mono text-xs font-bold" ] [ text "Cannon" ]
                        , FeatherIcons.crosshair |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [ Attr.class "text-red-400" ]
                        ]
                    ]
                , div [ Attr.class "flex items-center justify-between text-xs text-slate-300 bg-white/5 p-3 rounded-xl border border-white/10" ]
                    [ div [ Attr.class "flex items-center gap-2" ]
                        [ FeatherIcons.smartphone |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [ Attr.class "text-sky-300" ]
                        , FeatherIcons.rotateCw |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [ Attr.class "text-slate-400" ]
                        ]
                    , FeatherIcons.anchor |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [ Attr.class "text-amber-300" ]
                    ]
                ]
            ]

    else
        text ""


viewToast : Maybe String -> Html msg
viewToast toast =
    case toast of
        Just msg ->
            div [ Attr.class "absolute left-0 right-0 top-[38%] flex justify-center" ]
                [ span [ Attr.class "px-4 py-1.5 rounded-full bg-brand/80 text-brand-yellow font-black text-lg tracking-wide shadow-lg" ]
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
                    ( "bg-brand-yellow", "text-brand-yellow" )

                Hull.Critical ->
                    ( "bg-brand-red animate-pulse", "text-brand-red" )
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
                    [ Attr.class "flex items-center gap-2 px-3 py-1.5 rounded-full bg-brand-yellow/90 text-brand font-black text-sm shadow-lg"
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
                    , Attr.class "px-4 h-11 rounded-xl bg-brand-yellow text-brand font-black"
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
