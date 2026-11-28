module Sails.HUD exposing
    ( GoalKind(..)
    , HudActions
    , HudConfig
    , HudGoal
    , PointOfSail(..)
    , Side(..)
    , formatCounter
    , goalBearing
    , goalFullSizeFrom
    , goalHideWithin
    , goalOrbitOffset
    , goalScale
    , goalScreenAngle
    , healthColours
    , hintReturnSeconds
    , hintVisible
    , mapBadgeText
    , pointOfSail
    , showSteerButtons
    , shownGoals
    , viewHud
    , viewMinimalHealth
    , viewSailBadge
    , viewWrecked
    )

{-| Touch-first race HUD, a glanceable dashboard as in Brick Racer.

Top left: the course title with its map code under it, small (the map
code means the same map in Brick Racer), the race timer as a counter of digit tiles (`mm:ss.d`)
with the gold coins won beside it (the score, `Sails.Coins`), the
boat's speed in knots with the metres to the next harbour (for a few
seconds after docking, that harbour's time in their place, and "Mooring…"
while the boat moors), harbour progress, the hull's
health with single-word hazard warnings, the point of sail and how far the
boat heels. Top right: the icon-only sound toggle; there is no pause. Once
the boat has stopped (sails down, under 1 km/h), an Exit button and a
restart button show under it, the way out of a race. Bottom left: while
tilt steering is off (no sensor, permission refused, or not a phone), big
left/right steering buttons; phones always steer by tilt. Bottom right: the
anchor button. On a phone (`pointer-coarse`) the anchor moves to the bottom
left, above which the steering buttons sit, and a fire button takes the
bottom right. The middle of the screen stays clear for the boat in both
portrait and landscape. The top left starts with the coin counter
(`Sails.Coins`). Steering buttons react to pointer down/up so they can be
held. A boat far off the course gets an arrow back to it.

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
import Svg
import Svg.Attributes as SA


type Side
    = Left
    | Right


{-| The kind of game objective an arrow badge points towards.
-}
type GoalKind
    = HarbourGoal
    | StandardGoal
    | PirateGoal


{-| A navigation goal indicating direction and distance to an objective.
-}
type alias HudGoal =
    { kind : GoalKind
    , angleDeg : Float
    , distanceM : Float
    }


type alias HudActions msg =
    { onSteer : Side -> Bool -> msg
    , onRaiseSail : msg
    , onLowerSail : msg
    , onToggleMute : msg
    , onToggleHelp : msg
    , onRestart : msg
    , onExit : msg
    , onDropAnchor : msg
    , onFire : msg
    }


{-| Everything the race HUD shows (`viewHud`): the run, the boat, the
wind and weather, the controls' state, the goal hints round the boat and
whether they show (`hintVisible`, see the function of that name), and the
messages its buttons send.
-}
type alias HudConfig msg =
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
    , goals : List HudGoal
    , hintVisible : Bool
    , actions : HudActions msg
    }


viewHud :
    HudConfig msg
    -> Html msg
viewHud cfg =
    div
        [ Attr.class "absolute inset-0 z-20 pointer-events-none select-none overflow-hidden text-white"
        , Attr.attribute "data-testid" "hud-overlay"
        , Attr.style "padding" "max(8px, env(safe-area-inset-top)) max(8px, env(safe-area-inset-right)) max(10px, env(safe-area-inset-bottom)) max(8px, env(safe-area-inset-left))"
        ]
        [ viewFlash cfg.flash
        , div [ Attr.class "relative w-full h-full" ]
            [ div
                [ Attr.class "absolute top-0 left-0 right-0 z-30 flex justify-between items-start gap-2 pointer-events-none"
                , Attr.attribute "data-testid" "hud-top-bar"
                ]
                [ viewTopLeftButtons cfg
                , viewTopRightButtons cfg
                ]
            , viewToast cfg.toast
            , viewCountdown cfg.language cfg.countdown
            , div
                [ Attr.class "absolute bottom-0 left-0 flex flex-col items-start gap-2 pointer-events-auto pointer-coarse:bottom-24"
                , Attr.attribute "data-testid" "hud-steer-controls"
                ]
                [ if showSteerButtons cfg.tilt then
                    div [ Attr.class "flex gap-3" ]
                        [ steerButton cfg.language cfg.actions Left
                        , steerButton cfg.language cfg.actions Right
                        ]

                  else
                    text ""
                ]
            , div
                [ Attr.class "absolute inset-x-0 bottom-0 flex items-end justify-end gap-2 pointer-events-none pointer-coarse:justify-between"
                , Attr.attribute "data-testid" "hud-action-container"
                ]
                [ anchorButton cfg
                , fireButton cfg
                ]
            , viewHudGoals cfg.hintVisible cfg.goals
            , viewHelpModal cfg.showHelp cfg.actions.onToggleHelp
            ]
        ]


{-| Seconds of sailing without a shot before the goal hints come back.
-}
hintReturnSeconds : Float
hintReturnSeconds =
    3


{-| Whether the goal hints round the boat show: not while the player is
shooting (`sinceShot`, the seconds since the last shot, under
`hintReturnSeconds`), again once at anchor or after that long without a
shot.
-}
hintVisible : { sinceShot : Float, anchored : Bool } -> Bool
hintVisible { sinceShot, anchored } =
    anchored || sinceShot >= hintReturnSeconds


{-| Floating navigation goal badges orbiting near the player's ship.
Each badge indicates an objective with an outward-pointing arrow, an icon describing its meaning,
and the distance in meters. While the player shoots they fade out (`visible`
False, see `hintVisible`) over 0.4 s and fade back in after; they stay in
place while hidden, so nothing moves when they come back.
-}
viewHudGoals : Bool -> List HudGoal -> Html msg
viewHudGoals visible allGoals =
    let
        goals =
            shownGoals allGoals
    in
    if List.isEmpty goals then
        text ""

    else
        div
            [ Attr.class "absolute inset-0 pointer-events-none overflow-hidden select-none"
            , Attr.attribute "data-testid" "hud-goals"
            , Attr.attribute "data-visible"
                (if visible then
                    "true"

                 else
                    "false"
                )
            , Attr.attribute "aria-hidden"
                (if visible then
                    "false"

                 else
                    "true"
                )
            , Attr.style "opacity"
                (if visible then
                    "1"

                 else
                    "0"
                )
            , Attr.style "transition" "opacity 0.4s ease"
            ]
            (List.indexedMap viewHudGoal goals)


{-| Compass bearing in degrees (0 along +z, 90 along +x, as the boat's
heading) from the ship at `from` to a goal at `to`, on the water. Nothing
when the goal is right on the ship (no direction to point).
-}
goalBearing : { a | x : Float, z : Float } -> { b | x : Float, z : Float } -> Maybe Float
goalBearing from to =
    let
        dx =
            to.x - from.x

        dz =
            to.z - from.z
    in
    if dx * dx + dz * dz < 1.0e-6 then
        Nothing

    else
        Just (atan2 dx dz * 180 / pi)


{-| The screen angle (degrees clockwise from straight up, in [-180, 180))
of a goal at compass bearing `goal` seen by a camera looking along compass
bearing `camera`. The compass grows to the left on screen (steering right
lowers the heading, see `Sails.Physics.step`), so this is camera minus goal,
the same turn as the wind arrow (`cameraYaw - wind.direction`). It wraps
across -180/180, so a goal behind the camera points down.
-}
goalScreenAngle : Float -> Float -> Float
goalScreenAngle camera goal =
    Physics.angleDiff camera goal


{-| Metres within which a goal's badge hides: the boat is on it (the
on-water arrows of `Sails.Render` hide within the same range).
-}
goalHideWithin : Float
goalHideWithin =
    8


{-| Metres from which a goal's badge shows at full size; nearer it shrinks.
-}
goalFullSizeFrom : Float
goalFullSizeFrom =
    40


{-| The badge's scale at a goal's distance: Nothing (hidden) within
`goalHideWithin`, shrinking to 0.7 as the boat closes in from
`goalFullSizeFrom`, 1 beyond.
-}
goalScale : Float -> Maybe Float
goalScale distanceM =
    if distanceM < goalHideWithin then
        Nothing

    else
        Just (clamp 0.7 1 (0.7 + 0.3 * (distanceM - goalHideWithin) / (goalFullSizeFrom - goalHideWithin)))


{-| The goals that get a badge, closest first (it takes the innermost orbit
and `data-nearest`): goals within `goalHideWithin` are left out; equal
distances keep their given order (harbour, standard, pirate).
-}
shownGoals : List HudGoal -> List HudGoal
shownGoals goals =
    goals
        |> List.filter (\g -> goalScale g.distanceM /= Nothing)
        |> List.sortBy .distanceM


{-| Pixel offset of the `index`th badge from the boat (50% 56% of the
screen) for a screen angle: on an orbit round the boat, staggered outward
by index so two goals on a similar bearing do not overlap.
-}
goalOrbitOffset : Int -> Float -> { x : Float, y : Float }
goalOrbitOffset index angleDeg =
    let
        radius =
            85.0 + toFloat index * 20.0
    in
    { x = radius * sin (degrees angleDeg)
    , y = -radius * cos (degrees angleDeg)
    }


viewHudGoal : Int -> HudGoal -> Html msg
viewHudGoal index goal =
    let
        offset =
            goalOrbitOffset index goal.angleDeg

        scale =
            Maybe.withDefault 1 (goalScale goal.distanceM)

        -- Clamped so a badge never leaves a narrow (phone) screen: its
        -- centre stays 56 px inside the sides and 24 px inside top and bottom.
        offsetX =
            "clamp(calc(56px - 50vw), " ++ String.fromFloat offset.x ++ "px, calc(50vw - 56px))"

        offsetY =
            "clamp(calc(24px - 56vh), " ++ String.fromFloat offset.y ++ "px, calc(44vh - 24px))"

        kindName =
            case goal.kind of
                HarbourGoal ->
                    "harbour"

                StandardGoal ->
                    "standard"

                PirateGoal ->
                    "pirate"

        ( colorClasses, icon ) =
            case goal.kind of
                HarbourGoal ->
                    ( "bg-emerald-950/85 border-emerald-400/80 text-emerald-300 shadow-emerald-950/60"
                    , FeatherIcons.anchor
                    )

                StandardGoal ->
                    ( "bg-amber-950/85 border-amber-400/80 text-amber-300 shadow-amber-950/60"
                    , FeatherIcons.flag
                    )

                PirateGoal ->
                    ( "bg-rose-950/85 border-rose-400/80 text-rose-300 shadow-rose-950/60"
                    , FeatherIcons.crosshair
                    )
    in
    div
        [ Attr.class "absolute flex items-center gap-1.5 px-2 py-0.5 rounded-full border shadow-lg backdrop-blur-sm text-xs font-bold pointer-events-none select-none transition-all duration-75"
        , Attr.class colorClasses

        -- The hint moves every frame: by `transform` only (not `left` and
        -- `top`), so the browser neither lays the HUD out again nor runs a
        -- layout transition each frame; it lands in the same place.
        , Attr.style "left" "50%"
        , Attr.style "top" "56%"
        , Attr.style "transform" ("translate(calc(-50% + " ++ offsetX ++ "), calc(-50% + " ++ offsetY ++ ")) scale(" ++ String.fromFloat scale ++ ")")
        , Attr.attribute "data-testid" "hud-goal"
        , Attr.attribute "data-goal-kind" kindName
        , Attr.attribute "data-nearest"
            (if index == 0 then
                "true"

             else
                "false"
            )
        ]
        [ span
            [ Attr.class "inline-flex items-center justify-center shrink-0"
            , Attr.style "transform" ("rotate(" ++ String.fromFloat goal.angleDeg ++ "deg)")
            ]
            [ FeatherIcons.arrowUp |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml [] ]
        , span
            [ Attr.class "inline-flex items-center justify-center shrink-0" ]
            [ icon |> FeatherIcons.withSize 13 |> FeatherIcons.toHtml [] ]
        , span
            [ Attr.class "font-mono font-black text-[11px] tabular-nums leading-none" ]
            [ text (String.fromInt (round goal.distanceM) ++ "m") ]
        ]


{-| The colours of the top-left hull meter in each health state (see
`Sails.Hull.healthState`): the shield icon's and the bar's classes. Ok keeps
the usual sky shield and green bar; Caution is the brand yellow, Hurt orange
and Danger red (pulsing). All are light enough to read on the meter's dark
slate panel.
-}
healthColours : Hull.HealthState -> { icon : String, bar : String }
healthColours state =
    case state of
        Hull.HealthOk ->
            { icon = "text-sky-300", bar = "bg-emerald-400" }

        Hull.HealthCaution ->
            { icon = "text-brand-yellow", bar = "bg-brand-yellow" }

        Hull.HealthHurt ->
            { icon = "text-orange-400", bar = "bg-orange-500" }

        Hull.HealthDanger ->
            { icon = "text-red-400 animate-pulse", bar = "bg-red-500" }


{-| Sleek, minimal graphical health bar shown in top-left only when the hull
is damaged, coloured by its health state (`healthColours`), which it also
carries in `data-health-state` and tells screen readers in its aria-label.
-}
viewMinimalHealth : Language -> Hull -> Html msg
viewMinimalHealth language hull =
    let
        hp =
            hull.health / Hull.maxHealth

        state =
            Hull.healthState hull

        colours =
            healthColours state

        label =
            T.hullLabel language
                ++ " "
                ++ String.fromInt (Hull.percent hull)
                ++ " %: "
                ++ T.hullStateText language (Hull.healthStateName state)
    in
    if hp < 0.99 then
        div
            [ Attr.class "flex items-center gap-2 px-3 py-1.5 rounded-2xl bg-slate-900/80 border border-white/10 backdrop-blur-md shadow-lg"
            , Attr.attribute "data-testid" "hull-meter"
            , Attr.attribute "data-health-state" (Hull.healthStateName state)
            , Attr.attribute "role" "img"
            , Attr.attribute "aria-label" label
            , Attr.title label
            ]
            [ FeatherIcons.shield
                |> FeatherIcons.withSize 16
                |> FeatherIcons.toHtml [ Attr.class colours.icon ]
            , div [ Attr.class "w-20 h-2 rounded-full bg-slate-700 overflow-hidden" ]
                [ div
                    [ Attr.class ("h-full transition-all duration-300 " ++ colours.bar)
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
        , div [ Attr.class "flex items-center gap-2" ]
            [ viewCounter (formatCounter progress.elapsedSeconds)
            , viewCoins language progress.coins
            ]
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


{-| Top left: the wind (direction and speed), the sail setting, quick exit
icon back to boats shown only when anchored, and minimal hull health.
-}
viewTopLeftButtons :
    { a
        | language : Language
        , hull : Hull
        , isAnchored : Bool
        , actions : HudActions msg
        , wind : Wind
        , cameraYaw : Float
        , sailLevel : Int
        , state : SailingState
        , progress : CourseProgress
    }
    -> Html msg
viewTopLeftButtons { language, hull, isAnchored, actions, wind, cameraYaw, sailLevel, state, progress } =
    div [ Attr.class "flex items-center gap-1.5 pointer-events-auto shrink-0", Attr.attribute "data-testid" "hud-top-left" ]
        (viewCoinBadge language progress.coins
            :: viewWindBadge language cameraYaw wind
            :: viewSailBadge language isAnchored sailLevel state.sail
            :: (if isAnchored then
                    [ iconButton
                        [ Events.onClick actions.onExit ]
                        (T.backToBoats language)
                        FeatherIcons.logOut
                    , viewMinimalHealth language hull
                    ]

                else
                    [ viewMinimalHealth language hull ]
               )
        )


{-| The wind as an arrow (pointing where it blows to, turned to the camera's
view) and its speed in knots.
-}
viewWindBadge : Language -> Float -> Wind -> Html msg
viewWindBadge language cameraYaw wind =
    div
        [ Attr.class (panel ++ " px-2 py-1 flex items-center gap-1.5 text-[11px] font-bold text-sky-200")
        , Attr.attribute "data-testid" "wind-indicator"
        , Attr.title (T.windDirection language)
        ]
        [ span
            [ Attr.class "inline-flex"
            , Attr.style "transform" ("rotate(" ++ String.fromFloat (cameraYaw - wind.direction) ++ "deg)")
            ]
            [ FeatherIcons.arrowUp |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
        , windBars wind.speed
        ]


{-| The sail setting beside the wind: a sail icon (`sailIcon`) and five
rising bars, in the wind badge's style, lit as far as the sail is hoisted.
The level's name (Down, Storm, Reefed, Full, Press, see
`Sails.Physics.sailLevels`) is not shown, only told in the title and the
aria-label and kept in `data-sail-level`. `hoist` is the boat's eased sail
share (0 lowered, 1 full, 1.2 pressed), so the bars follow the sail as it
goes up or down. At anchor the sail shows lowered.
-}
viewSailBadge : Language -> Bool -> Int -> Float -> Html msg
viewSailBadge language isAnchored sailLevel hoist =
    let
        ( name, shownHoist ) =
            if isAnchored then
                ( "Down", 0 )

            else
                ( Tuple.first (Physics.sailLevel sailLevel), hoist )

        lit =
            clamp 0 5 (round (shownHoist / 1.2 * 5))

        label =
            T.sailStatusLabel language ++ ": " ++ T.sailLevelName language name
    in
    div
        [ Attr.class (panel ++ " px-2 py-1 flex items-center gap-1.5 text-[11px] font-bold text-sky-200")
        , Attr.attribute "data-testid" "sail-status"
        , Attr.attribute "data-sail-level" name
        , Attr.title label
        , Attr.attribute "aria-label" label
        ]
        [ span [ Attr.class "inline-flex", Attr.attribute "data-testid" "sail-icon" ]
            [ sailIcon |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]
        , levelBars "sail-bars" lit
        ]


{-| A sail in Feather's line style (Feather has no sail icon; its `wind`,
`navigation`, `anchor` and `flag` mean other things here): a mast, one tall
triangular mainsail from its masthead to the boom and the hull's sheer
under it. Drawn in the text colour, as the wind badge's arrow.
-}
sailIcon : FeatherIcons.Icon
sailIcon =
    FeatherIcons.customIcon
        [ Svg.line [ SA.x1 "11", SA.y1 "2", SA.x2 "11", SA.y2 "18" ] []
        , Svg.path [ SA.d "M11 3 L19 16 L11 16 Z" ] []
        , Svg.path [ SA.d "M3 19 L21 19 L18 22 L6 22 Z" ] []
        ]


{-| The wind's strength as five rising bars: one bar lit for each 3.2 m/s of
wind, so a light breeze lights one or two and a gale lights all five.
-}
windBars : Float -> Html msg
windBars speed =
    let
        lit =
            clamp 1 5 (round (speed / 3.2))
    in
    levelBars "wind-bars" lit


{-| Five rising bars with the first `lit` of them lit, the meter of the wind
and sail badges.
-}
levelBars : String -> Int -> Html msg
levelBars testId lit =
    div [ Attr.class "flex items-end gap-0.5 h-4", Attr.attribute "data-testid" testId ]
        (List.map
            (\bar ->
                div
                    [ Attr.class
                        ("w-1 rounded-sm "
                            ++ (if bar <= lit then
                                    "bg-sky-300"

                                else
                                    "bg-white/15"
                               )
                        )
                    , Attr.style "height" (String.fromInt (bar * 3) ++ "px")
                    ]
                    []
            )
            (List.range 1 5)
        )


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
    div [ Attr.class "flex items-center gap-1.5 pointer-events-auto shrink-0", Attr.attribute "data-testid" "hud-top-right" ]
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


{-| The coin counter at the top left: the coins won so far in a panel of its
own (`viewCoins`), the first thing in the top-left corner.
-}
viewCoinBadge : Language -> Int -> Html msg
viewCoinBadge language coins =
    div [ Attr.class (panel ++ " px-2 py-1 flex items-center") ]
        [ viewCoins language coins ]


{-| The coins won so far, the run's score (`Sails.Coins`): a gold 1x1
round plate seen from above, with its stud, and the count.
-}
viewCoins : Language -> Int -> Html msg
viewCoins language coins =
    div
        [ Attr.class "flex items-center gap-1"
        , Attr.attribute "data-testid" "coins"
        , Attr.title (T.coinsLabel language)
        , Attr.attribute "aria-label" (T.coinsLabel language ++ ": " ++ String.fromInt coins)
        ]
        [ coinIcon
        , span
            [ Attr.class "font-mono font-black inline-flex items-center"
            , Attr.style "height" "1.1rem"
            , Attr.style "line-height" "1.1rem"
            , Attr.style "color" "#f5c542"
            , Attr.style "font-size" "1.05rem"
            , Attr.style "text-shadow" "0 1px 2px rgba(0, 0, 0, 0.6)"
            ]
            [ text (String.fromInt coins) ]
        ]


{-| A gold 1x1 round plate from above: the plate's rim and its stud.
-}
coinIcon : Html msg
coinIcon =
    span
        [ Attr.class "inline-flex items-center justify-center rounded-full"
        , Attr.attribute "aria-hidden" "true"
        , Attr.style "width" "1.1rem"
        , Attr.style "height" "1.1rem"
        , Attr.style "background" "radial-gradient(circle at 35% 30%, #ffe8a3, #d9a520 55%, #a8761a)"
        , Attr.style "border" "1px solid #7a5512"
        , Attr.style "box-shadow" "0 1px 2px rgba(0, 0, 0, 0.5)"
        ]
        [ span
            [ Attr.class "inline-block rounded-full"
            , Attr.style "width" "0.6rem"
            , Attr.style "height" "0.6rem"
            , Attr.style "background" "radial-gradient(circle at 35% 30%, #fff1c4, #e3b33a 60%, #b8861f)"
            , Attr.style "border" "1px solid rgba(122, 85, 18, 0.7)"
            ]
            []
        ]


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
         , Attr.attribute "data-no-click-sfx" ""
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


{-| Anchor button for dropping and raising anchor: bottom right on a computer,
bottom left on a phone (`pointer-coarse`, where the fire button takes the
bottom right). Pulses with bright yellow ring when inside harbour mooring zone.
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
        , Attr.attribute "data-testid" "drop-anchor"
        , Attr.attribute "data-no-click-sfx" ""
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


{-| Fire button for phones, bottom right (`pointer-coarse`; hidden elsewhere,
where the F key and a click or tap on the sea fire). It sends the same
`onFire` as the F key and takes a pointer down, so it fires as the finger
lands; it dims while the guns reload, as the shoot hint did.
-}
fireButton :
    { a
        | cannonReady : Bool
        , language : Language
        , actions : HudActions msg
    }
    -> Html msg
fireButton { cannonReady, language, actions } =
    button
        [ Attr.type_ "button"
        , Attr.title (T.fireTitle language)
        , Attr.attribute "aria-label" (T.fireTitle language)
        , Attr.attribute "data-testid" "fire-button"
        , Attr.attribute "data-no-click-sfx" ""
        , Attr.style "touch-action" "none"
        , Attr.style "-webkit-user-select" "none"
        , Attr.style "user-select" "none"
        , Events.on "pointerdown" (Decode.succeed actions.onFire)
        , Attr.class
            ("hidden pointer-coarse:flex w-20 h-20 items-center justify-center rounded-2xl border-2 font-black text-lg tracking-wide shadow-xl backdrop-blur-md transition-all active:scale-95 pointer-events-auto cursor-pointer "
                ++ (if cannonReady then
                        "bg-brand-red/90 border-brand-red/60 text-white ring-4 ring-brand-red/40"

                    else
                        "bg-slate-800/80 border-slate-600 text-slate-400"
                   )
            )
        ]
        [ text (T.fireButton language cannonReady) ]


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
            div
                [ Attr.class "absolute inset-x-0 top-[14%] landscape:top-[4%] flex justify-center" ]
                [ span [ Attr.class "text-7xl font-black text-amber-300 drop-shadow-[0_4px_8px_rgba(0,0,0,0.6)] font-mono" ]
                    [ text
                        (if n > 0 then
                            String.fromInt n

                         else
                            T.goBanner language
                        )
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
