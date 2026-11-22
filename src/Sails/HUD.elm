module Sails.HUD exposing
    ( HudActions
    , viewHudOverlay
    )

{-| Pointer-friendly Tailwind sailing HUD overlay:
interactive wind rose / compass dial, knots speed gauge, point of sail badge,
heel angle meter, waypoint splits, sail sheet trim, and touch rudder controls.
-}

import FeatherIcons
import Game.Compass as Compass exposing (PointOfSail(..))
import Game.Leaderboard as Leaderboard
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Math.Vector3 as Vec3
import Sails.Types
    exposing
        ( CameraMode(..)
        , Course
        , CourseProgress
        , SailingState
        , ShipSpec
        )
import Tailwind as Tw
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Th
import UI.Button as Button


type alias HudActions msg =
    { onToggleMute : msg
    , onCycleCamera : msg
    , onReturnToShipyard : msg
    , onRestartCourse : msg
    , onSteerLeft : msg
    , onSteerRight : msg
    , onSheetIn : msg
    , onSheetOut : msg
    , onTack : msg
    , onFireCannons : msg
    }


{-| Render the complete sailing HUD overlay.
-}
viewHudOverlay :
    { state : SailingState
    , spec : ShipSpec
    , course : Course
    , progress : CourseProgress
    , audioMuted : Bool
    , cameraMode : CameraMode
    , cannonCooldown : Float
    , hitsScored : Int
    , actions : HudActions msg
    }
    -> Html msg
viewHudOverlay { state, spec, course, progress, audioMuted, cameraMode, cannonCooldown, hitsScored, actions } =
    div
        [ Attr.class "absolute inset-0 pointer-events-none flex flex-col justify-between p-4 z-20 select-none overflow-hidden"
        ]
        [ -- Top Bar: Header, Course & Waypoint Split, Controls
          div [ Attr.class "flex justify-between items-start w-full pointer-events-auto" ]
            [ -- Left Top: Compass & Wind Rose
              viewWindRose state course
            , -- Center Top: Waypoint & Race Timer & Combat Score
              div [ Attr.class "flex flex-col items-center gap-1.5" ]
                [ viewTimerSplit progress course
                , if hitsScored > 0 then
                    div [ Attr.class "flex items-center gap-1.5 px-3 py-1 rounded-full bg-rose-950/85 border border-rose-500/40 text-rose-300 text-xs font-bold shadow-lg" ]
                        [ FeatherIcons.target |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml []
                        , text (String.fromInt hitsScored ++ " Broadside Hits")
                        ]

                  else
                    text ""
                ]
            , -- Right Top: Controls (Camera, Audio Mute, Shipyard return)
              div [ Attr.class "flex items-center gap-2 bg-slate-900/80 backdrop-blur-md px-3 py-2 rounded-2xl border border-white/10 shadow-lg text-white" ]
                [ button
                    [ onClick actions.onToggleMute
                    , Attr.title (if audioMuted then "Unmute audio" else "Mute audio")
                    , Attr.class "p-2 rounded-xl hover:bg-white/10 transition-colors"
                    ]
                    [ if audioMuted then
                        FeatherIcons.volumeX |> FeatherIcons.toHtml []

                      else
                        FeatherIcons.volume2 |> FeatherIcons.toHtml []
                    ]
                , button
                    [ onClick actions.onCycleCamera
                    , Attr.title "Cycle camera view (Chase / Orbit / Helm)"
                    , Attr.class "p-2 rounded-xl hover:bg-white/10 transition-colors flex items-center gap-1 text-xs font-semibold"
                    ]
                    [ FeatherIcons.camera |> FeatherIcons.toHtml []
                    , span [ Attr.class "hidden sm:inline" ]
                        [ text
                            (case cameraMode of
                                Chase ->
                                    "Chase"

                                Orbit ->
                                    "Orbit"

                                Helm ->
                                    "Helm"
                            )
                        ]
                    ]
                , button
                    [ onClick actions.onReturnToShipyard
                    , Attr.title "Return to Shipyard"
                    , Attr.class "p-2 rounded-xl hover:bg-white/10 transition-colors"
                    ]
                    [ FeatherIcons.anchor |> FeatherIcons.toHtml []
                    ]
                ]
            ]
        , -- Bottom Area: Knotmeter, Heel Meter, Sail Sheet & Rudder Touch Controls
          div [ Attr.class "flex justify-between items-end w-full pointer-events-auto gap-4" ]
            [ -- Bottom Left: Knotmeter & Heel
              viewSpeedAndHeel state
            , -- Bottom Center/Right: Touch Controls (Fire Cannons, Tack, Trim Sheet, Rudder)
              viewTouchControls state cannonCooldown actions
            ]
        ]


{-| Integrated Compass & Wind Rose Widget
-}
viewWindRose : SailingState -> Course -> Html msg
viewWindRose state course =
    let
        shipHeading =
            state.heading

        cardinal =
            Compass.cardinalName shipHeading

        relWind =
            state.apparentWindAngle

        point =
            Compass.pointOfSail relWind

        pointName =
            Compass.pointOfSailName point

        pointBadgeCol =
            case point of
                InIrons ->
                    "bg-red-500/80 text-white"

                CloseHauled ->
                    "bg-sky-500/80 text-white"

                BeamReach ->
                    "bg-emerald-500/80 text-white font-bold"

                BroadReach ->
                    "bg-teal-500/80 text-white"

                Running ->
                    "bg-amber-500/80 text-white"
    in
    div [ Attr.class "flex flex-col gap-1 bg-slate-900/85 backdrop-blur-md p-3 rounded-2xl border border-white/10 shadow-lg text-white min-w-[130px]" ]
        [ div [ Attr.class "flex justify-between items-center text-xs text-slate-400 font-semibold" ]
            [ span [] [ text "HEADING" ]
            , span [ Attr.class "text-sky-400 font-bold" ] [ text (String.fromInt (round shipHeading) ++ "° " ++ cardinal) ]
            ]
        , div [ Attr.class "relative w-20 h-20 mx-auto my-1 flex items-center justify-center border-2 border-slate-700 rounded-full bg-slate-950/60" ]
            [ -- Compass dial needle
              div
                [ Attr.class "absolute w-1 h-16 bg-gradient-to-t from-slate-600 to-red-500 rounded-full transition-transform duration-75"
                , Attr.style "transform" ("rotate(" ++ String.fromFloat -shipHeading ++ "deg)")
                ]
                []
            , -- Apparent wind indicator arrow
              div
                [ Attr.class "absolute w-0.5 h-14 bg-sky-400 rounded-full transition-transform duration-75"
                , Attr.style "transform" ("rotate(" ++ String.fromFloat relWind ++ "deg)")
                ]
                []
            , div [ Attr.class "w-2.5 h-2.5 rounded-full bg-white z-10" ] []
            ]
        , div [ Attr.class ("px-2 py-0.5 rounded-full text-center text-[10px] uppercase font-bold tracking-wider " ++ pointBadgeCol) ]
            [ text pointName ]
        ]


{-| Timer and Waypoint Split Pill
-}
viewTimerSplit : CourseProgress -> Course -> Html msg
viewTimerSplit progress course =
    let
        timeStr =
            Leaderboard.formatRaceTime progress.elapsedSeconds

        currentWp =
            min (progress.currentWaypointIndex + 1) progress.totalWaypoints

        wpStr =
            "Buoy " ++ String.fromInt currentWp ++ " / " ++ String.fromInt progress.totalWaypoints
    in
    div [ Attr.class "flex flex-col items-center bg-slate-900/85 backdrop-blur-md px-4 py-2 rounded-2xl border border-white/10 shadow-lg text-white" ]
        [ span [ Attr.class "font-mono text-2xl font-bold tracking-wider text-amber-400" ]
            [ text timeStr ]
        , div [ Attr.class "flex items-center gap-2 text-xs text-slate-300 font-medium" ]
            [ span [ Attr.class "px-1.5 py-0.5 bg-slate-800 rounded-md text-sky-300 font-semibold" ]
                [ text wpStr ]
            , if progress.finished then
                span [ Attr.class "text-emerald-400 font-bold uppercase" ] [ text "FINISHED!" ]

              else
                span [ Attr.class "text-slate-400" ] [ text course.title ]
            ]
        ]


{-| Speedometer & Heel Angle Display
-}
viewSpeedAndHeel : SailingState -> Html msg
viewSpeedAndHeel state =
    let
        knots =
            state.waterSpeed

        kmh =
            knots * 1.852

        heel =
            round (abs state.heelAngle)

        heelSide =
            if state.heelAngle > 1.5 then
                "Starboard"

            else if state.heelAngle < -1.5 then
                "Port"

            else
                "Even Keel"
    in
    div [ Attr.class "flex flex-col gap-1 bg-slate-900/85 backdrop-blur-md p-3.5 rounded-2xl border border-white/10 shadow-lg text-white min-w-[140px]" ]
        [ div [ Attr.class "flex items-baseline gap-1" ]
            [ span [ Attr.class "font-mono text-3xl font-black text-sky-400" ]
                [ text (String.fromInt (round knots)) ]
            , span [ Attr.class "text-xs font-bold text-slate-400 uppercase" ] [ text "knots" ]
            ]
        , div [ Attr.class "text-[11px] text-slate-400 font-medium" ]
            [ text (String.fromInt (round kmh) ++ " km/h") ]
        , div [ Attr.class "mt-1 pt-1.5 border-t border-slate-800 flex justify-between items-center text-[11px]" ]
            [ span [ Attr.class "text-slate-400" ] [ text "Heel" ]
            , span [ Attr.class "font-mono font-bold text-amber-300" ]
                [ text (String.fromInt heel ++ "° " ++ heelSide) ]
            ]
        ]


{-| Responsive Touch & Button Controls (Fire Cannons, Sheet In/Out, Tack, Rudder)
-}
viewTouchControls : SailingState -> Float -> HudActions msg -> Html msg
viewTouchControls state cannonCooldown actions =
    div [ Attr.class "flex items-center gap-3" ]
        [ -- Cannon Broadside Fire button
          button
            [ onClick actions.onFireCannons
            , Attr.title "Fire broadside naval cannons [Space or KeyF]"
            , Attr.class
                (if cannonCooldown <= 0.0 then
                    "px-3.5 py-2.5 rounded-2xl bg-gradient-to-r from-rose-600 to-red-600 hover:from-rose-500 hover:to-red-500 active:scale-95 transition-all text-white font-black text-xs uppercase shadow-lg shadow-rose-600/30 tracking-wider flex items-center gap-1.5"

                 else
                    "px-3.5 py-2.5 rounded-2xl bg-slate-800 text-slate-400 font-bold text-xs uppercase cursor-not-allowed opacity-70"
                )
            ]
            [ FeatherIcons.target |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml []
            , text
                (if cannonCooldown <= 0.0 then
                    "FIRE"

                 else
                    "RELOAD"
                )
            ]
        , -- Tack button
          button
            [ onClick actions.onTack
            , Attr.title "Tack / Gybe through the wind [KeyT]"
            , Attr.class "px-3.5 py-2.5 rounded-2xl bg-amber-500 hover:bg-amber-400 active:scale-95 transition-all text-slate-950 font-black text-xs uppercase shadow-md tracking-wider"
            ]
            [ text "TACK" ]
        , -- Sail Sheet Trim buttons
          div [ Attr.class "flex flex-col gap-1 bg-slate-900/85 backdrop-blur-md p-1.5 rounded-2xl border border-white/10 shadow-lg" ]
            [ button
                [ onClick actions.onSheetIn
                , Attr.title "Trim sail sheet in (tighter)"
                , Attr.class "px-3 py-1.5 rounded-xl bg-slate-800 hover:bg-slate-700 active:bg-slate-600 transition-colors text-white text-xs font-bold"
                ]
                [ text "SHEET IN" ]
            , button
                [ onClick actions.onSheetOut
                , Attr.title "Ease sail sheet out (looser)"
                , Attr.class "px-3 py-1.5 rounded-xl bg-slate-800 hover:bg-slate-700 active:bg-slate-600 transition-colors text-white text-xs font-bold"
                ]
                [ text "EASE OUT" ]
            ]
        , -- Rudder Helm Buttons
          div [ Attr.class "flex items-center gap-1.5 bg-slate-900/85 backdrop-blur-md p-1.5 rounded-2xl border border-white/10 shadow-lg" ]
            [ button
                [ onClick actions.onSteerLeft
                , Attr.title "Helm Port (Turn Left)"
                , Attr.class "w-11 h-11 flex items-center justify-center rounded-xl bg-slate-800 hover:bg-slate-700 active:bg-slate-600 transition-colors text-white"
                ]
                [ FeatherIcons.chevronLeft |> FeatherIcons.toHtml [] ]
            , button
                [ onClick actions.onSteerRight
                , Attr.title "Helm Starboard (Turn Right)"
                , Attr.class "w-11 h-11 flex items-center justify-center rounded-xl bg-slate-800 hover:bg-slate-700 active:bg-slate-600 transition-colors text-white"
                ]
                [ FeatherIcons.chevronRight |> FeatherIcons.toHtml [] ]
            ]
        ]
