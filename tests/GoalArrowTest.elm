module GoalArrowTest exposing (suite)

{-| The goal badges round the boat (`Sails.HUD.viewHudGoals`): the bearing
they point along on screen, which goals get one and in which order.
-}

import Expect
import I18n exposing (Language(..))
import Math.Vector3 as Vec3 exposing (vec3)
import Pages.Sail as Sail
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.CannonPick as CannonPick
import Sails.HUD as HUD exposing (GoalKind(..))
import Test exposing (Test, describe, test)


goal : GoalKind -> Float -> HUD.HudGoal
goal kind distanceM =
    { kind = kind, angleDeg = 0, distanceM = distanceM }


{-| Where a goal at `goalBearing` (50 m off) shows on an 800 x 600 screen,
seen by a camera 20 m behind the ship looking along `camBearing`, and the
badge's screen angle for it.
-}
seen : Float -> Float -> { angle : Float, screen : Maybe { x : Float, y : Float }, ship : Maybe { x : Float, y : Float } }
seen camBearing goalBearing =
    let
        ship =
            vec3 10 0 -5

        along b d =
            vec3 (d * sin (degrees b)) 0 (d * cos (degrees b))

        cam =
            { eye = Vec3.add ship (Vec3.add (along camBearing -20) (vec3 0 8 0))
            , target = Vec3.add ship (along camBearing 4)
            }

        e =
            Vec3.toRecord cam.eye

        t =
            Vec3.toRecord cam.target

        goalPos =
            Vec3.add ship (along goalBearing 50)

        cameraLooks =
            atan2 (t.x - e.x) (t.z - e.z) * 180 / pi
    in
    { angle =
        HUD.goalBearing (Vec3.toRecord ship) (Vec3.toRecord goalPos)
            |> Maybe.map (HUD.goalScreenAngle cameraLooks)
            |> Maybe.withDefault 999
    , screen = CannonPick.projectToScreen cam 800 600 goalPos
    , ship = CannonPick.projectToScreen cam 800 600 ship
    }


suite : Test
suite =
    describe "Goal arrows"
        [ describe "bearing"
            [ test "compass bearing from the ship: 0 along +z, 90 along +x, -90 along -x" <|
                \_ ->
                    Expect.equal [ Just 0, Just 90, Just -90, Just 180 ]
                        (List.map (HUD.goalBearing { x = 0, z = 0 })
                            [ { x = 0, z = 5 }, { x = 5, z = 0 }, { x = -5, z = 0 }, { x = 0, z = -5 } ]
                        )
            , test "no bearing for a goal right on the ship" <|
                \_ -> Expect.equal Nothing (HUD.goalBearing { x = 3, z = 4 } { x = 3, z = 4 })
            , test "a goal straight ahead of the camera points up" <|
                \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 (HUD.goalScreenAngle 37 37)
            , test "the compass grows to the left on screen: a higher bearing points left" <|
                \_ -> Expect.equal ( -30, 30 ) ( HUD.goalScreenAngle 0 30, HUD.goalScreenAngle 30 0 )
            , test "it wraps across -180/180 the short way" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-9) -20 (HUD.goalScreenAngle 170 -170)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 20 (HUD.goalScreenAngle -170 170)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 10 (HUD.goalScreenAngle 365 355)
                        ]
                        ()
            , test "a goal behind the camera points down" <|
                \_ -> Expect.equal -180 (HUD.goalScreenAngle 0 180)
            , test "the badge's screen angle agrees with where the renderer draws the goal, as the camera orbits" <|
                \_ ->
                    let
                        wrong =
                            List.concatMap
                                (\camB ->
                                    List.filterMap
                                        (\goalB ->
                                            let
                                                s =
                                                    seen camB goalB

                                                rad =
                                                    degrees s.angle
                                            in
                                            case ( s.screen, s.ship ) of
                                                ( Just g, Just b ) ->
                                                    if abs (sin rad) > 0.25 && (g.x > 400) /= (sin rad > 0) then
                                                        Just ( camB, goalB, "left/right" )

                                                    else if cos rad > 0.5 && g.y >= b.y then
                                                        Just ( camB, goalB, "ahead not up" )

                                                    else
                                                        Nothing

                                                _ ->
                                                    -- Behind the camera: the badge points backward.
                                                    if cos rad < 0 then
                                                        Nothing

                                                    else
                                                        Just ( camB, goalB, "behind not down" )
                                        )
                                        (List.map (\i -> toFloat i * 30 - 180) (List.range 0 11))
                                )
                                [ 0, 40, -95, 179, -179 ]
                    in
                    Expect.equal [] wrong
            , test "the badge's orbit offset follows the screen angle (right is +x, up is -y)" <|
                \_ ->
                    let
                        o =
                            HUD.goalOrbitOffset 0 90

                        up =
                            HUD.goalOrbitOffset 0 0
                    in
                    Expect.equal ( True, True ) ( o.x > 84 && abs o.y < 1.0e-6, up.y < -84 && abs up.x < 1.0e-6 )
            ]
        , describe "which goals"
            [ test "closest first, whatever kind" <|
                \_ ->
                    Expect.equal [ PirateGoal, StandardGoal, HarbourGoal ]
                        (List.map .kind (HUD.shownGoals [ goal HarbourGoal 300, goal StandardGoal 120, goal PirateGoal 40 ]))
            , test "equal distances keep the given order (harbour first)" <|
                \_ ->
                    Expect.equal [ HarbourGoal, StandardGoal, PirateGoal ]
                        (List.map .kind (HUD.shownGoals [ goal HarbourGoal 50, goal StandardGoal 50, goal PirateGoal 50 ]))
            , test "a goal the boat is on gets no badge" <|
                \_ ->
                    Expect.equal [ StandardGoal ]
                        (List.map .kind (HUD.shownGoals [ goal HarbourGoal 0, goal StandardGoal 9, goal PirateGoal (HUD.goalHideWithin - 0.1) ]))
            , test "badges shrink as the boat closes in and hide on the goal" <|
                \_ ->
                    Expect.equal [ Nothing, Just 0.7, Just 0.85, Just 1, Just 1 ]
                        (List.map HUD.goalScale [ 0, HUD.goalHideWithin, (HUD.goalHideWithin + HUD.goalFullSizeFrom) / 2, HUD.goalFullSizeFrom, 500 ])
            ]
        , describe "in the game"
            (let
                course =
                    Archipelago.defaultCourse

                fresh =
                    Sail.initSail En Boats.default course True Nothing Nothing { available = False, enabled = False } 800 600

                harbourAt i =
                    List.drop i course.waypoints |> List.head |> Maybe.map .position

                harbourGoal m =
                    Sail.buildHudGoals m |> List.filter (\g -> g.kind == HarbourGoal) |> List.head |> Maybe.map .distanceM

                distanceTo m p =
                    let
                        a =
                            Vec3.toRecord m.state.position

                        b =
                            Vec3.toRecord p
                    in
                    sqrt ((a.x - b.x) ^ 2 + (a.z - b.z) ^ 2)
             in
             [ test "the harbour badge points to the current harbour" <|
                \_ ->
                    case harbourAt 0 of
                        Just h ->
                            Expect.within (Expect.Absolute 1.0e-6) (distanceTo fresh h) (Maybe.withDefault -1 (harbourGoal fresh))

                        Nothing ->
                            Expect.fail "the default course has harbours"
             , test "once a harbour is reached the badge moves on to the next one" <|
                \_ ->
                    let
                        progress =
                            fresh.progress

                        moved =
                            { fresh | progress = { progress | currentWaypointIndex = 1 } }
                    in
                    case harbourAt 1 of
                        Just h ->
                            Expect.within (Expect.Absolute 1.0e-6) (distanceTo moved h) (Maybe.withDefault -1 (harbourGoal moved))

                        Nothing ->
                            Expect.fail "the default course has two harbours"
             , test "no harbour badge on the harbour itself, nor once the course is done" <|
                \_ ->
                    let
                        state =
                            fresh.state

                        progress =
                            fresh.progress

                        onIt =
                            case harbourAt 0 of
                                Just h ->
                                    { fresh | state = { state | position = h } }

                                Nothing ->
                                    fresh

                        done =
                            { fresh | progress = { progress | finished = True } }
                    in
                    Expect.equal ( Nothing, Nothing ) ( harbourGoal onIt, harbourGoal done )
             , test "the goals come closest first" <|
                \_ ->
                    let
                        ds =
                            List.map .distanceM (Sail.buildHudGoals fresh)
                    in
                    Expect.equal (List.sort ds) ds
             ]
            )
        ]
