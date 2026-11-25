module ControlTest exposing (suite)

{-| Tests for the easy-control work: the helm's response, the course
current, the hull's health and the wreck, and the HUD's steering and point
of sail logic.
-}

import Expect
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Boats as Boats
import Sails.HUD as HUD exposing (PointOfSail(..))
import Sails.Hull as Hull
import Sails.Physics as Physics
import Sails.Types exposing (SailingState)
import Sails.Wreck as Wreck
import Test exposing (Test, describe, test)


course : Sails.Types.Course
course =
    Physics.defaultCourse


{-| Open water east of the start, clear of every island.
-}
openWater : SailingState
openWater =
    let
        start =
            Physics.initSailingState course
    in
    { start | position = vec3 0 0 60, heading = 0, speed = Boats.default.topSpeed }


{-| Step the boat with a steering input per frame (0.02 s).
-}
steerFor : List Float -> SailingState -> SailingState
steerFor inputs state0 =
    List.foldl
        (\( i, steer ) s ->
            Physics.stepBoat { dt = 0.02, time = toFloat i * 0.02, steer = steer, throttle = 1, sea = 1, sail = 1 } course.wind Boats.default [] s
        )
        state0
        (List.indexedMap Tuple.pair inputs)


frames : Float -> Float -> List Float
frames seconds steer =
    List.repeat (round (seconds / 0.02)) steer


route : List Vec3.Vec3
route =
    Physics.courseRoute course


suite : Test
suite =
    describe "Easy control"
        [ describe "Helm response"
            [ test "the helm curve is soft in the middle, full at full helm and symmetric" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-9) 1 (Physics.helmCurve 1)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) -1 (Physics.helmCurve -1)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 (Physics.helmCurve 0)
                        , \_ -> Expect.lessThan 0.25 (Physics.helmCurve 0.3)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) (Physics.helmCurve 0.5) -(Physics.helmCurve -0.5)
                        , \_ ->
                            List.map (\i -> Physics.helmCurve (toFloat i / 10)) (List.range 0 10)
                                |> (\xs -> Expect.equal (List.sort xs) xs)
                        ]
                        ()
            , test "full helm at full speed turns at 70 % of the boat's rate, and still steers from a standstill" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-6) (Boats.default.turnRate * 0.7) (Physics.steerResponse Boats.default 1 1)
                        , \_ -> Expect.greaterThan (Boats.default.turnRate * 0.4) (Physics.steerResponse Boats.default 0 1)
                        ]
                        ()
            , test "the helm goes over gently: a quick tap turns the boat only a little" <|
                \_ ->
                    let
                        after =
                            steerFor (frames 0.2 1) openWater
                    in
                    Expect.lessThan 5 (abs (Physics.angleDiff after.heading openWater.heading))
            , test "letting go stops the turn: little overshoot after a tack" <|
                \_ ->
                    let
                        turning =
                            steerFor (frames 1.5 1) openWater

                        settled =
                            steerFor (frames 2 0) turning
                    in
                    Expect.lessThan 8 (abs (Physics.angleDiff settled.heading turning.heading))
            , test "the helm can be reversed quickly" <|
                \_ ->
                    let
                        right =
                            steerFor (frames 1 1) openWater

                        left =
                            steerFor (frames 0.3 -1) right
                    in
                    Expect.lessThan 0 left.steer
            , test "the boat picks up speed quickly off the start line" <|
                \_ ->
                    let
                        start =
                            Physics.initSailingState course

                        moving =
                            steerFor (frames 2 0) { start | position = vec3 0 0 60, heading = 0 }
                    in
                    Expect.greaterThan (Boats.default.topSpeed * 0.75) moving.speed
            , test "the same input always gives the same boat (deterministic)" <|
                \_ ->
                    let
                        run _ =
                            steerFor (frames 1 0.6 ++ frames 1 -0.4) openWater
                    in
                    Expect.equal (run ()) (run ())
            ]
        , describe "Back to the course"
            [ test "the route runs from the start through every buoy" <|
                \_ ->
                    Expect.equal (1 + List.length course.waypoints) (List.length route)
            , test "on the course there is no current" <|
                \_ ->
                    Expect.equal ( 0, 0 ) (Physics.courseCurrent route (vec3 10 0 30))
            , test "on every seat's route the treasure detour is inside the free range" <|
                \_ ->
                    List.range 0 (Physics.routeCount - 1)
                        |> List.map Physics.courseForSeat
                        |> List.concatMap (\seatCourse -> List.map (\t -> Physics.courseCurrent (Physics.courseRoute seatCourse) t.position) seatCourse.treasures)
                        |> Expect.equal (List.repeat Physics.routeCount ( 0, 0 ))
            , test "far off the course the current carries the boat back, gently" <|
                \_ ->
                    let
                        far =
                            vec3 140 0 -60

                        ( vx, vz ) =
                            Physics.courseCurrent route far

                        strength =
                            sqrt (vx * vx + vz * vz)

                        near =
                            Physics.nearestCoursePoint route far |> Maybe.map .point |> Maybe.withDefault far

                        toCourse =
                            Vec3.normalize (Vec3.sub near far)
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 0.5 strength
                        , \_ -> Expect.atMost 3.0 strength
                        , \_ -> Expect.atMost (Boats.default.topSpeed * 0.4) strength
                        , \_ -> Expect.greaterThan 0.99 ((vx * Vec3.getX toCourse + vz * Vec3.getZ toCourse) / strength)
                        ]
                        ()
            , test "a drifting boat comes back within the free range" <|
                \_ ->
                    let
                        far =
                            { openWater | position = vec3 140 0 -60, speed = 0 }

                        drift n s =
                            if n <= 0 then
                                s

                            else
                                drift (n - 1) (Physics.driftWith 0.1 (Physics.courseCurrent route s.position) s)

                        distanceOf s =
                            Physics.nearestCoursePoint route s.position |> Maybe.map .distance |> Maybe.withDefault 0
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 80 (distanceOf far)
                        , \_ -> Expect.atMost (Physics.courseFreeRange + 1) (distanceOf (drift 600 far))
                        ]
                        ()
            ]
        , describe "Hull health"
            [ test "a slow nudge does no harm, a hard knock does" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal 0 (Hull.knockDamage 2)
                        , \_ -> Expect.within (Expect.Absolute 0.5) 20 (Hull.knockDamage 9)
                        ]
                        ()
            , test "four full-speed crashes in a row leave the boat afloat; the fifth wrecks it" <|
                \_ ->
                    let
                        crash h =
                            Hull.step 0.02 True { rock = Just 9, cannonHits = 0, roughSea = 0 } h

                        after n =
                            List.foldl (\_ h -> crash h) Hull.fresh (List.range 1 n)
                    in
                    Expect.all
                        [ \_ -> Expect.equal False (Hull.wrecked (after 4))
                        , \_ -> Expect.equal True (Hull.wrecked (after 5))
                        , \_ -> Expect.equal 0 (Hull.percent (after 5))
                        ]
                        ()
            , test "cannonball hits and storm waves wear the hull down" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.lessThan Hull.maxHealth (Hull.step 0.02 True { rock = Nothing, cannonHits = 1, roughSea = 0 } Hull.fresh).health
                        , \_ -> Expect.lessThan Hull.maxHealth (Hull.step 1 True { rock = Nothing, cannonHits = 0, roughSea = 2 } Hull.fresh).health
                        ]
                        ()
            , test "nothing harms the boat before the start or after the finish" <|
                \_ ->
                    Expect.equal Hull.maxHealth (Hull.step 0.02 False { rock = Just 9, cannonHits = 2, roughSea = 5 } Hull.fresh).health
            , test "the hull mends slowly once left alone" <|
                \_ ->
                    let
                        knocked =
                            Hull.step 0.02 True { rock = Just 9, cannonHits = 0, roughSea = 0 } Hull.fresh

                        later seconds =
                            List.foldl (\_ h -> Hull.step 0.1 True Hull.noKnocks h) knocked (List.range 1 (round (seconds * 10)))
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.01) knocked.health (later 3).health
                        , \_ -> Expect.greaterThan knocked.health (later 8).health
                        , \_ -> Expect.atMost Hull.maxHealth (later 60).health
                        ]
                        ()
            , test "a wrecked hull stays wrecked" <|
                \_ ->
                    let
                        wreck =
                            { health = 0, sinceDamage = 0, lastDamage = 30 }
                    in
                    Expect.equal True (Hull.wrecked (Hull.step 10 True Hull.noKnocks wreck))
            , test "the HUD bands follow the racer's thresholds" <|
                \_ ->
                    let
                        at h =
                            Hull.band { health = h, sinceDamage = 0, lastDamage = 0 }
                    in
                    Expect.equal [ Hull.Sound, Hull.Damaged, Hull.Damaged, Hull.Critical, Hull.Critical ]
                        (List.map at [ 100, 65, 31, 30, 1 ])
            , test "sailing straight onto rock is a hard knock, a glancing touch a soft one" <|
                \_ ->
                    let
                        island =
                            { x = 0, z = 0, radius = 10, height = 4 }

                        approach heading =
                            { openWater | position = vec3 0 0 -12.3, heading = heading, speed = 9, bumped = False }

                        after before =
                            Physics.stepBoat { dt = 0.05, time = 0, steer = 0, throttle = 1, sea = 1, sail = 1 } course.wind Boats.default [ island ] before

                        impact heading =
                            Physics.rockImpact [ island ] (approach heading) (after (approach heading))
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (Maybe.withDefault 0 (impact 0) > 8)
                        , \_ -> Expect.equal True (Maybe.withDefault 0 (impact 80) < 2.5)
                        ]
                        ()
            , test "scraping along the shore after the knock counts as nothing" <|
                \_ ->
                    let
                        island =
                            { x = 0, z = 0, radius = 10, height = 4 }

                        touching =
                            { openWater | position = vec3 0 0 -12, heading = 0, speed = 9, bumped = True }
                    in
                    Expect.equal Nothing (Physics.rockImpact [ island ] touching touching)
            , test "rough seas strain the hull only in a real storm, sailing into the waves" <|
                \_ ->
                    let
                        into =
                            { openWater | heading = Physics.swellHeading + 180 }

                        away =
                            { openWater | heading = Physics.swellHeading }
                    in
                    Expect.all
                        [ \_ -> Expect.equal 0 (Physics.roughSeaStrain 1 0 Boats.default into)
                        , \_ -> Expect.greaterThan 0 (Physics.roughSeaStrain 2.4 0 Boats.default into)
                        , \_ -> Expect.equal 0 (Physics.roughSeaStrain 2.4 0 Boats.default away)
                        ]
                        ()
            ]
        , describe "Wreck"
            [ test "a wrecked boat breaks up into bricks, the same way every time" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal Wreck.pieceCount (List.length (Wreck.breakUp Wreck.OwnBoat openWater))
                        , \_ -> Expect.equal (Wreck.breakUp Wreck.OwnBoat openWater) (Wreck.breakUp Wreck.OwnBoat openWater)
                        ]
                        ()
            , test "the bricks splash down and float" <|
                \_ ->
                    let
                        settled =
                            List.foldl (\i ps -> Wreck.step 0.02 1 (toFloat i * 0.02) ps) (Wreck.breakUp Wreck.OwnBoat openWater) (List.range 1 200)
                    in
                    Expect.equal True (List.all .afloat settled)
            ]
        , describe "HUD"
            [ test "the steering buttons hide while tilt steering is on" <|
                \_ ->
                    Expect.equal [ False, True, True, True ]
                        (List.map HUD.showSteerButtons
                            [ { available = True, enabled = True }
                            , { available = True, enabled = False }
                            , { available = False, enabled = True }
                            , { available = False, enabled = False }
                            ]
                        )
            , test "the race counter shows minutes, seconds and tenths" <|
                \_ ->
                    Expect.equal [ "00:00.0", "01:23.4", "10:05.9" ]
                        (List.map HUD.formatCounter [ 0, 83.47, 605.99 ])
            , test "the point of sail tells into the wind, close-hauled, reach and run apart" <|
                \_ ->
                    let
                        from =
                            course.wind.direction + 180
                    in
                    Expect.equal [ InIrons, CloseHauled, Reaching, Running ]
                        (List.map (\off -> HUD.pointOfSail course.wind (from + off)) [ 10, 60, 100, 170 ])
            ]
        ]
