module ControlTest exposing (suite)

{-| Tests for the easy-control work: the helm's response, the course
current, the hull's health and the wreck, and the HUD's steering and point
of sail logic.
-}

import Expect
import Game.MapParams exposing (Biome(..), Season(..), WeatherKind(..))
import Html.Attributes as Attr
import I18n exposing (Language(..))
import Math.Vector3 as Vec3 exposing (vec3)
import Pages.Sail as Sail exposing (SailMsg(..))
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.HUD as HUD exposing (PointOfSail(..))
import Sails.Hull as Hull
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.TiltPitch as TiltPitch
import Sails.Types exposing (IslandRole(..), SailingState)
import Sails.Wreck as Wreck
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


course : Sails.Types.Course
course =
    Archipelago.defaultCourse


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


testId : String -> Selector.Selector
testId id =
    Selector.attribute (Attr.attribute "data-testid" id)


{-| The race HUD of the default course with these goal hints, shown or
faded out (`hintVisible`).
-}
hudConfig : Bool -> List HUD.HudGoal -> HUD.HudConfig ()
hudConfig visible goals =
    let
        state =
            Physics.initSailingState course
    in
    { progress = Physics.initProgress course
    , course = course
    , state = state
    , cameraYaw = 0
    , cannonReady = True
    , touchShoot = False
    , audioMuted = False
    , tilt = { available = False, enabled = False }
    , countdown = Nothing
    , toast = Nothing
    , sailLevel = Physics.defaultSailLevel
    , wind = course.wind
    , weather = Nothing
    , flash = 0
    , hull = Hull.fresh
    , hazards = { rocks = False, pounding = False }
    , stopped = False
    , nextHarbourDistance = Nothing
    , courseBearing = Nothing
    , language = En
    , canAnchor = False
    , isAnchored = False
    , showHelp = False
    , goals = goals
    , hintVisible = visible
    , actions =
        { onSteer = \_ _ -> ()
        , onRaiseSail = ()
        , onLowerSail = ()
        , onToggleMute = ()
        , onToggleHelp = ()
        , onRestart = ()
        , onExit = ()
        , onDropAnchor = ()
        , onFire = ()
        }
    }


ownBoat : Wreck.Source
ownBoat =
    Wreck.Ship { name = Boats.default.name, scale = 1 }


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
                    let
                        seats =
                            Archipelago.seatCourses (Archipelago.generate Archipelago.defaultMap)
                    in
                    seats
                        |> List.concatMap (\seatCourse -> List.map (\t -> Physics.courseCurrent (Physics.courseRoute seatCourse) t.position) seatCourse.treasures)
                        |> Expect.equal (List.repeat (List.length seats) ( 0, 0 ))
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
                        , \_ -> Expect.atMost (3.0 + 1.0e-9) strength
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
                            { x = 0, z = 0, radius = 10, height = 4, role = HarbourIsle }

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
                            { x = 0, z = 0, radius = 10, height = 4, role = HarbourIsle }

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
                        [ \_ -> Expect.equal Wreck.pieceCount (List.length (Wreck.breakUp ownBoat openWater))
                        , \_ -> Expect.equal (Wreck.breakUp ownBoat openWater) (Wreck.breakUp ownBoat openWater)
                        ]
                        ()
            , test "the bricks splash down and float" <|
                \_ ->
                    let
                        settled =
                            List.foldl (\i ps -> Wreck.step 0.02 1 (toFloat i * 0.02) ps) (Wreck.breakUp ownBoat openWater) (List.range 1 200)
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
            , test "the coin counter sits at the top left and shows the coins won" <|
                \_ ->
                    let
                        config =
                            hudConfig True []

                        progress =
                            config.progress
                    in
                    Query.fromHtml (HUD.viewHud { config | progress = { progress | coins = 7 } })
                        |> Query.find [ testId "hud-top-left" ]
                        |> Query.find [ testId "coins" ]
                        |> Query.has [ Selector.text "7" ]
            , test "the fire button is an icon with its title as the accessible label" <|
                \_ ->
                    Query.fromHtml (HUD.viewHud (hudConfig True []))
                        |> Query.find [ testId "fire-button" ]
                        |> Query.has [ Selector.attribute (Attr.attribute "aria-label" (T.fireTitle En)) ]
            , test "the race counter shows minutes, seconds and tenths" <|
                \_ ->
                    Expect.equal [ "00:00.0", "01:23.4", "10:05.9" ]
                        (List.map HUD.formatCounter [ 0, 83.47, 605.99 ])
            , test "the map badge shows the course title and the map code" <|
                \_ ->
                    let
                        map =
                            { biome = Boreal, season = Winter, weather = Snow, seed = 48213 }

                        borealCourse =
                            Archipelago.courseFor (Archipelago.generate map) 0
                    in
                    Expect.equal ( "Boreal · Winter · Snow #48213", "boreal-winter-snow-48213" )
                        (HUD.mapBadgeText En borealCourse)
            , test "the point of sail tells into the wind, close-hauled, reach and run apart" <|
                \_ ->
                    let
                        from =
                            course.wind.direction + 180
                    in
                    Expect.equal [ InIrons, CloseHauled, Reaching, Running ]
                        (List.map (\off -> HUD.pointOfSail course.wind (from + off)) [ 10, 60, 100, 170 ])
            , test "the sail badge shows the sail icon and the bars, not the level's name" <|
                \_ ->
                    let
                        badge language level =
                            Query.fromHtml (HUD.viewSailBadge language False level 1.0)

                        names language =
                            List.map (\( name, _ ) -> T.sailLevelName language name) Physics.sailLevels
                    in
                    [ En, Fi ]
                        |> List.concatMap (\language -> List.map (\level -> ( language, level )) (List.range 0 (List.length Physics.sailLevels - 1)))
                        |> List.map
                            (\( language, level ) ->
                                Expect.all
                                    ([ Query.has [ testId "sail-status", Selector.attribute (Attr.attribute "data-sail-level" (Tuple.first (Physics.sailLevel level))) ]
                                     , Query.find [ testId "sail-bars" ] >> Query.children [] >> Query.count (Expect.equal 5)
                                     , Query.find [ testId "sail-icon" ] >> Query.has [ Selector.tag "svg" ]
                                     ]
                                        ++ List.map (\name -> Query.hasNot [ Selector.text name ]) (names language)
                                    )
                                    (badge language level)
                            )
                        |> List.map (\e -> \() -> e)
                        |> (\checks -> Expect.all checks ())
            , test "the sail badge at anchor reads lowered in its title, still without the name on show" <|
                \_ ->
                    Query.fromHtml (HUD.viewSailBadge En True 3 1.0)
                        |> Expect.all
                            [ Query.has [ Selector.attribute (Attr.attribute "data-sail-level" "Down") ]
                            , Query.has [ Selector.attribute (Attr.attribute "aria-label" (T.sailStatusLabel En ++ ": " ++ T.sailLevelName En "Down")) ]
                            , Query.hasNot [ Selector.text (T.sailLevelName En "Down") ]
                            ]
            ]
        , describe "Goal hints"
            [ test "the hint is hidden right after a shot and while shooting" <|
                \_ ->
                    List.map (\t -> HUD.hintVisible { sinceShot = t, anchored = False }) [ 0, 0.4, 1.5, 2.9 ]
                        |> Expect.equal [ False, False, False, False ]
            , test "the hint is back after 3 s of sailing without a shot" <|
                \_ ->
                    List.map (\t -> HUD.hintVisible { sinceShot = t, anchored = False }) [ 3, 4, 60 ]
                        |> Expect.equal [ True, True, True ]
            , test "the hint shows at anchor, even just after a shot" <|
                \_ ->
                    List.map (\t -> HUD.hintVisible { sinceShot = t, anchored = True }) [ 0, 1, 5 ]
                        |> Expect.equal [ True, True, True ]
            , test "in a race: a shot resets the time since the last shot, which then runs with the race" <|
                \_ ->
                    let
                        fresh =
                            Sail.initSail En Boats.default course True Nothing Nothing { available = False, enabled = False } 800 600

                        tick m =
                            Tuple.first (Sail.update (Tick 20) m)

                        shot =
                            Tuple.first (Sail.update FireCannons fresh)

                        later =
                            List.foldl (\_ m -> tick m) shot (List.range 1 160)

                        visible m =
                            HUD.hintVisible { sinceShot = m.sinceShot, anchored = m.anchored }
                    in
                    Expect.equal ( ( True, False ), ( 0, True ) )
                        ( ( visible fresh, visible shot )
                        , ( shot.sinceShot, visible later && later.sinceShot > 3 )
                        )
            , test "the goal hints fade by opacity over 0.4 s and stay in place" <|
                \_ ->
                    let
                        goals =
                            [ { kind = HUD.HarbourGoal, angleDeg = 30, distanceM = 120 } ]

                        sea visible =
                            Query.fromHtml (HUD.viewHud (hudConfig visible goals))
                                |> Query.find [ testId "hud-goals" ]
                    in
                    Expect.all
                        [ \_ -> sea True |> Query.has [ Selector.style "opacity" "1", Selector.style "transition" "opacity 0.4s ease" ]
                        , \_ -> sea False |> Query.has [ Selector.style "opacity" "0", Selector.style "transition" "opacity 0.4s ease" ]
                        , \_ -> sea False |> Query.children [] |> Query.count (Expect.equal 1)
                        ]
                        ()
            ]
        , describe "Sail Tilt & Directional Cannons"
            [ test "tilting forward raises sails (Forward zone) and tilting backward lowers sails (Backward zone)" <|
                \_ ->
                    let
                        -- Initial reading calibrates neutral point at 55 degrees
                        neutral =
                            TiltPitch.receive 55.0 TiltPitch.init

                        -- Tipping top of phone away from user (face down) lowers beta (e.g. to 35) -> forward tilt
                        forward =
                            TiltPitch.receive 35.0 neutral

                        -- Tipping top of phone towards user (face up) raises beta (e.g. to 75) -> backward tilt
                        backward =
                            TiltPitch.receive 75.0 neutral
                    in
                    Expect.all
                        [ \_ -> Expect.equal TiltPitch.Level neutral.zone
                        , \_ -> Expect.equal TiltPitch.Forward forward.zone
                        , \_ -> Expect.equal TiltPitch.Backward backward.zone
                        ]
                        ()
            , test "fireDirectional fires a single cannonball towards tapped side and sets broadside bias" <|
                \_ ->
                    let
                        state =
                            openWater

                        ( portBalls, portBias ) =
                            Physics.fireDirectional 0 state -1.0 [] 100

                        ( stbdBalls, stbdBias ) =
                            Physics.fireDirectional 0 state 1.0 [] 200
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (List.length portBalls)
                        , \_ -> Expect.equal 1 (List.length stbdBalls)
                        , \_ -> Expect.lessThan 0 portBias
                        , \_ -> Expect.greaterThan 0 stbdBias
                        , \_ ->
                            case ( List.head portBalls, List.head stbdBalls ) of
                                ( Just pb, Just sb ) ->
                                    -- Velocity vectors should have opposite lateral signs
                                    Expect.notEqual (Vec3.getX pb.velocity) (Vec3.getX sb.velocity)

                                _ ->
                                    Expect.fail "Expected cannonballs"
                        ]
                        ()
            ]
        ]
