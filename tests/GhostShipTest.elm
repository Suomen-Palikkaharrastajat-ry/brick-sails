module GhostShipTest exposing (suite)

{-| Tests for Phase 4: Ghost Ship squall encounter and draft-sensitive reef dynamics.
-}

import Expect
import I18n exposing (Language(..))
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Boats as Boats
import Sails.GhostShip as GhostShip
import Sails.Hull as Hull
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.Types exposing (CourseProgress, Faction(..), SailingState)
import Sails.Weather as Weather
import Sails.Wreck as Wreck
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Phase 4: Ghost Ship & Coral Reef Draft Dynamics"
        [ describe "Coral Reef Draft Dynamics"
            [ test "coral reefs are placed at the 3 specific coordinates with 6.0 m radius" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal 3 (List.length Physics.coralReefs)
                        , \_ -> Expect.equal 6.0 Physics.coralReefRadius
                        , \_ -> Expect.equal 1.0 Physics.reefDraftLimit
                        , \_ ->
                            Expect.equal
                                [ vec3 -112 0 250
                                , vec3 -92 0 -42
                                , vec3 205 0 140
                                ]
                                Physics.coralReefs
                        ]
                        ()
            , test "nominal draft: light boats have draft < 1.0 m, deep boats >= 1.0 m" <|
                \_ ->
                    let
                        catamaran =
                            Boats.byName "Catamaran"

                        cutter =
                            Boats.byName "Cutter"

                        sloop =
                            Boats.byName "Sloop"

                        schooner =
                            Boats.byName "Schooner"
                    in
                    Expect.all
                        [ \_ -> Expect.lessThan 1.0 (Physics.boatDraft catamaran)
                        , \_ -> Expect.lessThan 1.0 (Physics.boatDraft cutter)
                        , \_ -> Expect.atLeast 1.0 (Physics.boatDraft sloop)
                        , \_ -> Expect.atLeast 1.0 (Physics.boatDraft schooner)
                        ]
                        ()
            , test "light boat without standard clears reef shallows safely" <|
                \_ ->
                    let
                        cutter =
                            Boats.byName "Cutter"

                        state =
                            Physics.initSailingState Physics.defaultCourse
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (Physics.canClearReef cutter state)
                        , \_ -> Expect.equal False (Physics.isDeepDraft cutter state)
                        , \_ -> Expect.lessThan 1.0 (Physics.effectiveDraft cutter state)
                        ]
                        ()
            , test "Keel Burden: carrying a Regalia standard increases effective draft by 0.5 m, forcing grounding risk" <|
                \_ ->
                    let
                        cutter =
                            Boats.byName "Cutter"

                        baseState =
                            Physics.initSailingState Physics.defaultCourse

                        burdenedState =
                            { baseState | carriedStandard = Just Pirates }
                    in
                    Expect.all
                        [ \_ -> Expect.atLeast 1.0 (Physics.effectiveDraft cutter burdenedState)
                        , \_ -> Expect.equal True (Physics.isDeepDraft cutter burdenedState)
                        , \_ -> Expect.equal False (Physics.canClearReef cutter burdenedState)
                        ]
                        ()
            , test "deep boat (Sloop) grounds on coral reef and suffers rock knock damage" <|
                \_ ->
                    let
                        sloop =
                            Boats.byName "Sloop"

                        base =
                            Physics.initSailingState Physics.defaultCourse

                        before =
                            { base | position = vec3 -112 0 260, speed = 8.0 }

                        after =
                            { base | position = vec3 -112 0 250, speed = 8.0 }
                    in
                    case Physics.checkReefGrounding sloop before after of
                        Just impactSpeed ->
                            let
                                damage =
                                    Hull.knockDamage impactSpeed
                            in
                            Expect.all
                                [ \_ -> Expect.atLeast 8.8 impactSpeed
                                , \_ -> Expect.atLeast 19.0 damage
                                ]
                                ()

                        Nothing ->
                            Expect.fail "Expected deep boat to ground on reef"
            , test "light boat (Cutter) without standard glides safely through coral reef without grounding" <|
                \_ ->
                    let
                        cutter =
                            Boats.byName "Cutter"

                        base =
                            Physics.initSailingState Physics.defaultCourse

                        before =
                            { base | position = vec3 -112 0 260, speed = 8.0 }

                        after =
                            { base | position = vec3 -112 0 250, speed = 8.0 }
                    in
                    Expect.equal Nothing (Physics.checkReefGrounding cutter before after)
            , test "light boat under Keel Burden grounds when passing through coral reef" <|
                \_ ->
                    let
                        cutter =
                            Boats.byName "Cutter"

                        base =
                            Physics.initSailingState Physics.defaultCourse

                        before =
                            { base | position = vec3 205 0 150, speed = 8.0, carriedStandard = Just RedCoats }

                        after =
                            { base | position = vec3 205 0 140, speed = 8.0, carriedStandard = Just RedCoats }
                    in
                    case Physics.checkReefGrounding cutter before after of
                        Just impactSpeed ->
                            Expect.atLeast 8.8 impactSpeed

                        Nothing ->
                            Expect.fail "Expected burdened boat to ground on reef"
            , test "remaining inside reef does not re-ground on consecutive frames" <|
                \_ ->
                    let
                        sloop =
                            Boats.byName "Sloop"

                        base =
                            Physics.initSailingState Physics.defaultCourse

                        insideStep1 =
                            { base | position = vec3 -92 0 -42, speed = 5.0 }

                        insideStep2 =
                            { base | position = vec3 -92 0 -41, speed = 5.0 }
                    in
                    Expect.equal Nothing (Physics.checkReefGrounding sloop insideStep1 insideStep2)
            ]
        , describe "Ghost Ship (Aavelaiva) Encounter"
            [ test "spectral tint is trans-green (vec3 0.18 0.98 0.52)" <|
                \_ ->
                    Expect.equal (vec3 0.18 0.98 0.52) GhostShip.spectralTint
            , test "outer patrol loop has radius 175 m at (0, 140)" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal 175 GhostShip.patrolLoop.radius
                        , \_ -> Expect.equal 0 GhostShip.patrolLoop.centreX
                        , \_ -> Expect.equal 140 GhostShip.patrolLoop.centreZ
                        ]
                        ()
            , test "does not materialize during fair weather" <|
                \_ ->
                    let
                        fairWeather =
                            Weather.init 42

                        ship =
                            GhostShip.init

                        ( updated, appeared, _ ) =
                            GhostShip.step 10.0 fairWeather [] ship
                    in
                    Expect.all
                        [ \_ -> Expect.equal False (GhostShip.isStormActive fairWeather)
                        , \_ -> Expect.equal False updated.active
                        , \_ -> Expect.equal False appeared
                        , \_ -> Expect.equal False (GhostShip.canTarget updated)
                        ]
                        ()
            , test "materializes when squall > 0.25" <|
                \_ ->
                    let
                        base =
                            Weather.init 42

                        squallWeather =
                            { base | squall = 0.3 }

                        ship =
                            GhostShip.init

                        ( updated, appeared, banished ) =
                            GhostShip.step 15.0 squallWeather [] ship
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (GhostShip.isStormActive squallWeather)
                        , \_ -> Expect.equal True updated.active
                        , \_ -> Expect.equal True appeared
                        , \_ -> Expect.equal False banished
                        , \_ -> Expect.equal True (GhostShip.canTarget updated)
                        ]
                        ()
            , test "materializes when thunder > 0.25" <|
                \_ ->
                    let
                        base =
                            Weather.init 42

                        thunderWeather =
                            { base | thunder = 0.4 }

                        ship =
                            GhostShip.init

                        ( updated, appeared, banished ) =
                            GhostShip.step 20.0 thunderWeather [] ship
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (GhostShip.isStormActive thunderWeather)
                        , \_ -> Expect.equal True updated.active
                        , \_ -> Expect.equal True appeared
                        , \_ -> Expect.equal False banished
                        ]
                        ()
            , test "banished when struck by player cannon fire and awards -5.0s bonus" <|
                \_ ->
                    let
                        base =
                            Weather.init 42

                        stormWeather =
                            { base | squall = 0.5 }

                        activeShip =
                            let
                                ( s, _, _ ) =
                                    GhostShip.step 5.0 stormWeather [] GhostShip.init
                            in
                            s

                        ( banishedShip, appeared, justBanished ) =
                            GhostShip.step 6.0 stormWeather [ GhostShip.ghostShipSlot ] activeShip
                    in
                    Expect.all
                        [ \_ -> Expect.equal True justBanished
                        , \_ -> Expect.equal False appeared
                        , \_ -> Expect.equal True banishedShip.banished
                        , \_ -> Expect.equal False banishedShip.active
                        , \_ -> Expect.equal False (GhostShip.canTarget banishedShip)
                        , \_ -> Expect.equal 5.0 GhostShip.ghostShipBonusSeconds
                        ]
                        ()
            , test "once banished, remains banished even if storm continues" <|
                \_ ->
                    let
                        base =
                            Weather.init 42

                        stormWeather =
                            { base | squall = 0.8 }

                        initShip =
                            GhostShip.init

                        banishedShip =
                            { initShip | banished = True, active = False }

                        ( stillBanished, appeared, justBanished ) =
                            GhostShip.step 30.0 stormWeather [] banishedShip
                    in
                    Expect.all
                        [ \_ -> Expect.equal True stillBanished.banished
                        , \_ -> Expect.equal False stillBanished.active
                        , \_ -> Expect.equal False appeared
                        , \_ -> Expect.equal False justBanished
                        ]
                        ()
            , test "explodes into sparkling trans-green brick debris when banished" <|
                \_ ->
                    let
                        state =
                            GhostShip.stateAt 10.0

                        pieces =
                            Wreck.breakUp Wreck.GhostShip state

                        isTransGreenBrick p =
                            case p.look of
                                Wreck.Brick _ colour ->
                                    colour == 20

                                _ ->
                                    False
                    in
                    Expect.all
                        [ \_ -> Expect.equal Wreck.pieceCount (List.length pieces)
                        , \_ -> Expect.equal True (List.all isTransGreenBrick pieces)
                        ]
                        ()
            , test "final time accounts for ghost ship bonus in CourseProgress" <|
                \_ ->
                    let
                        base =
                            Physics.initProgress Physics.defaultCourse

                        progress : CourseProgress
                        progress =
                            { base
                                | elapsedSeconds = 60.0
                                , targetsHit = 2
                                , treasureBonus = GhostShip.ghostShipBonusSeconds
                                , finished = True
                            }

                        -- 60.0 - (2 * 2.0 targets) - 5.0 ghost ship bonus = 51.0
                        expectedTime =
                            51.0
                    in
                    Expect.equal expectedTime (Physics.finalTime progress)
            ]
        , describe "Localization Phrases"
            [ test "reef scrape toast" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Riutta raapii köliä!" (T.reefScrapeToast Fi)
                        , \_ -> Expect.equal "The reef scrapes your keel!" (T.reefScrapeToast En)
                        ]
                        ()
            , test "ghost ship appeared toast" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Aavelaiva nousee sumusta!" (T.ghostShipAppearedToast Fi)
                        , \_ -> Expect.equal "The Ghost Ship rises from the mist!" (T.ghostShipAppearedToast En)
                        ]
                        ()
            , test "ghost ship banished toast" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Aavelaiva karkotettu!" (T.ghostShipBanishedToast Fi)
                        , \_ -> Expect.equal "Ghost Ship banished! -5 s bonus!" (T.ghostShipBanishedToast En)
                        ]
                        ()
            ]
        ]
