module GhostShipTest exposing (suite)

{-| Tests for Phase 4: Ghost Ship squall encounter and draft-sensitive reef dynamics.
-}

import Expect
import I18n exposing (Language(..))
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.GhostShip as GhostShip
import Sails.Hull as Hull
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.Types exposing (CourseProgress, DecorKind(..), Faction(..), SailingState)
import Sails.Weather as Weather
import Sails.Wreck as Wreck
import Test exposing (Test, describe, test)


{-| The coral reefs of the default course.
-}
reefs : List Vec3.Vec3
reefs =
    Physics.reefsOf Archipelago.defaultCourse


{-| The first of them.
-}
reef : Vec3.Vec3
reef =
    List.head reefs |> Maybe.withDefault (vec3 0 0 0)


suite : Test
suite =
    describe "Phase 4: Ghost Ship & Coral Reef Draft Dynamics"
        [ describe "Coral Reef Draft Dynamics"
            [ test "the course's coral reefs are its reef decoration, 2 to 4 shoals of 6.0 m radius" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.atLeast 2 (List.length reefs)
                        , \_ -> Expect.atMost 4 (List.length reefs)
                        , \_ -> Expect.equal 6.0 Physics.coralReefRadius
                        , \_ -> Expect.equal 1.0 Physics.reefDraftLimit
                        , \_ ->
                            Expect.equal
                                (List.filterMap
                                    (\d ->
                                        if d.kind == Reef then
                                            Just (vec3 d.x 0 d.z)

                                        else
                                            Nothing
                                    )
                                    Archipelago.defaultCourse.decor
                                )
                                reefs
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
                            Physics.initSailingState Archipelago.defaultCourse
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
                            Physics.initSailingState Archipelago.defaultCourse

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
                            Physics.initSailingState Archipelago.defaultCourse

                        before =
                            { base | position = Vec3.add reef (vec3 0 0 10), speed = 8.0 }

                        after =
                            { base | position = reef, speed = 8.0 }
                    in
                    case Physics.checkReefGrounding reefs sloop before after of
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
                            Physics.initSailingState Archipelago.defaultCourse

                        before =
                            { base | position = Vec3.add reef (vec3 0 0 10), speed = 8.0 }

                        after =
                            { base | position = reef, speed = 8.0 }
                    in
                    Expect.equal Nothing (Physics.checkReefGrounding reefs cutter before after)
            , test "light boat under Keel Burden grounds when passing through coral reef" <|
                \_ ->
                    let
                        cutter =
                            Boats.byName "Cutter"

                        base =
                            Physics.initSailingState Archipelago.defaultCourse

                        before =
                            { base | position = Vec3.add reef (vec3 0 0 10), speed = 8.0, carriedStandard = Just RedCoats }

                        after =
                            { base | position = reef, speed = 8.0, carriedStandard = Just RedCoats }
                    in
                    case Physics.checkReefGrounding reefs cutter before after of
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
                            Physics.initSailingState Archipelago.defaultCourse

                        insideStep1 =
                            { base | position = reef, speed = 5.0 }

                        insideStep2 =
                            { base | position = Vec3.add reef (vec3 0 0 1), speed = 5.0 }
                    in
                    Expect.equal Nothing (Physics.checkReefGrounding reefs sloop insideStep1 insideStep2)
            ]
        , describe "Ghost Ship (Aavelaiva) Encounter"
            [ test "spectral tint is trans-green (vec3 0.18 0.98 0.52)" <|
                \_ ->
                    Expect.equal (vec3 0.18 0.98 0.52) GhostShip.spectralTint
            , test "the patrol circle lies inside the course arena and keeps the old speed" <|
                \_ ->
                    let
                        course =
                            Archipelago.defaultCourse

                        loop =
                            GhostShip.patrolFor course
                    in
                    Expect.all
                        [ \_ -> Expect.atLeast (0.3 * course.arena.radius) loop.radius
                        , \_ -> Expect.atMost course.arena.radius (sqrt ((loop.centreX - course.arena.x) ^ 2 + (loop.centreZ - course.arena.z) ^ 2) + loop.radius)
                        , \_ -> Expect.within (Expect.Absolute 0.01) (2 * pi * 175 / 120) (2 * pi * loop.radius / loop.lapSeconds)
                        ]
                        ()
            , test "does not materialize during fair weather" <|
                \_ ->
                    let
                        fairWeather =
                            Weather.init Weather.stormProfile 42

                        ship =
                            GhostShip.init Archipelago.defaultCourse

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
                            Weather.init Weather.stormProfile 42

                        squallWeather =
                            { base | squall = 0.3 }

                        ship =
                            GhostShip.init Archipelago.defaultCourse

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
                            Weather.init Weather.stormProfile 42

                        thunderWeather =
                            { base | thunder = 0.4 }

                        ship =
                            GhostShip.init Archipelago.defaultCourse

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
                            Weather.init Weather.stormProfile 42

                        stormWeather =
                            { base | squall = 0.5 }

                        activeShip =
                            let
                                ( s, _, _ ) =
                                    GhostShip.step 5.0 stormWeather [] (GhostShip.init Archipelago.defaultCourse)
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
                            Weather.init Weather.stormProfile 42

                        stormWeather =
                            { base | squall = 0.8 }

                        initShip =
                            GhostShip.init Archipelago.defaultCourse

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
                            GhostShip.stateAt (GhostShip.patrolFor Archipelago.defaultCourse) 10.0

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
                            Physics.initProgress Archipelago.defaultCourse

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
