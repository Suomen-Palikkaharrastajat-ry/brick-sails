module RegaliaTest exposing (suite)

{-| Comprehensive tests of the Regalia Raid (Valtikkaryöstö) objective mechanics:
seizing enemy standards, keel burden handling penalties, dropping and recovery at sea,
aging/resetting, and repatriation at home citadels.
-}

import Expect
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Boats as Boats
import Sails.Physics as Physics
import Sails.Types
    exposing
        ( Boat
        , Course
        , Faction(..)
        , SailingState
        , StandardState(..)
        , carriedBySlot
        , isAtCitadel
        , isAtSea
        , standardFaction
        , standardForCarrier
        )
import Test exposing (Test, describe, test)


course : Course
course =
    Physics.defaultCourse


sloop : Boat
sloop =
    Boats.default


sailingState : SailingState
sailingState =
    Physics.initSailingState course


suite : Test
suite =
    describe "Regalia Raid (War of the Three Ensigns CTF)"
        [ describe "standard state and helper functions"
            [ test "standardFaction returns the faction for any standard state" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal Pirates (standardFaction (AtCitadel Pirates))
                        , \_ -> Expect.equal BlueCoats (standardFaction (CarriedBy 1 BlueCoats))
                        , \_ -> Expect.equal RedCoats (standardFaction (AtSea RedCoats (vec3 10 0 20) 15.0))
                        ]
                        ()
            , test "isAtCitadel identifies citadel standards correctly" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal True (isAtCitadel Pirates (AtCitadel Pirates))
                        , \_ -> Expect.equal False (isAtCitadel BlueCoats (AtCitadel Pirates))
                        , \_ -> Expect.equal False (isAtCitadel Pirates (CarriedBy 1 Pirates))
                        , \_ -> Expect.equal False (isAtCitadel Pirates (AtSea Pirates (vec3 0 0 0) 0))
                        ]
                        ()
            , test "isAtSea identifies floating standards correctly" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal True (isAtSea (AtSea BlueCoats (vec3 10 0 20) 5.0))
                        , \_ -> Expect.equal False (isAtSea (AtCitadel BlueCoats))
                        , \_ -> Expect.equal False (isAtSea (CarriedBy 1 BlueCoats))
                        ]
                        ()
            , test "carriedBySlot and standardForCarrier identify ship carriers" <|
                \_ ->
                    let
                        standards =
                            [ AtCitadel Pirates
                            , CarriedBy 1 BlueCoats
                            , CarriedBy 2 RedCoats
                            ]
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just BlueCoats) (carriedBySlot 1 (CarriedBy 1 BlueCoats))
                        , \_ -> Expect.equal Nothing (carriedBySlot 2 (CarriedBy 1 BlueCoats))
                        , \_ -> Expect.equal (Just BlueCoats) (standardForCarrier 1 standards)
                        , \_ -> Expect.equal (Just RedCoats) (standardForCarrier 2 standards)
                        , \_ -> Expect.equal Nothing (standardForCarrier 3 standards)
                        ]
                        ()
            , test "initStandards has all three faction standards safe at citadels" <|
                \_ ->
                    Expect.equal
                        [ AtCitadel Pirates
                        , AtCitadel BlueCoats
                        , AtCitadel RedCoats
                        ]
                        Physics.initStandards
            ]
        , describe "Keel Burden handling modifier"
            [ test "unburdened ship retains baseline speed, turn rate, and acceleration" <|
                \_ ->
                    let
                        unburdened =
                            Physics.keelBurdenBoat sailingState sloop
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) sloop.topSpeed unburdened.topSpeed
                        , \_ -> Expect.within (Expect.Absolute 0.001) sloop.turnRate unburdened.turnRate
                        , \_ -> Expect.within (Expect.Absolute 0.001) sloop.acceleration unburdened.acceleration
                        ]
                        ()
            , test "carrying an enemy standard reduces top speed by 18% and handling by 25%" <|
                \_ ->
                    let
                        burdenedState =
                            { sailingState | carriedStandard = Just BlueCoats }

                        burdened =
                            Physics.keelBurdenBoat burdenedState sloop
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) (sloop.topSpeed * 0.82) burdened.topSpeed
                        , \_ -> Expect.within (Expect.Absolute 0.001) (sloop.turnRate * 0.75) burdened.turnRate
                        , \_ -> Expect.within (Expect.Absolute 0.001) (sloop.acceleration * 0.75) burdened.acceleration
                        ]
                        ()
            ]
        , describe "seizing enemy standards"
            [ test "slowing down inside enemy harbor circle seizes enemy standard" <|
                \_ ->
                    let
                        blueCoatsHarbour =
                            Physics.factionHarbourPoint course BlueCoats

                        -- Boat slowed down to 2.0 m/s inside harbor circle (radius 10 m)
                        boatPos =
                            Vec3.add blueCoatsHarbour (vec3 3 0 4)

                        boatSpeed =
                            2.0
                    in
                    Physics.canSeizeStandard boatPos boatSpeed course BlueCoats (AtCitadel BlueCoats)
                        |> Expect.equal True
            , test "sailing too fast (>= 3.0 m/s) does not seize the standard" <|
                \_ ->
                    let
                        blueCoatsHarbour =
                            Physics.factionHarbourPoint course BlueCoats

                        boatPos =
                            blueCoatsHarbour

                        fastSpeed =
                            4.5
                    in
                    Physics.canSeizeStandard boatPos fastSpeed course BlueCoats (AtCitadel BlueCoats)
                        |> Expect.equal False
            , test "sailing outside the harbor circle (> 10 m) does not seize the standard" <|
                \_ ->
                    let
                        blueCoatsHarbour =
                            Physics.factionHarbourPoint course BlueCoats

                        boatPos =
                            Vec3.add blueCoatsHarbour (vec3 25 0 0)

                        slowSpeed =
                            1.5
                    in
                    Physics.canSeizeStandard boatPos slowSpeed course BlueCoats (AtCitadel BlueCoats)
                        |> Expect.equal False
            , test "cannot seize standard if it is not at the citadel (already carried or at sea)" <|
                \_ ->
                    let
                        blueCoatsHarbour =
                            Physics.factionHarbourPoint course BlueCoats
                    in
                    Expect.all
                        [ \_ ->
                            Physics.canSeizeStandard blueCoatsHarbour 1.5 course BlueCoats (CarriedBy 1 BlueCoats)
                                |> Expect.equal False
                        , \_ ->
                            Physics.canSeizeStandard blueCoatsHarbour 1.5 course BlueCoats (AtSea BlueCoats blueCoatsHarbour 0)
                                |> Expect.equal False
                        ]
                        ()
            ]
        , describe "dropping and recovery at sea"
            [ test "recovering standard bobbing at sea within 4.5 m" <|
                \_ ->
                    let
                        seaPos =
                            vec3 50 0 120

                        boatNear =
                            vec3 52 0 122

                        boatFar =
                            vec3 60 0 120
                    in
                    Expect.all
                        [ \_ ->
                            Physics.canRecoverStandardAtSea boatNear (AtSea RedCoats seaPos 10.0)
                                |> Expect.equal True
                        , \_ ->
                            Physics.canRecoverStandardAtSea boatFar (AtSea RedCoats seaPos 10.0)
                                |> Expect.equal False
                        ]
                        ()
            , test "standard floating at sea advances age each second" <|
                \_ ->
                    let
                        seaStandard =
                            AtSea BlueCoats (vec3 10 0 20) 5.0

                        stepped =
                            Physics.stepStandards 1.0 [ seaStandard ]
                    in
                    Expect.equal [ AtSea BlueCoats (vec3 10 0 20) 6.0 ] stepped
            , test "standard floating at sea for > 60 seconds resets to citadel" <|
                \_ ->
                    let
                        oldSeaStandard =
                            AtSea BlueCoats (vec3 10 0 20) 59.5

                        stepped =
                            Physics.stepStandards 1.0 [ oldSeaStandard ]
                    in
                    Expect.equal [ AtCitadel BlueCoats ] stepped
            ]
        , describe "repatriation at home citadel"
            [ test "docking at home harbor with enemy standard triggers repatriation" <|
                \_ ->
                    let
                        piratesHarbour =
                            Physics.factionHarbourPoint course Pirates

                        boatPos =
                            Vec3.add piratesHarbour (vec3 2 0 3)

                        boatSpeed =
                            1.8
                    in
                    Physics.canRepatriateStandard boatPos boatSpeed course Pirates (Just BlueCoats)
                        |> Expect.equal True
            , test "docking at enemy harbor with enemy standard does not repatriate" <|
                \_ ->
                    let
                        redCoatsHarbour =
                            Physics.factionHarbourPoint course RedCoats
                    in
                    Physics.canRepatriateStandard redCoatsHarbour 1.5 course Pirates (Just BlueCoats)
                        |> Expect.equal False
            , test "moving too fast (>= 3.0 m/s) does not repatriate" <|
                \_ ->
                    let
                        piratesHarbour =
                            Physics.factionHarbourPoint course Pirates
                    in
                    Physics.canRepatriateStandard piratesHarbour 3.5 course Pirates (Just BlueCoats)
                        |> Expect.equal False
            , test "having no carried standard does not trigger repatriation" <|
                \_ ->
                    let
                        piratesHarbour =
                            Physics.factionHarbourPoint course Pirates
                    in
                    Physics.canRepatriateStandard piratesHarbour 1.5 course Pirates Nothing
                        |> Expect.equal False
            , test "repatriation bonus is exactly 15 seconds" <|
                \_ ->
                    Expect.equal 15.0 Physics.repatriationBonusSeconds
            ]
        ]
