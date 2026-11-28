module DebrisTest exposing (suite)

{-| Unit tests of fishing floating debris out of the water: which pieces the
boat can pick up (`Sails.Wreck.collect`), how much a piece patches the hull
(`Sails.Hull.repair`) and floating pieces shrinking away (`Sails.Wreck.step`).
-}

import Expect
import Math.Vector3 exposing (Vec3, vec3)
import Sails.Ending as Ending
import Sails.Harbours as Harbours
import Sails.Hull as Hull exposing (Hull)
import Sails.Types exposing (SailingState)
import Sails.Wreck as Wreck
import Test exposing (Test, describe, test)


floating : Vec3 -> Wreck.Piece
floating position =
    { position = position
    , velocity = vec3 0 0 0
    , yaw = 0
    , tumble = 0
    , spin = 0
    , look = Wreck.Brick "3001.dat" 4
    , afloat = True
    , afloatFor = 1
    , salvageable = True
    }


hurt : Float -> Hull
hurt health =
    { health = health, sinceDamage = 1.5, lastDamage = 10 }


origin : Vec3
origin =
    vec3 0 0 0


{-| A pier footprint centred on the origin.
-}
pierAtOrigin : Harbours.Footprint
pierAtOrigin =
    { x = 0, z = 0, dirX = 1, dirZ = 0, halfLength = 6, halfWidth = 1.5 }


{-| A boat at rest at the origin, heading north.
-}
boatAtOrigin : SailingState
boatAtOrigin =
    { position = origin
    , heading = 0
    , speed = 0
    , steer = 0
    , sailPower = 0
    , sailAngle = 0
    , heel = 0
    , pitch = 0
    , bumped = False
    , sail = 0
    , carriedStandard = Nothing
    }


{-| Pieces settled on a calm sea and drifted back to the origin, so only
`salvageable` decides whether the boat there picks them up.
-}
settledAtOrigin : List Wreck.Piece -> List Wreck.Piece
settledAtOrigin pieces =
    steps 200 0.02 pieces |> List.map (\p -> { p | position = origin })


{-| Step the pieces `n` times by `dt` on a calm sea.
-}
steps : Int -> Float -> List Wreck.Piece -> List Wreck.Piece
steps n dt pieces =
    List.foldl (\i ps -> Wreck.step dt 0 (toFloat i * dt) ps) pieces (List.range 1 n)


suite : Test
suite =
    describe "Floating debris"
        [ describe "Sails.Wreck.collect"
            [ test "picks up a floating piece inside the radius" <|
                \_ ->
                    Wreck.collect origin [ floating (vec3 (Wreck.collectRadius - 0.5) 0.4 0) ]
                        |> Expect.equal ( 1, [] )
            , test "leaves a piece just outside the radius" <|
                \_ ->
                    let
                        far =
                            floating (vec3 0 0 (Wreck.collectRadius + 0.5))
                    in
                    Wreck.collect origin [ far ]
                        |> Expect.equal ( 0, [ far ] )
            , test "measures across the water: the swell's height does not count" <|
                \_ ->
                    Wreck.collectable origin (floating (vec3 4 3 4))
                        |> Expect.equal True
            , test "leaves a piece still flying off what broke up" <|
                \_ ->
                    let
                        flying =
                            floating (vec3 1 2 1) |> (\p -> { p | afloat = False, afloatFor = 0 })
                    in
                    Wreck.collect origin [ flying ]
                        |> Expect.equal ( 0, [ flying ] )
            , test "a collected piece is gone, so it is never counted twice" <|
                \_ ->
                    let
                        near =
                            floating (vec3 1 0 1)

                        far =
                            floating (vec3 50 0 0)

                        ( first, left ) =
                            Wreck.collect origin [ near, far, near ]

                        ( second, after ) =
                            Wreck.collect origin left
                    in
                    Expect.all
                        [ \_ -> Expect.equal 2 first
                        , \_ -> Expect.equal [ far ] left
                        , \_ -> Expect.equal ( 0, [ far ] ) ( second, after )
                        ]
                        ()
            , test "a burst's pieces can be picked up once they splash down" <|
                \_ ->
                    let
                        pieces =
                            Wreck.burst Wreck.GhostShip origin 0 0

                        settled =
                            steps 200 0.02 pieces

                        ( picked, _ ) =
                            Wreck.collect origin settled
                    in
                    Expect.all
                        [ \_ -> Expect.equal ( 0, pieces ) (Wreck.collect origin pieces)
                        , \_ -> Expect.equal True (List.all .afloat settled)
                        , \_ -> Expect.atLeast 1 picked
                        ]
                        ()
            ]
        , describe "which pieces are salvageable"
            [ test "a burst's pieces are collected" <|
                \_ ->
                    let
                        pieces =
                            settledAtOrigin (Harbours.smash pierAtOrigin 0 0)
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (List.all .salvageable pieces)
                        , \_ -> Expect.equal ( List.length pieces, [] ) (Wreck.collect origin pieces)
                        ]
                        ()
            , test "a pirate ship's pieces are collected" <|
                \_ ->
                    settledAtOrigin (Wreck.breakUp (Wreck.Ship { name = "npc:frigate", scale = 2 }) boatAtOrigin)
                        |> Wreck.collect origin
                        |> Tuple.first
                        |> Expect.equal Wreck.pieceCount
            , test "the pieces of a pier the boat rams are not collected" <|
                \_ ->
                    let
                        pieces =
                            settledAtOrigin (Harbours.ram pierAtOrigin 0 6)
                    in
                    Expect.all
                        [ \_ -> Expect.equal False (List.any .salvageable pieces)
                        , \_ -> Expect.equal ( 0, pieces ) (Wreck.collect origin pieces)
                        , \_ -> Expect.equal Wreck.pieceCount (List.length pieces)
                        ]
                        ()
            , test "the finish's confetti is not collected" <|
                \_ ->
                    let
                        pieces =
                            settledAtOrigin (Ending.confetti 0 boatAtOrigin)
                    in
                    Expect.all
                        [ \_ -> Expect.equal False (List.any .salvageable pieces)
                        , \_ -> Expect.equal ( 0, pieces ) (Wreck.collect origin pieces)
                        , \_ -> Expect.equal True (List.all .afloat pieces)
                        ]
                        ()
            , test "unsalvageable pieces still expire" <|
                \_ ->
                    steps 1500 0.02 (Harbours.ram pierAtOrigin 0 6)
                        |> Expect.equal []
            ]
        , describe "Sails.Wreck.salvage"
            [ test "a hurt hull picks up pieces and is patched debrisRepair each" <|
                \_ ->
                    let
                        ( picked, patched, left ) =
                            Wreck.salvage origin (hurt 50) [ floating (vec3 1 0 1), floating (vec3 2 0 0), floating (vec3 50 0 0) ]
                    in
                    Expect.all
                        [ \_ -> Expect.equal 2 picked
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) (50 + 2 * Hull.debrisRepair) patched.health
                        , \_ -> Expect.equal [ floating (vec3 50 0 0) ] left
                        ]
                        ()
            , test "a hull at full health picks nothing up and is not repaired" <|
                \_ ->
                    let
                        pieces =
                            [ floating (vec3 1 0 1), floating (vec3 2 0 0) ]
                    in
                    Wreck.salvage origin Hull.fresh pieces
                        |> Expect.equal ( 0, Hull.fresh, pieces )
            , test "a wrecked hull picks nothing up" <|
                \_ ->
                    let
                        pieces =
                            [ floating (vec3 1 0 1) ]
                    in
                    Wreck.salvage origin (hurt 0) pieces
                        |> Expect.equal ( 0, hurt 0, pieces )
            ]
        , describe "Sails.Hull.repair"
            [ test "each piece patches debrisRepair" <|
                \_ ->
                    (Hull.repair Hull.debrisRepair (hurt 50)).health
                        |> Expect.within (Expect.Absolute 1.0e-9) (50 + Hull.debrisRepair)
            , test "is capped at maxHealth" <|
                \_ ->
                    (Hull.repair (Hull.debrisRepair * 3) (hurt (Hull.maxHealth - 2))).health
                        |> Expect.within (Expect.Absolute 1.0e-9) Hull.maxHealth
            , test "does nothing to a wrecked hull" <|
                \_ ->
                    Hull.repair Hull.debrisRepair (hurt 0)
                        |> Expect.equal (hurt 0)
            , test "leaves the mending timer as it was" <|
                \_ ->
                    (Hull.repair Hull.debrisRepair (hurt 50)).sinceDamage
                        |> Expect.within (Expect.Absolute 1.0e-9) 1.5
            ]
        , describe "Sails.Wreck.step expiry"
            [ test "a floating piece is gone after floatSeconds" <|
                \_ ->
                    let
                        piece =
                            floating (vec3 10 0 10) |> (\p -> { p | afloatFor = 0 })
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (List.length (steps 50 0.1 [ piece ]))
                        , \_ -> Expect.equal 0 (List.length (steps (round (Wreck.floatSeconds / 0.1) + 1) 0.1 [ piece ]))
                        ]
                        ()
            , test "a whole wreck's pieces are all gone in time" <|
                \_ ->
                    steps 1500 0.02 (Wreck.burst Wreck.HarbourPier origin 0 0)
                        |> Expect.equal []
            , test "a piece shrinks away over its last shrinkSeconds" <|
                \_ ->
                    let
                        at s =
                            Wreck.size (floating origin |> (\p -> { p | afloatFor = s }))
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-9) 1 (at 5)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0.5 (at (Wreck.floatSeconds - Wreck.shrinkSeconds / 2))
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 (at Wreck.floatSeconds)
                        ]
                        ()
            , test "a piece still in the air does not age" <|
                \_ ->
                    let
                        high =
                            floating (vec3 0 50 0) |> (\p -> { p | afloat = False, afloatFor = 0 })
                    in
                    List.map .afloatFor (Wreck.step 0.02 0 0 [ high ])
                        |> Expect.equal [ 0 ]
            ]
        ]
