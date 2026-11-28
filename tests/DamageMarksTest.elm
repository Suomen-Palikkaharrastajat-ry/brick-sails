module DamageMarksTest exposing (suite)

{-| The durability bars over struck structures (`Sails.DamageMarks`): a
small pool, refreshed per structure and capped, aged out after a few
seconds, with a fill that is the share of durability left, green to red,
fading in and out.
-}

import Expect
import Math.Vector3 as Vec3 exposing (vec3)
import Sails.DamageMarks as DamageMarks
import Test exposing (Test, describe, test)


at : Float -> Vec3.Vec3
at x =
    vec3 x 5 0


suite : Test
suite =
    describe "Sails.DamageMarks"
        [ test "a hit adds a bar; a second hit on the same structure refreshes it" <|
            \_ ->
                DamageMarks.empty
                    |> DamageMarks.hit "a" (at 0) 3 4
                    |> DamageMarks.step 1
                    |> DamageMarks.hit "a" (at 0) 2 4
                    |> List.map (\m -> ( m.key, m.left, m.age ))
                    |> Expect.equal [ ( "a", 2, 0 ) ]
        , test "the pool holds at most poolSize bars, latest first" <|
            \_ ->
                List.foldl (\i pool -> DamageMarks.hit (String.fromInt i) (at (toFloat i)) 1 2 pool) DamageMarks.empty (List.range 1 (DamageMarks.poolSize + 2))
                    |> List.map .key
                    |> Expect.equal (List.map String.fromInt (List.reverse (List.range 3 (DamageMarks.poolSize + 2))))
        , test "bars are gone after markSeconds" <|
            \_ ->
                DamageMarks.empty
                    |> DamageMarks.hit "a" (at 0) 1 2
                    |> DamageMarks.step (DamageMarks.markSeconds - 0.1)
                    |> (\pool -> ( List.length pool, List.length (DamageMarks.step 0.2 pool) ))
                    |> Expect.equal ( 1, 0 )
        , test "the fill is the share of durability left" <|
            \_ ->
                DamageMarks.empty
                    |> DamageMarks.hit "a" (at 0) 1 4
                    |> List.map DamageMarks.fraction
                    |> Expect.equal [ 0.25 ]
        , test "green when whole, yellow at half, red when down" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (vec3 0.2 0.85 0.25) (DamageMarks.colourFor 1)
                    , \_ -> Expect.equal (vec3 0.98 0.85 0.15) (DamageMarks.colourFor 0.5)
                    , \_ -> Expect.equal (vec3 0.92 0.18 0.12) (DamageMarks.colourFor 0)
                    ]
                    ()
        , test "a bar fades in on the hit, flashes, and fades out at the end" <|
            \_ ->
                let
                    mark age =
                        { key = "a", position = at 0, left = 1, total = 2, age = age }
                in
                Expect.all
                    [ \_ -> Expect.equal 0 (DamageMarks.alpha (mark 0))
                    , \_ -> Expect.equal 1 (DamageMarks.alpha (mark 1))
                    , \_ -> Expect.lessThan 0.5 (DamageMarks.alpha (mark (DamageMarks.markSeconds - 0.1)))
                    , \_ -> Expect.equal ( 1, 0 ) ( DamageMarks.flash (mark 0), DamageMarks.flash (mark 1) )
                    ]
                    ()
        ]
