module FrameGuardTest exposing (suite)

{-| The canvas resolution guard (`Sails.FrameGuard`): full size at speed,
a step down after sustained slow frames, back up after sustained fast ones.
-}

import Expect
import Sails.FrameGuard as FrameGuard exposing (FrameGuard)
import Test exposing (Test, describe, test)


frames : Int -> Float -> FrameGuard -> FrameGuard
frames n dtMs guard =
    List.foldl (\_ g -> FrameGuard.record dtMs g) guard (List.range 1 n)


suite : Test
suite =
    describe "Sails.FrameGuard"
        [ test "60 fps keeps the full canvas size" <|
            \_ ->
                FrameGuard.init
                    |> frames 600 (1000 / 60)
                    |> (\g -> FrameGuard.scaled g 1280)
                    |> Expect.equal 1280
        , test "a 30 Hz power-saving cap does not count as slow" <|
            \_ ->
                FrameGuard.init
                    |> frames 300 (1000 / 30)
                    |> .scale
                    |> Expect.equal 1
        , test "sustained slow frames step the canvas down" <|
            \_ ->
                FrameGuard.init
                    |> frames 120 50
                    |> .scale
                    |> Expect.lessThan 1
        , test "the scale never goes below the floor" <|
            \_ ->
                FrameGuard.init
                    |> frames 3000 60
                    |> .scale
                    |> Expect.atLeast FrameGuard.minRenderScale
        , test "long hitches are not counted" <|
            \_ ->
                FrameGuard.init
                    |> frames 100 1000
                    |> Expect.equal FrameGuard.init
        , test "fast frames step back up to full size" <|
            \_ ->
                FrameGuard.init
                    |> frames 120 50
                    |> frames 3000 (1000 / 60)
                    |> .scale
                    |> Expect.within (Expect.Absolute 1.0e-9) 1
        ]
