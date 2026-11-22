module GameplayTest exposing (suite)

{-| Tests of the sea, the sails, the weather and the islands.
-}

import Expect
import Math.Vector3 as Vec3
import Sails.Boats as Boats
import Sails.Physics as Physics
import Sails.Types exposing (SailingState)
import Test exposing (Test, describe, test)


course : Sails.Types.Course
course =
    Physics.defaultCourse


{-| The highest swell seen over a grid of points and a minute and a half.
-}
peakWave : Float -> Float
peakWave sea =
    List.range 0 90
        |> List.concatMap
            (\i ->
                List.range 0 8
                    |> List.map (\j -> abs (Physics.seaElevation sea (toFloat j * 17) (toFloat j * 11) (toFloat i)))
            )
        |> List.maximum
        |> Maybe.withDefault 0


{-| Sail straight on a heading (the helm holds it) for `seconds`.
-}
holdCourse : Float -> Float -> Float -> SailingState
holdCourse sea heading seconds =
    let
        start =
            Physics.initSailingState course
    in
    List.foldl
        (\i s ->
            Physics.stepBoat
                { dt = 0.02, time = toFloat i * 0.02, steer = clamp -1 1 (-(Physics.angleDiff heading s.heading) / 10), throttle = 1, sea = sea }
                course.wind
                Boats.default
                []
                s
        )
        { start | heading = heading }
        (List.range 1 (round (seconds / 0.02)))


suite : Test
suite =
    describe "Gameplay"
        [ describe "Sea"
            [ test "normal weather keeps the original swell between wave sets" <|
                \_ ->
                    let
                        original px pz pt =
                            0.22 * sin (2 * pi / 36 * (pz * 0.9 + px * 0.4) - 2 * pi / 36 * 3 * pt) + 0.1 * sin (2 * pi / 22 * (px * 0.8 - pz * 0.5) - 2 * pi / 22 * 2.4 * pt)

                        -- Out of a set: the set envelope is 1 there.
                        x =
                            0

                        z =
                            -50

                        t =
                            0
                    in
                    Expect.within (Expect.Absolute 0.001) (original x z t) (Physics.waterElevation x z t)
            , test "wave sets roll through: some waves are higher than the plain swell" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.greaterThan 0.34 (peakWave 1)
                        , \_ -> Expect.atMost 0.5 (peakWave 1)
                        ]
                        ()
            , test "a storm sea is much bigger, a calm one smaller" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.greaterThan (2 * peakWave 1) (peakWave 2.4)
                        , \_ -> Expect.lessThan (peakWave 1) (peakWave 0.6)
                        ]
                        ()
            , test "big head seas slow the boat; normal seas barely do" <|
                \_ ->
                    let
                        into =
                            Physics.swellHeading + 180

                        calm =
                            (holdCourse 1 into 12).speed

                        rough =
                            (holdCourse 2.4 into 12).speed
                    in
                    Expect.all
                        [ \_ -> Expect.lessThan (calm * 0.92) rough
                        , \_ -> Expect.greaterThan (calm * 0.6) rough
                        ]
                        ()
            , test "the hull pitches and rolls more in a storm sea" <|
                \_ ->
                    let
                        motion sea =
                            List.range 1 40
                                |> List.map (\i -> holdCourse sea 90 (toFloat i * 0.25))
                                |> List.map (\s -> abs s.heel + abs s.pitch)
                                |> List.maximum
                                |> Maybe.withDefault 0
                    in
                    Expect.greaterThan (motion 1 * 1.5) (motion 2.4)
            , test "the boat rides the water" <|
                \_ ->
                    let
                        s =
                            holdCourse 2.4 90 7

                        p =
                            Vec3.toRecord s.position
                    in
                    Expect.within (Expect.Absolute 0.001) (Physics.seaElevation 2.4 p.x p.z 7) p.y
            ]
        ]
