module GameplayTest exposing (suite)

{-| Tests of the sea, the sails, the weather and the islands.
-}

import Expect
import Math.Vector3 as Vec3
import Sails.Boats as Boats
import Sails.Physics as Physics
import Sails.Types exposing (SailingState, Wind)
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
    sailWith { sea = sea, sail = 1, wind = course.wind, steer = Nothing } heading seconds


{-| Sail from the start on a heading for `seconds` with a sail area, sea
and wind; `steer` holds the helm over instead of keeping the heading.
-}
sailWith : { sea : Float, sail : Float, wind : Wind, steer : Maybe Float } -> Float -> Float -> SailingState
sailWith cfg heading seconds =
    let
        start =
            Physics.initSailingState course
    in
    List.foldl
        (\i s ->
            Physics.stepBoat
                { dt = 0.02
                , time = toFloat i * 0.02
                , steer = Maybe.withDefault (clamp -1 1 (-(Physics.angleDiff heading s.heading) / 10)) cfg.steer
                , throttle = 1
                , sea = cfg.sea
                , sail = cfg.sail
                }
                cfg.wind
                Boats.default
                []
                s
        )
        { start | heading = heading, sail = cfg.sail }
        (List.range 1 (round (seconds / 0.02)))


{-| A storm wind: twice the course wind.
-}
gale : Wind
gale =
    { speed = 14, direction = course.wind.direction }


{-| Heading across the course wind (the fastest point of sail).
-}
reach : Float
reach =
    course.wind.direction + 180 + 90


speedWith : Float -> Wind -> Float
speedWith area wind =
    (sailWith { sea = 1, sail = area, wind = wind, steer = Nothing } reach 10).speed


{-| Degrees turned in two seconds hard over.
-}
turnedWith : Float -> Wind -> Float
turnedWith area wind =
    let
        moving =
            sailWith { sea = 1, sail = area, wind = wind, steer = Nothing } reach 8

        after =
            List.foldl
                (\i s -> Physics.stepBoat { dt = 0.02, time = 8 + toFloat i * 0.02, steer = 1, throttle = 1, sea = 1, sail = area } wind Boats.default [] s)
                moving
                (List.range 1 100)
    in
    abs (Physics.angleDiff after.heading moving.heading)


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
        , describe "Sails"
            [ test "full sail in the course wind is the tuned sailing" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-9) 1 (Physics.sailDrive 1 course.wind)
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 (Physics.overpower 1 course.wind)
                        , \_ -> Expect.equal ( "Full", 1.0 ) (Physics.sailLevel Physics.defaultSailLevel)
                        ]
                        ()
            , test "raising the sail is faster, lowering it slower" <|
                \_ ->
                    let
                        speeds =
                            List.map (\( _, area ) -> speedWith area course.wind) Physics.sailLevels
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.sort speeds) speeds
                        , \_ -> Expect.greaterThan (speedWith 1 course.wind * 1.05) (speedWith 1.2 course.wind)
                        , \_ -> Expect.lessThan (speedWith 1 course.wind * 0.85) (speedWith 0.65 course.wind)
                        , \_ -> Expect.greaterThan 0.5 (speedWith 0 course.wind)
                        ]
                        ()
            , test "less sail steers more easily, extra sail turns wider" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.greaterThan (turnedWith 1 course.wind) (turnedWith 0.35 course.wind)
                        , \_ -> Expect.lessThan (turnedWith 1 course.wind) (turnedWith 1.2 course.wind)
                        ]
                        ()
            , test "in a gale full sail is overpowered: it heels hard and reefing is faster" <|
                \_ ->
                    let
                        heelWith area =
                            abs (sailWith { sea = 1, sail = area, wind = gale, steer = Nothing } reach 10).heel
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 1 (Physics.overpower 1 gale)
                        , \_ -> Expect.greaterThan (speedWith 1 gale) (speedWith 0.65 gale)
                        , \_ -> Expect.greaterThan (heelWith 0.65 + 3) (heelWith 1)
                        , \_ -> Expect.greaterThan (turnedWith 1 gale) (turnedWith 0.65 gale)
                        ]
                        ()
            , test "the sail eases to a new setting over a second or two" <|
                \_ ->
                    let
                        start =
                            Physics.initSailingState course

                        stepped n =
                            List.foldl
                                (\i s -> Physics.stepBoat { dt = 0.02, time = toFloat i * 0.02, steer = 0, throttle = 1, sea = 1, sail = 0.35 } course.wind Boats.default [] s)
                                start
                                (List.range 1 n)
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 0.6 (stepped 10).sail
                        , \_ -> Expect.within (Expect.Absolute 0.05) 0.35 (stepped 200).sail
                        ]
                        ()
            ]
        ]
