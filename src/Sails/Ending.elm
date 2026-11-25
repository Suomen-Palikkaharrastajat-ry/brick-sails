module Sails.Ending exposing
    ( Ending
    , burstTimes
    , bursts
    , confetti
    , starsShown
    , start
    , step
    )

{-| The end of a voyage, as Brick Racer's finish celebration: once the boat
docks at the last harbour, bricks burst into the air over it a few times
(`burstTimes`), the result card comes up with its stars popping in one by one
(`starsShown`), the headline, the final time and the new-record badge, and
the two ways on: sail again or back to the main menu.

Everything here follows from the finished run and the time since it ended,
so an ending looks the same each time.

-}

import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Physics as Physics
import Sails.Types exposing (CourseProgress, SailingState)
import Sails.Wreck as Wreck


{-| A finished voyage: seconds since it ended, its final time, its stars (1
to 3) and whether it beat the personal best held when the run started.
-}
type alias Ending =
    { timer : Float
    , score : Float
    , stars : Int
    , isNewRecord : Bool
    }


{-| The ending of a run that has just finished on a course with this target
time, against the personal best (seconds) held when it started.
-}
start : { targetSeconds : Float, personalBest : Maybe Float } -> CourseProgress -> Ending
start { targetSeconds, personalBest } progress =
    let
        score =
            Physics.finalTime progress
    in
    { timer = 0
    , score = score
    , stars = Physics.starRating targetSeconds score
    , isNewRecord =
        case personalBest of
            Just best ->
                score < best

            Nothing ->
                True
    }


step : Float -> Ending -> Ending
step dt ending =
    { ending | timer = ending.timer + max 0 dt }


{-| Seconds after the finish at which bricks burst over the boat.
-}
burstTimes : List Float
burstTimes =
    [ 0, 0.7, 1.4 ]


{-| How many bursts go off between `before` and `after` (the same ending a
step apart). The first goes off when the ending starts.
-}
bursts : Maybe Ending -> Ending -> Int
bursts before after =
    let
        from =
            Maybe.map .timer before
    in
    burstTimes
        |> List.filter
            (\t ->
                (case from of
                    Just f ->
                        t > f

                    Nothing ->
                        True
                )
                    && t
                    <= after.timer
            )
        |> List.length


{-| The stars shown so far: the first after 0.4 s, then one every 0.35 s, up
to the run's stars.
-}
starsShown : Ending -> Int
starsShown ending =
    if ending.timer < 0.4 then
        0

    else
        min ending.stars (1 + floor ((ending.timer - 0.4) / 0.35))


{-| Burst number `n` of bright bricks over the boat: they fly up high, tumble
down and float, like the bricks of a wreck (`Sails.Wreck.step` moves them).
-}
confetti : Int -> SailingState -> List Wreck.Piece
confetti n state =
    let
        p =
            Vec3.toRecord state.position

        piece i =
            let
                fi =
                    toFloat (i + 7 * n)

                angle =
                    fi * 2.39996

                out =
                    1.5 + 2.5 * frac (fi * 0.618034)
            in
            { position = vec3 p.x (p.y + 3) p.z
            , velocity = vec3 (sin angle * out) (9 + 4 * frac (fi * 0.4142)) (cos angle * out)
            , yaw = angle
            , tumble = 0
            , spin = 4 + 5 * frac (fi * 0.3183)
            , part =
                if modBy 2 i == 0 then
                    "3001.dat"

                else
                    "3003.dat"
            , colour =
                List.drop (modBy (List.length palette) (i + n)) palette
                    |> List.head
                    |> Maybe.withDefault 14
            , afloat = False
            }
    in
    List.map piece (List.range 0 (piecesPerBurst - 1))


piecesPerBurst : Int
piecesPerBurst =
    14


{-| Party colours: yellow, red, blue, green, white, orange.
-}
palette : List Int
palette =
    [ 14, 4, 1, 2, 15, 25 ]


frac : Float -> Float
frac x =
    x - toFloat (floor x)
