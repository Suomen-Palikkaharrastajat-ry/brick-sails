module Sails.Coins exposing
    ( FlyingCoin
    , burst
    , burstCount
    , courseTarget
    , harbourDocked
    , lifetime
    , pirateBoarded
    , pirateSunk
    , piratesToSink
    , pop
    , popCount
    , starRating
    , step
    , treasureChest
    )

{-| The score of a voyage: gold coins. Every coin a run earns comes from one
of four sources, each worth a fixed number of coins (all of them here):

  - `pirateSunk`: a cannon ball sinks a pirate or patrol ship (10).
  - `pirateBoarded`: a patrol ship comes alongside and the crew fights the
    boarders off, taking their purse (15). The hull still takes the knock.
  - `treasureChest`: the boat sails up to a treasure chest on an island (25).
  - `harbourDocked`: the boat docks at a harbour of the course (5).

Time does not rank a run: the stars, the personal best and the leaderboard
all count coins (`starRating`, `Sails.Records`). The time is still shown.

The coins also fly (`FlyingCoin`): a sunk ship throws a burst of them up in
an arc that falls into the sea (`burst`), a boarding or a chest pops a few
up over the boat that drop back onto its deck (`pop`). Each lives
`lifetime` seconds, spinning as it flies, and is then gone (`step`).

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Types exposing (Course)



-- VALUES


{-| Coins for sinking a pirate or patrol ship.
-}
pirateSunk : Int
pirateSunk =
    10


{-| Coins for fighting off a patrol ship that boards the boat.
-}
pirateBoarded : Int
pirateBoarded =
    15


{-| Coins in a treasure chest.
-}
treasureChest : Int
treasureChest =
    25


{-| Coins for docking at a harbour.
-}
harbourDocked : Int
harbourDocked =
    5


{-| How many patrol ships the coin target counts on sinking: the size of the
fleet, `Sails.Pirates.fleetSize` (a test keeps the two equal: `Sails.Pirates`
imports `Sails.Physics`, which uses this module, so it cannot be imported
here).
-}
piratesToSink : Int
piratesToSink =
    3


{-| The coins of a full run of a course: every harbour docked, every chest
found and the whole patrol fleet sunk. Boardings come on top.
-}
courseTarget : Course -> Int
courseTarget course =
    harbourDocked
        * List.length course.waypoints
        + treasureChest
        * List.length course.treasures
        + pirateSunk
        * piratesToSink


{-| 3 stars at or over the course's coin target, 2 at half of it or more,
otherwise 1.
-}
starRating : Int -> Int -> Int
starRating target coins =
    if coins >= target then
        3

    else if 2 * coins >= target then
        2

    else
        1



-- FLYING COINS


{-| A coin in the air: where it is, how fast it goes, its age (seconds),
and its spin (radians per second) and starting turn, so no two coins turn
alike.
-}
type alias FlyingCoin =
    { position : Vec3
    , velocity : Vec3
    , age : Float
    , spin : Float
    , phase : Float
    }


{-| Seconds a coin flies before it is gone.
-}
lifetime : Float
lifetime =
    1.2


{-| Coins in the burst of a sunk ship.
-}
burstCount : Int
burstCount =
    10


{-| Coins in the pop of a boarding or a treasure chest.
-}
popCount : Int
popCount =
    6


gravity : Float
gravity =
    10


{-| A sunk ship's coins: `burstCount` coins thrown up from its deck (`at`
is the ship's water-line position) in a ring, falling into the sea round it
as they end. `seed` turns the ring, so two bursts differ.
-}
burst : Int -> Vec3 -> List FlyingCoin
burst seed at =
    fling { count = burstCount, seed = seed, lift = 1.6, out = 2.2, up = 6.2, carry = vec3 0 0 0 } at


{-| A few coins popping up over the boat (`at`, moving with `carry` m/s) and
dropping back onto its deck as they end: they keep the boat's speed, so
they stay over it.
-}
pop : Int -> Vec3 -> Vec3 -> List FlyingCoin
pop seed carry at =
    fling { count = popCount, seed = seed, lift = 1.2, out = 0.7, up = 6.0, carry = carry } at


fling : { count : Int, seed : Int, lift : Float, out : Float, up : Float, carry : Vec3 } -> Vec3 -> List FlyingCoin
fling { count, seed, lift, out, up, carry } at =
    let
        origin =
            Vec3.add at (vec3 0 lift 0)

        coin i =
            let
                fi =
                    toFloat (i + 13 * seed)

                angle =
                    2 * pi * toFloat i / toFloat count + frac (fi * 0.618034) * 0.6

                speedOut =
                    out * (0.7 + 0.6 * frac (fi * 0.4142))
            in
            { position = origin
            , velocity =
                Vec3.add carry
                    (vec3 (sin angle * speedOut) (up * (0.9 + 0.2 * frac (fi * 0.3183))) (cos angle * speedOut))
            , age = 0
            , spin = 6 + 6 * frac (fi * 0.7071)
            , phase = angle
            }
    in
    List.map coin (List.range 0 (count - 1))


{-| Move the coins `dt` seconds on under gravity and drop those whose
`lifetime` is over.
-}
step : Float -> List FlyingCoin -> List FlyingCoin
step dt coins =
    coins
        |> List.map
            (\c ->
                let
                    v =
                        Vec3.sub c.velocity (vec3 0 (gravity * dt) 0)
                in
                { c | velocity = v, position = Vec3.add c.position (Vec3.scale dt v), age = c.age + dt }
            )
        |> List.filter (\c -> c.age < lifetime)


frac : Float -> Float
frac x =
    x - toFloat (floor x)
