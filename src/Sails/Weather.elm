module Sails.Weather exposing
    ( Conditions
    , Spell(..)
    , Weather
    , conditions
    , developed
    , fair
    , init
    , label
    , seedFrom
    , step
    )

{-| Storm weather over the course.

Weather runs as a string of spells. Fair weather is the course wind and the
normal sea. From time to time a storm blows up, one of three kinds:

  - **Squall**: a short, sharp blow. The wind gets up by three quarters and
    gusts hard, the sea builds and rain sweeps through. No lightning.
  - **Thunderstorm**: the long one. The wind doubles and gusts, the biggest
    seas, heavy rain and lightning.
  - **Lull**: the calm before the storm. The wind drops and backs, the sea
    goes glassy and the sky darkens. A thunderstorm always follows.

Each spell eases in and out over a few seconds. Storms end after a while:
a squall lasts 10 to 16 s, a thunderstorm 16 to 26 s, then fair weather
comes back for 14 to 30 s. The first spell is always fair, 10 to 20 s.

Everything is driven by a seeded generator stepped with the game's fixed
timestep, so the same seed and the same steps always give the same
weather: every player in a room sees the same storms.

-}

import Sails.Types exposing (Wind)


type Spell
    = Fair
    | Squall
    | Thunderstorm
    | Lull


{-| Weather state. `squall`, `thunder` and `lull` are how far each kind of
spell has set in (0..1); they ease towards 1 while that spell lasts and
back to 0 after it. `flash` is the lightning flash, 0..1. `clock` drives
the gusts.
-}
type alias Weather =
    { spell : Spell
    , left : Float
    , squall : Float
    , thunder : Float
    , lull : Float
    , flash : Float
    , boltIn : Float
    , boltBearing : Float
    , seed : Int
    , clock : Float
    }


{-| The weather to sail in now: the wind blowing, the sea state (see
`Sails.Physics.seaElevation`), and the look: `rain`, storm `gloom` and the
lightning `flash`, each 0..1, and the compass bearing of the last `bolt`.
-}
type alias Conditions =
    { wind : Wind
    , sea : Float
    , rain : Float
    , gloom : Float
    , flash : Float
    , bolt : Float
    }


modulus : Int
modulus =
    2147483647


{-| Next state of the Park-Miller generator and a number in [0, 1).
-}
random : Int -> ( Float, Int )
random seed =
    let
        next =
            modBy modulus (seed * 16807)
    in
    ( toFloat (next - 1) / toFloat (modulus - 1), next )


{-| A seed from text, such as a course id and a room id.
-}
seedFrom : String -> Int
seedFrom text =
    String.foldl (\c h -> modBy modulus (h * 31 + Char.toCode c)) 7 text
        |> modBy (modulus - 1)
        |> (+) 1


{-| Fair weather at the start of a race.
-}
init : Int -> Weather
init seed0 =
    let
        ( r, seed ) =
            random (clamp 1 (modulus - 1) seed0)
    in
    { spell = Fair
    , left = 10 + 10 * r
    , squall = 0
    , thunder = 0
    , lull = 0
    , flash = 0
    , boltIn = 3
    , boltBearing = 0
    , seed = seed
    , clock = 0
    }


{-| A fully developed spell that lasts a long time, for tests.
-}
developed : Spell -> Weather
developed spell =
    let
        w =
            init 1

        on s =
            if s == spell then
                1

            else
                0
    in
    { w | spell = spell, left = 1.0e9, squall = on Squall, thunder = on Thunderstorm, lull = on Lull }


{-| Seconds it takes a spell to set in or clear.
-}
easeSeconds : Float
easeSeconds =
    4


{-| The spell after the one that just ended, and how long it lasts.
-}
nextSpell : Spell -> Int -> ( ( Spell, Float ), Int )
nextSpell spell seed0 =
    let
        ( r1, seed1 ) =
            random seed0

        ( r2, seed2 ) =
            random seed1
    in
    ( case spell of
        Fair ->
            if r1 < 0.4 then
                ( Squall, 10 + 6 * r2 )

            else if r1 < 0.75 then
                ( Thunderstorm, 16 + 10 * r2 )

            else
                ( Lull, 6 + 3 * r2 )

        Lull ->
            ( Thunderstorm, 16 + 10 * r2 )

        _ ->
            ( Fair, 14 + 16 * r2 )
    , seed2
    )


{-| Advance the weather by `dt` seconds. Returns the weather and whether a
lightning bolt struck in this step (for the thunder).
-}
step : Float -> Weather -> ( Weather, Bool )
step dt w =
    let
        ( ( spell, left ), seed1 ) =
            if w.left - dt <= 0 then
                nextSpell w.spell w.seed

            else
                ( ( w.spell, w.left - dt ), w.seed )

        ease target v =
            if target > v then
                min target (v + dt / easeSeconds)

            else
                max target (v - dt / easeSeconds)

        on s =
            if spell == s then
                1

            else
                0

        thunder =
            ease (on Thunderstorm) w.thunder

        boltDue =
            thunder > 0.4 && w.boltIn - dt <= 0

        ( ( boltIn, boltBearing ), seed2 ) =
            if boltDue then
                let
                    ( r1, s1 ) =
                        random seed1

                    ( r2, s2 ) =
                        random s1
                in
                ( ( 2.5 + 4 * r1, 360 * r2 ), s2 )

            else
                ( ( w.boltIn - dt, w.boltBearing ), seed1 )
    in
    ( { spell = spell
      , left = left
      , squall = ease (on Squall) w.squall
      , thunder = thunder
      , lull = ease (on Lull) w.lull
      , flash =
            if boltDue then
                1

            else
                max 0 (w.flash - dt * 3)
      , boltIn = boltIn
      , boltBearing = boltBearing
      , seed = seed2
      , clock = w.clock + dt
      }
    , boltDue
    )


{-| Gust factor at a time, -1 .. 1: three slow, unrelated waves.
-}
gust : Float -> Float
gust t =
    0.5 * sin (t * 1.7) + 0.3 * sin (t * 0.63 + 1.3) + 0.2 * sin (t * 3.1 + 0.4)


{-| Fair weather in a wind: the normal sea, no rain, no clouds.
-}
fair : Wind -> Conditions
fair wind =
    { wind = wind, sea = 1, rain = 0, gloom = 0, flash = 0, bolt = 0 }


{-| The wind, sea and look of the weather, over the course wind `base`.
In fair weather this is exactly the course wind and the normal sea.
-}
conditions : Wind -> Weather -> Conditions
conditions base w =
    let
        t =
            w.clock

        strength =
            (1 + 0.75 * w.squall + 1.0 * w.thunder - 0.45 * w.lull)
                * (1 + (0.3 * w.squall + 0.22 * w.thunder) * gust t)
    in
    { wind =
        { speed = base.speed * strength
        , direction =
            base.direction
                + 18
                * w.squall
                * sin (t * 0.21)
                + 12
                * w.thunder
                * sin (t * 0.13 + 2)
                - 15
                * w.lull
        }
    , sea = 1 + 0.6 * w.squall + 1.4 * w.thunder - 0.4 * w.lull
    , rain = clamp 0 1 (0.75 * w.squall + w.thunder)
    , gloom = clamp 0 1 (0.45 * w.squall + 0.75 * w.thunder + 0.3 * w.lull)
    , flash = w.flash * w.thunder
    , bolt = w.boltBearing
    }


{-| What the HUD calls the weather, while it is not fair.
-}
label : Weather -> Maybe String
label w =
    case w.spell of
        Squall ->
            Just "Squall"

        Thunderstorm ->
            Just "Thunderstorm"

        Lull ->
            Just "Calm before the storm"

        Fair ->
            if w.thunder > 0.1 || w.squall > 0.1 then
                Just "Clearing"

            else
                Nothing
