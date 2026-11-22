module Sails.Hull exposing
    ( Band(..)
    , Hull
    , Knocks
    , band
    , cannonDamage
    , fresh
    , knockDamage
    , maxHealth
    , noKnocks
    , percent
    , repairDelay
    , repairRate
    , roughSeaRate
    , step
    , wrecked
    )

{-| The boat's hull health.

A boat starts each race sound, at `maxHealth`. Striking rock (an island's
shore), a cannonball hit and pounding into a rough storm sea take health
off; at none the boat breaks up into its bricks and the run is lost.

It is meant to be fair: a slow nudge does no harm (`knockDamage` is zero
under 2.5 m/s), a hard knock at full speed takes about a fifth, and a hull
that has not been hit for a few seconds slowly mends (`repairDelay`,
`repairRate`). A boat survives four full-speed crashes into rock in a row;
the fifth sinks it.

All of it is a pure function of the fixed steps it is given, like the
rest of the physics.

-}


type alias Hull =
    { health : Float
    , sinceDamage : Float
    , lastDamage : Float
    }


maxHealth : Float
maxHealth =
    100


fresh : Hull
fresh =
    { health = maxHealth, sinceDamage = 0, lastDamage = 0 }


{-| What struck the hull in one step: the speed of a knock on rock, if any
(see `Sails.Physics.rockImpact`), how many cannonballs hit, and how hard the
sea is pounding it (see `Sails.Physics.roughSeaStrain`).
-}
type alias Knocks =
    { rock : Maybe Float
    , cannonHits : Int
    , roughSea : Float
    }


noKnocks : Knocks
noKnocks =
    { rock = Nothing, cannonHits = 0, roughSea = 0 }


{-| Health a knock on rock takes off at an impact speed in m/s: none below
2.5 m/s, then 6 plus 2.2 per m/s, so a full-speed sloop (9 m/s) loses
about 20.
-}
knockDamage : Float -> Float
knockDamage speed =
    if speed < 2.5 then
        0

    else
        6 + 2.2 * (speed - 2.5)


{-| Health a cannonball hit takes off.
-}
cannonDamage : Float
cannonDamage =
    12


{-| Health lost per second per unit of rough-sea strain: about 1 a second
driving hard into the biggest storm waves.
-}
roughSeaRate : Float
roughSeaRate =
    0.6


{-| Seconds without damage before the hull starts to mend.
-}
repairDelay : Float
repairDelay =
    4


{-| Health mended per second once it does.
-}
repairRate : Float
repairRate =
    2


{-| Advance the hull one step. Damage counts only while `racing`; the
hull mends in any case, and a wrecked hull stays wrecked.
-}
step : Float -> Bool -> Knocks -> Hull -> Hull
step dt racing knocks hull =
    if wrecked hull then
        hull

    else
        let
            damage =
                if racing then
                    Maybe.withDefault 0 (Maybe.map knockDamage knocks.rock)
                        + cannonDamage
                        * toFloat knocks.cannonHits
                        + roughSeaRate
                        * max 0 knocks.roughSea
                        * dt

                else
                    0
        in
        if damage > 0 then
            { health = max 0 (hull.health - damage)
            , sinceDamage = 0
            , lastDamage = damage
            }

        else
            let
                since =
                    hull.sinceDamage + dt

                mend =
                    if since > repairDelay then
                        repairRate * dt

                    else
                        0
            in
            { hull
                | health = min maxHealth (hull.health + mend)
                , sinceDamage = since
            }


wrecked : Hull -> Bool
wrecked hull =
    hull.health <= 0


{-| Health as a whole percentage, 0 to 100. A hull with any health left
shows at least 1 %.
-}
percent : Hull -> Int
percent hull =
    if wrecked hull then
        0

    else
        clamp 1 100 (ceiling (hull.health / maxHealth * 100 - 1.0e-6))


{-| How the HUD shows the hull, with Brick Racer's integrity thresholds:
sound above 65 %, damaged above 30 %, critical at or below.
-}
type Band
    = Sound
    | Damaged
    | Critical


band : Hull -> Band
band hull =
    let
        pct =
            percent hull
    in
    if pct > 65 then
        Sound

    else if pct > 30 then
        Damaged

    else
        Critical
