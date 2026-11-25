module Sails.Handling exposing
    ( Handling
    , Limits
    , fromSpec
    , limits
    , reference
    )

{-| Handling of a ship loaded from a build, derived from its analysis.

`Sails.Analyze` reports the build's sail area / displacement ratio (how much
drive the rig has for its mass), its hull speed (from the waterline length)
and its reserve buoyancy. Each is compared with the same figure for the
built-in sloop's brick model (`reference`), so a build like the sloop handles
like the sloop:

  - more sail for the mass: faster, quicker to pick up speed, turns a little
    wider,
  - a longer hull: a higher top speed and wider turns,
  - a heavier build: slower to accelerate and to turn,
  - more reserve buoyancy: rides higher and is a touch faster.

Every value is clamped to `limits`, so any build that passes
`Sails.Ingest.validateShip` stays sailable round the course. The four
built-in boats keep their own hand-tuned values (`Sails.Boats`).

-}

import Sails.Analyze as Analyze
import Sails.Types exposing (ShipSpec)


{-| How a boat handles: `topSpeed` in m/s, `turnRate` its agility in
degrees per second, `acceleration` as the rate (1/s) the speed closes on its
target. The race physics turns at 70 % of `turnRate` at full helm and full
speed, with a soft helm and quicker acceleration from a standstill (see
`Sails.Physics.steerResponse`), so every boat is easy to point.
-}
type alias Handling =
    { topSpeed : Float
    , turnRate : Float
    , acceleration : Float
    }


type alias Limits =
    { topSpeed : ( Float, Float )
    , turnRate : ( Float, Float )
    , acceleration : ( Float, Float )
    }


{-| Allowed range of each derived value. The built-in boats lie inside it.
-}
limits : Limits
limits =
    { topSpeed = ( 7.4, 10.2 )
    , turnRate = ( 60, 130 )
    , acceleration = ( 0.8, 2.0 )
    }


{-| The built-in sloop's handling and the analysis figures of its brick
model, which the derivation is measured against.
-}
reference :
    { handling : Handling
    , sailAreaDisplacementRatio : Float
    , hullSpeedKnots : Float
    , mass : Float
    , lengthMeters : Float
    , reserveBuoyancy : Float
    }
reference =
    { handling = { topSpeed = 9.0, turnRate = 95.0, acceleration = 1.3 }
    , sailAreaDisplacementRatio = 6.89
    , hullSpeedKnots = 7.04
    , mass = 3277.8
    , lengthMeters = 8.4
    , reserveBuoyancy = 3.18
    }


{-| Speed, turning and acceleration for a loaded build.
-}
fromSpec : ShipSpec -> Handling
fromSpec spec =
    let
        report =
            Analyze.analyzeShip spec

        ratio a b =
            max 0.05 a / max 0.05 b

        power =
            ratio report.sailAreaDisplacementRatio reference.sailAreaDisplacementRatio

        hull =
            ratio report.maxHullSpeedKnots reference.hullSpeedKnots

        heavy =
            ratio spec.mass reference.mass

        long =
            ratio spec.lengthMeters reference.lengthMeters

        buoyant =
            ratio report.reserveBuoyancy reference.reserveBuoyancy

        within ( lo, hi ) v =
            clamp lo hi v

        base =
            reference.handling
    in
    { topSpeed =
        within limits.topSpeed
            (base.topSpeed * sqrt hull * (power ^ 0.3) * (buoyant ^ 0.1))
    , turnRate =
        within limits.turnRate
            (base.turnRate * (long ^ -0.5) * (heavy ^ -0.15) * (power ^ -0.2))
    , acceleration =
        within limits.acceleration
            (base.acceleration * (power ^ 0.6) * (heavy ^ -0.2))
    }
