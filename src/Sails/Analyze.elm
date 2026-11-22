module Sails.Analyze exposing
    ( AnalysisReport
    , Seaworthiness(..)
    , StabilityRating(..)
    , analyzeShip
    )

{-| Seaworthiness, stability, and sail-plan analysis for LEGO ships.
-}

import Sails.Types exposing (ShipSpec)


type Seaworthiness
    = Seaworthy
    | MinorAdvisory String
    | Unseaworthy String


type StabilityRating
    = RatingA -- Ocean-going, extremely stable
    | RatingB -- Coastal cruiser, good righting moment
    | RatingC -- Sheltered waters / light winds only


type alias AnalysisReport =
    { seaworthiness : Seaworthiness
    , stabilityRating : StabilityRating
    , maxHullSpeedKnots : Float
    , sailAreaDisplacementRatio : Float
    , minCloseHauledAngle : Float -- degrees to wind (e.g. 42°)
    , notes : List String
    }


{-| Perform complete maritime and architectural analysis of an ingested ship.
-}
analyzeShip : ShipSpec -> AnalysisReport
analyzeShip spec =
    let
        -- Displacement in cubic meters of seawater (~1025 kg/m³)
        dispCubicM =
            spec.mass / 1025.0

        -- Sail Area / Displacement ratio: SAD = SailArea / (Disp)^(2/3)
        sad =
            spec.sailAreaSqMeters / max 0.1 (dispCubicM ^ (2.0 / 3.0))

        -- Theoretical displacement hull speed: 1.34 * sqrt(LWL in feet)
        -- in knots: ~ 2.43 * sqrt(length in meters)
        hullSpeedKnots =
            2.43 * sqrt (max 1.0 spec.lengthMeters)

        stability =
            if spec.metacentricHeight > 1.0 then
                RatingA

            else if spec.metacentricHeight > 0.6 then
                RatingB

            else
                RatingC

        -- Check minimum build integrity
        ( seaworthiness, notes ) =
            if spec.hullPartCount < 2 then
                ( Unseaworthy "Ship has insufficient hull volume to float upright."
                , [ "Add inverted slopes or hull bricks to form an enclosed buoyant displacement." ]
                )

            else if spec.sailPartCount == 0 then
                ( MinorAdvisory "No sail detected. Vessel will drift under wind and tide only."
                , [ "Attach a flag (2335), cloth, or wedge plate to the mast to catch the wind." ]
                )

            else if spec.mastPartCount == 0 then
                ( MinorAdvisory "No mast detected. Rigging is low to the deck."
                , [ "Add an antenna (3957a) or technic axle for a taller sail plan." ]
                )

            else
                let
                    baseNotes =
                        [ "Hull displacement: " ++ String.fromInt (round spec.mass) ++ " kg"
                        , "Sail area: " ++ String.fromInt (round spec.sailAreaSqMeters) ++ " m² (SAD ratio: " ++ String.fromInt (round sad) ++ ")"
                        ]

                    speedNote =
                        if sad > 20.0 then
                            "High-performance rig: fast acceleration in light to moderate breezes."

                        else
                            "Cruising rig: very stable and forgiving in heavy seas."
                    cannonNote =
                        if spec.cannonCount > 0 then
                            "Broadside battery: " ++ String.fromInt spec.cannonCount ++ " naval cannons equipped. Press [Space] or tap FIRE to shoot!"

                        else
                            "Unarmed vessel: swift racer with low displacement ballast."
                in
                ( Seaworthy, baseNotes ++ [ speedNote, cannonNote ] )

        closeHauledAngle =
            if spec.beamMeters > 2.0 then
                40.0

            else
                44.0
    in
    { seaworthiness = seaworthiness
    , stabilityRating = stability
    , maxHullSpeedKnots = hullSpeedKnots
    , sailAreaDisplacementRatio = sad
    , minCloseHauledAngle = closeHauledAngle
    , notes = notes
    }
