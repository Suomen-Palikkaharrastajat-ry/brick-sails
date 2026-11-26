module Sails.TiltPitch exposing
    ( Pitch
    , Zone(..)
    , calibrate
    , degrees
    , init
    , receive
    )

{-| Forward and backward tilt of the phone, for the sail controls.

The phone reports its pitch (`beta`, in degrees) as it is held. `calibrate`
takes the pitch at that moment as level: the neutral point at the start of a
run. Past 14 degrees of tilt either way the zone changes, and it only changes
back inside 7 degrees, so a hand wobbling near the edge does not flicker
between zones. Forward tilt (tipping top of phone away from user) decreases
beta below neutral, giving positive degrees (neutral - beta > 0) to raise sails;
backward tilt (tipping top towards user) increases beta, giving negative degrees
to lower sails.

The first pitch received sets the neutral point, so the controls work from the
start of a voyage; the start of each run calibrates again.

-}


type Zone
    = Forward
    | Level
    | Backward


type alias Pitch =
    { beta : Maybe Float
    , neutral : Maybe Float
    , zone : Zone
    }


init : Pitch
init =
    { beta = Nothing
    , neutral = Nothing
    , zone = Level
    }


{-| Take the latest pitch as level. Does nothing until a pitch has been
received.
-}
calibrate : Pitch -> Pitch
calibrate pitch =
    case pitch.beta of
        Just b ->
            { pitch | neutral = Just b, zone = Level }

        Nothing ->
            pitch


{-| Record a new pitch from the phone and move the zone if it has changed.
-}
receive : Float -> Pitch -> Pitch
receive b pitch =
    let
        withBeta =
            { pitch | beta = Just b }
    in
    case withBeta.neutral of
        Nothing ->
            { withBeta | neutral = Just b, zone = Level }

        Just _ ->
            { withBeta | zone = nextZone (degrees withBeta) withBeta.zone }


{-| Tilt from level in degrees, positive forward, between -180 and 180. Zero
until calibrated.
-}
degrees : Pitch -> Float
degrees pitch =
    case ( pitch.beta, pitch.neutral ) of
        ( Just b, Just n ) ->
            wrap (n - b)

        _ ->
            0


wrap : Float -> Float
wrap angle =
    angle - 360 * toFloat (round (angle / 360))


nextZone : Float -> Zone -> Zone
nextZone deg zone =
    case zone of
        Level ->
            if deg > 14 then
                Forward

            else if deg < -14 then
                Backward

            else
                Level

        Forward ->
            if deg < 7 then
                Level

            else
                Forward

        Backward ->
            if deg > -7 then
                Level

            else
                Backward
