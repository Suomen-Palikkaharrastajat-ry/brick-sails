port module Ports exposing
    ( playSfx
    , updateAudioState
    )

{-| Ports of Brick Sails whose payload is its own: the procedural Web Audio
telemetry and one-shot sounds `audio.js` plays. Every other port the game
uses is shared by the Brick Frame games and lives in `Game.Ports`.
-}

import Json.Decode


{-| Stream continuous ship audio telemetry to the procedural Web Audio synthesizer.
-}
port updateAudioState :
    { active : Bool
    , windGain : Float
    , windHz : Float
    , washGain : Float
    , washHz : Float
    , hullGain : Float
    , toneHz : Float
    , toneGain : Float
    }
    -> Cmd msg


{-| Trigger a discrete one-shot sound effect.
-}
port playSfx : Json.Decode.Value -> Cmd msg
