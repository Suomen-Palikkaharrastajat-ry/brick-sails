port module Ports exposing
    ( playSfx
    , requestThumbnails
    , thumbnailLoaded
    , updateAudioState
    )

{-| Ports of Brick Sails whose payload is its own: the procedural Web Audio
telemetry and one-shot sounds `audio.js` plays, and the part pictures of the
ship editor's dock. Every other port the game uses is shared by the Brick
Frame games and lives in `Game.Ports`.
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


{-| Ask the shared thumbnail pipeline (`initThumbnails` in `main.js`) for a
picture of each part in a colour, for the ship editor's dock.
-}
port requestThumbnails : List { file : String, color : Int, rgb : List Float } -> Cmd msg


{-| A rendered part picture (a data URL) from the thumbnail pipeline.
-}
port thumbnailLoaded : ({ file : String, color : Int, url : String } -> msg) -> Sub msg
