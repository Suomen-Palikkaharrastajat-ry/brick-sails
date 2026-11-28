port module Ports exposing
    ( buildHistoryLoaded
    , modelThumbnailLoaded
    , playSfx
    , requestModelThumbnails
    , requestThumbnails
    , saveBuildHistory
    , thumbnailLoaded
    , updateAudioState
    )

{-| Ports of Brick Sails whose payload is its own: the procedural Web Audio
telemetry and one-shot sounds `audio.js` plays, the part and model pictures
of the ship editor, and its saved build history. Every other port the game uses is shared by the Brick
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


{-| Ask the shared thumbnail pipeline for a picture of a whole model (a
starter hull, a saved build), see `Game.ModelPreview`.
-}
port requestModelThumbnails : List { key : String, text : String, colours : List { code : Int, rgb : List Float } } -> Cmd msg


{-| A rendered model picture (a data URL), under the key it was asked for.
-}
port modelThumbnailLoaded : ({ key : String, url : String } -> msg) -> Sub msg


{-| Save the ship editor's build history (`Game.BuildHistory.toJson`) in
localStorage (`main.js`).
-}
port saveBuildHistory : String -> Cmd msg


{-| The saved build history, sent by `main.js` once the app has started (an
empty string when there is none or storage is blocked).
-}
port buildHistoryLoaded : (String -> msg) -> Sub msg
