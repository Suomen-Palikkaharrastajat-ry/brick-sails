port module Ports exposing
    ( buildHistoryLoaded
    , clipboardRead
    , clipboardReadFailed
    , downloadStudioIo
    , modelThumbnailLoaded
    , playSfx
    , readClipboard
    , requestModelThumbnails
    , requestThumbnails
    , saveBuildHistory
    , thumbnailLoaded
    , updateAudioState
    )

{-| Ports of Brick Sails whose payload is its own: the procedural Web Audio
telemetry and one-shot sounds `audio.js` plays, the part and model pictures
of the ship editor, its saved build history, reading a shared build from the
clipboard and downloading the build as a BrickLink Studio file. Every other port the game uses is shared by the Brick
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


{-| Read the text on the clipboard (`navigator.clipboard.readText` in
`main.js`), for the ship editor's paste button. The answer comes back as
`clipboardRead` or `clipboardReadFailed`.
-}
port readClipboard : () -> Cmd msg


{-| The text read from the clipboard.
-}
port clipboardRead : (String -> msg) -> Sub msg


{-| The clipboard could not be read: `"denied"` (the browser or the player
refused the permission), `"unsupported"` (no clipboard API) or `"failed"`.
-}
port clipboardReadFailed : (String -> msg) -> Sub msg


{-| Download the build as a BrickLink Studio `.io` archive: `main.js` zips the
LDraw text as its `model.ldr` (as Brick Play does) and hands the browser the
file.
-}
port downloadStudioIo : { filename : String, ldrText : String } -> Cmd msg
