port module Ports exposing
    ( binaryHashDecoded
    , copyShareUrl
    , createMultiplayerRoom
    , decodeBinaryHash
    , fileContentReceived
    , fileLoadError
    , geometryFlattenFailed
    , geometryFlattened
    , hashChanged
    , joinMultiplayerRoom
    , leaveMultiplayerRoom
    , multiplayerError
    , multiplayerRoomStateChanged
    , playSfx
    , requestFileUpload
    , requestGeometryFlatten
    , requestTiltPermission
    , saveLeaderboard
    , sendMultiplayerTelemetry
    , setAudioMuted
    , setUrlHash
    , startMultiplayerCountdown
    , storeLanguage
    , tiltOrientationReceived
    , updateAudioState
    , viewportResized
    )

{-| Ports for Brick Sails: runtime asset/file bridges (`initBricksRuntime`),
binary hash decoding, procedural Web Audio synthesis, device tilt controls, and
localStorage persistence.
-}

import Json.Decode


{-| Request the browser file picker for `.ldr`, `.mpd`, `.dat`, or `.io`.
-}
port requestFileUpload : () -> Cmd msg


{-| Raw LDraw file text delivered from upload, drag-and-drop, or `.io` extraction.
-}
port fileContentReceived : (String -> msg) -> Sub msg


{-| File load error string from the JS runtime.
-}
port fileLoadError : (String -> msg) -> Sub msg


{-| Geometry flattening worker port required by `initBricksRuntime`.
-}
port requestGeometryFlatten : String -> Cmd msg


{-| Flattened geometry response from `initBricksRuntime`.
-}
port geometryFlattened : (String -> msg) -> Sub msg


{-| Geometry worker failure from `initBricksRuntime`.
-}
port geometryFlattenFailed : (String -> msg) -> Sub msg


{-| Update browser URL hash without triggering page reload.
-}
port setUrlHash : String -> Cmd msg


{-| Copy full shareable sails URL to clipboard.
-}
port copyShareUrl : String -> Cmd msg


{-| Ask JS to inflate a `#b/<base64url>` string pasted into the in-app input.
-}
port decodeBinaryHash : String -> Cmd msg


{-| Receive decompressed `#b/...` payload bytes (or `Nothing` on error).
-}
port binaryHashDecoded : (Maybe (List Int) -> msg) -> Sub msg


{-| Notify Elm when the browser address bar hash changes externally.
-}
port hashChanged : (String -> msg) -> Sub msg


{-| Viewport resize notifications from `initBricksRuntime`.
-}
port viewportResized : ({ width : Int, height : Int } -> msg) -> Sub msg


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


{-| Persist sound mute toggle state.
-}
port setAudioMuted : Bool -> Cmd msg


{-| Persist language selection.
-}
port storeLanguage : String -> Cmd msg


{-| Persist leaderboard JSON string.
-}
port saveLeaderboard : String -> Cmd msg


{-| Prompt iOS / mobile browser for device tilt permission.
-}
port requestTiltPermission : () -> Cmd msg


{-| Normalized tilt steering value from device accelerometer [-1.0, 1.0].
-}
port tiltOrientationReceived : (Float -> msg) -> Sub msg


{-| Create a new multiplayer regatta room as host/commentator.
-}
port createMultiplayerRoom : { roomId : String, courseId : String, name : String, vehicleHash : String } -> Cmd msg


{-| Join an existing multiplayer regatta room as a sailor.
-}
port joinMultiplayerRoom : { roomId : String, name : String, vehicleHash : String } -> Cmd msg


{-| Stream local ship telemetry to room peers over WebRTC.
-}
port sendMultiplayerTelemetry :
    { pos : List Float
    , rot : List Float
    , speed : Float
    , progress : Float
    , finished : Bool
    }
    -> Cmd msg


{-| Trigger synchronized 3-second regatta countdown.
-}
port startMultiplayerCountdown : () -> Cmd msg


{-| Disconnect from multiplayer room.
-}
port leaveMultiplayerRoom : () -> Cmd msg


{-| Incoming room state update from WebRTC signaling runtime.
-}
port multiplayerRoomStateChanged : (Json.Decode.Value -> msg) -> Sub msg


{-| Multiplayer error notification.
-}
port multiplayerError : (String -> msg) -> Sub msg
