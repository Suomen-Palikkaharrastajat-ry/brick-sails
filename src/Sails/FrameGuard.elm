module Sails.FrameGuard exposing
    ( FrameGuard
    , init, record, scaled
    , slowFrameMs, fastFrameMs, slowSeconds, fastSeconds, scaleStep, minRenderScale
    )

{-| A frame-rate guard for the 3D canvas: when frames stay slow for a while
(a phone that cannot keep up), draw the canvas at a lower resolution, a step
at a time, and the browser stretches it to the screen. When frames are fast
again for a good while, step back up. Only the canvas is scaled: the HUD is
HTML and stays sharp, and nothing in the game reads the canvas size (taps
are aimed in CSS pixels, `Sails.CannonPick`).

At full speed the scale stays 1 and the canvas is exactly as before.

@docs FrameGuard
@docs init, record, scaled
@docs slowFrameMs, fastFrameMs, slowSeconds, fastSeconds, scaleStep, minRenderScale

-}


{-| Average frame time above this (ms, about 26 frames a second) counts as
slow. It stays above 33 ms so the 30 Hz cap of a phone in power-saving mode
never counts as slow by itself.
-}
slowFrameMs : Float
slowFrameMs =
    38


{-| Average frame time below this (ms, about 45 frames a second) counts as
fast enough to step the resolution back up. The wide gap to `slowFrameMs`
keeps the guard from stepping up and down in turn.
-}
fastFrameMs : Float
fastFrameMs =
    22


{-| Seconds of slow frames before the resolution steps down.
-}
slowSeconds : Float
slowSeconds =
    2


{-| Seconds of fast frames before the resolution steps back up.
-}
fastSeconds : Float
fastSeconds =
    10


{-| How much one step changes the canvas scale (of the CSS size, per side).
-}
scaleStep : Float
scaleStep =
    0.15


{-| The lowest canvas scale the guard goes down to (per side).
-}
minRenderScale : Float
minRenderScale =
    0.55


{-| Frames longer than this (ms) are not counted: a hidden tab coming back,
or a pause while part models are built, says nothing about the drawing.
-}
hitchMs : Float
hitchMs =
    250


{-| How fast the average follows each new frame (an exponential moving
average over roughly the last ten frames).
-}
smoothing : Float
smoothing =
    0.1


{-| The average frame time (`averageMs`), how long it has been slow or fast
(seconds), and the canvas `scale` (1 is the full CSS size).
-}
type alias FrameGuard =
    { averageMs : Float
    , slowFor : Float
    , fastFor : Float
    , scale : Float
    }


{-| Full resolution, a 60 Hz average to start from.
-}
init : FrameGuard
init =
    { averageMs = 1000 / 60, slowFor = 0, fastFor = 0, scale = 1 }


{-| Count one frame of `dtMs` milliseconds.
-}
record : Float -> FrameGuard -> FrameGuard
record dtMs guard =
    if dtMs <= 0 || dtMs > hitchMs then
        guard

    else
        let
            average =
                guard.averageMs + smoothing * (dtMs - guard.averageMs)

            seconds =
                dtMs / 1000

            slowFor =
                if average > slowFrameMs then
                    guard.slowFor + seconds

                else
                    0

            fastFor =
                if average < fastFrameMs then
                    guard.fastFor + seconds

                else
                    0
        in
        if slowFor >= slowSeconds && guard.scale > minRenderScale then
            -- Start the average afresh, so the next step waits to see what
            -- this one did.
            { averageMs = (slowFrameMs + fastFrameMs) / 2
            , slowFor = 0
            , fastFor = 0
            , scale = max minRenderScale (guard.scale - scaleStep)
            }

        else if fastFor >= fastSeconds && guard.scale < 1 then
            { averageMs = average
            , slowFor = 0
            , fastFor = 0
            , scale = min 1 (guard.scale + scaleStep)
            }

        else
            { averageMs = average, slowFor = slowFor, fastFor = fastFor, scale = guard.scale }


{-| A canvas side (CSS pixels) at the guard's scale, at least 1 pixel.
-}
scaled : FrameGuard -> Int -> Int
scaled guard size =
    if guard.scale >= 1 then
        size

    else
        max 1 (round (toFloat size * guard.scale))
