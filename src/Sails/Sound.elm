module Sails.Sound exposing (Sound(..), encode)

{-| The one-shot sounds of a voyage, each its own so the synthesizer
(`audio.js`) can give it its own voice: the player's cannon and the
pirates', a ball splashing, a ball hitting a ship, a pier or a building,
something sunk or falling down, the hull knocked or wrecked, coins, the
buoy bell and so on. Sent through `Ports.playSfx` as `{ type, strength?,
count? }`.

Brick Sails has more sounds than the shared `Game.Audio` events, so it
encodes its own; `audio.js` still understands those too.

-}

import Json.Encode as Encode


{-| A one-shot sound.

  - `CannonFire`: the player's own broadside.
  - `EnemyCannon`: a pirate or patrol ship firing.
  - `Splash`: a ball dropping in the sea, by strength (0..1).
  - `HitShip`, `HitDock`, `HitBuilding`: a ball striking a ship, a harbour
    pier, a building.
  - `ShipSunk`: a pirate or patrol ship sinks.
  - `Collapse`: a pier or building falls down.
  - `HullHit`: the player's hull takes a knock, by strength (0..1).
  - `Shipwrecked`: the player's ship breaks up.
  - `Bump`: scraping a rock or reef.
  - `Coins`: coins earned, by how many.
  - `Bell`: a harbour or buoy passed. `Finish`: the voyage is done.
  - `Horn`: the race starts.
  - `AnchorDrop`, `AnchorWeigh`, `Thunder`, `GhostAppear`, `GhostBanish`.

-}
type Sound
    = CannonFire
    | EnemyCannon
    | Splash Float
    | HitShip
    | HitDock
    | HitBuilding
    | ShipSunk
    | Collapse
    | HullHit Float
    | Shipwrecked
    | Bump
    | Coins Int
    | Bell
    | Finish
    | Horn
    | AnchorDrop
    | AnchorWeigh
    | Thunder
    | GhostAppear
    | GhostBanish


{-| The sound as `Ports.playSfx` sends it.
-}
encode : Sound -> Encode.Value
encode sound =
    case sound of
        CannonFire ->
            kind "cannon"

        EnemyCannon ->
            kind "enemyCannon"

        Splash strength ->
            withStrength "splash" strength

        HitShip ->
            kind "hitShip"

        HitDock ->
            kind "hitDock"

        HitBuilding ->
            kind "hitBuilding"

        ShipSunk ->
            kind "sunk"

        Collapse ->
            kind "collapse"

        HullHit strength ->
            withStrength "hullHit" strength

        Shipwrecked ->
            kind "wrecked"

        Bump ->
            kind "bump"

        Coins count ->
            Encode.object
                [ ( "type", Encode.string "coins" )
                , ( "count", Encode.int count )
                ]

        Bell ->
            kind "bell"

        Finish ->
            kind "finish"

        Horn ->
            kind "horn"

        AnchorDrop ->
            kind "anchorDrop"

        AnchorWeigh ->
            kind "anchorWeigh"

        Thunder ->
            kind "thunder"

        GhostAppear ->
            kind "ghostAppear"

        GhostBanish ->
            kind "ghostBanish"


kind : String -> Encode.Value
kind name =
    Encode.object [ ( "type", Encode.string name ) ]


withStrength : String -> Float -> Encode.Value
withStrength name strength =
    Encode.object
        [ ( "type", Encode.string name )
        , ( "strength", Encode.float strength )
        ]
