module Sails.NpcShips exposing
    ( all
    , frigate
    , frigateName
    , isNpcShip
    , sailColour
    , sloop
    , sloopName
    )

{-| The brick ships the computer sails: a three-masted frigate and a
single-masted sloop, built like the microscale pirate ships of the
"ship in a bottle" kind: a stepped hull with a raked bow and bowsprit, a
gunport band on the sides, cannons on deck, and square sails of stacked
1 x 3 bricks hanging from brick yards.

Each model is LDraw MPD text, bow towards LDraw -Z and -Y up, like the boats
in `Sails.Boats` and the starter hulls in `Sails.StarterBases`. The hull is
one grouped submodel (`npc-frigate-hull.ldr`, `npc-sloop-hull.ldr`) that the
main model places once; masts, yards, sails, flags and cannons sit on it in
the main model, as on a player's ship. `Sails.ShipMesh.build` flattens both.

The two hulls are also starter hulls of the shipyard,
`starters/sails/frigate-hull.mpd` and `starters/sails/sloop-hull.mpd`, with
the same submodel name and parts, so an NPC ship is a build on its starter
hull and the player can build the same ship from the editor's parts and
colours. Change a hull in both places, then run
`node starters/embed-starters.mjs`; `tests/StarterHullsTest.elm` checks they
match. Every part is stud-joined to the ship (a flag by its clips on the mast);
`tests/LegalConnectionsTest.elm` checks the hull, the rig and the flattened
ship with `Studio.Health` and for loose parts.

Every part is in the brick catalog's curated set, so the ships load from the
same part bundles as the player's boats. The masts and yards are reddish
brown (70), the wood of the editor's palette. The sails are the white bricks
(`sailColour`), so the game can tint them per faction; nothing else on the
ships is white. The cannons are 32828 plates (1 x 1 round with bar) with the
bar pointing outboard, so `Sails.ShipMesh` finds their muzzles.

-}


{-| Name of the frigate's model in the game's document and mesh tables.
-}
frigateName : String
frigateName =
    "npc:frigate"


{-| Name of the sloop's model in the game's document and mesh tables.
-}
sloopName : String
sloopName =
    "npc:sloop"


{-| Both ships as (name, LDraw text), to register with the other models.
-}
all : List ( String, String )
all =
    [ ( frigateName, frigate ), ( sloopName, sloop ) ]


{-| Whether a model name is one of these ships.
-}
isNpcShip : String -> Bool
isNpcShip name =
    name == frigateName || name == sloopName


{-| LDraw colour of the sail bricks (white).
-}
sailColour : Int
sailColour =
    15


{-| LDraw MPD text of the frigate: 4 x 11 stud hull (14 studs with the
bowsprit), three masts, eight cannons.
-}
frigate : String
frigate =
    String.join "\n"
        [ "0 FILE npc-frigate.ldr"
        , "0 NPC Frigate"
        , "0 Name: npc-frigate.ldr"
        , "0 Author: Brick Sails"
        , "0 !LICENSE Licensed under CC BY 4.0 : see CAreadme.txt"
        , "0 // Microscale three-masted frigate: 4 x 11 stud hull with a raked bow, bowsprit,"
        , "0 // stepped forecastle, waist and quarterdeck, a tan and brown gunport band,"
        , "0 // eight deck cannons and white brick square sails (the sail colour, 15)."
        , "0 // Hull: one grouped submodel, moved as one piece"
        , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 npc-frigate-hull.ldr"
        , "0 // Waist cannons, four a side (32828 plate 1 x 1 round with bar)"
        , "1 0 30 -40 -50 1 0 0 0 1 0 0 0 1 32828.dat"
        , "1 0 -30 -40 -50 -1 0 0 0 1 0 0 0 -1 32828.dat"
        , "1 0 30 -40 -30 1 0 0 0 1 0 0 0 1 32828.dat"
        , "1 0 -30 -40 -30 -1 0 0 0 1 0 0 0 -1 32828.dat"
        , "1 0 30 -40 10 1 0 0 0 1 0 0 0 1 32828.dat"
        , "1 0 -30 -40 10 -1 0 0 0 1 0 0 0 -1 32828.dat"
        , "1 0 30 -40 30 1 0 0 0 1 0 0 0 1 32828.dat"
        , "1 0 -30 -40 30 -1 0 0 0 1 0 0 0 -1 32828.dat"
        , "0 // Foremast: jumper step, lower mast, yard and course"
        , "1 70 0 -64 -80 1 0 0 0 1 0 0 0 1 87580.dat"
        , "1 70 0 -208 -80 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 70 0 -216 -90 1 0 0 0 1 0 0 0 1 3021.dat"
        , "1 15 0 -208 -100 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -184 -100 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -160 -100 1 0 0 0 1 0 0 0 1 3622.dat"
        , "0 // Foremast: topmast, topsail yard and topsail"
        , "1 70 0 -240 -80 1 0 0 0 1 0 0 0 1 3062b.dat"
        , "1 70 0 -264 -80 1 0 0 0 1 0 0 0 1 3062b.dat"
        , "1 70 0 -272 -90 1 0 0 0 1 0 0 0 1 3021.dat"
        , "1 15 0 -264 -100 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -240 -100 1 0 0 0 1 0 0 0 1 3622.dat"
        , "0 // Foremast: pole and flag"
        , "1 70 0 -280 -80 1 0 0 0 1 0 0 0 1 3957a.dat"
        , "1 4 0 -356 -80 1 0 0 0 1 0 0 0 1 2335.dat"
        , "0 // Mainmast: jumper step, lower mast, yard and course"
        , "1 70 0 -40 0 1 0 0 0 1 0 0 0 1 87580.dat"
        , "1 70 0 -184 0 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 70 0 -192 -10 1 0 0 0 1 0 0 0 1 3021.dat"
        , "1 15 0 -184 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -160 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -136 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -112 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "0 // Mainmast: topmast, topsail yard and topsail"
        , "1 70 0 -216 0 1 0 0 0 1 0 0 0 1 3062b.dat"
        , "1 70 0 -240 0 1 0 0 0 1 0 0 0 1 3062b.dat"
        , "1 70 0 -264 0 1 0 0 0 1 0 0 0 1 3062b.dat"
        , "1 70 0 -288 0 1 0 0 0 1 0 0 0 1 3062b.dat"
        , "1 70 0 -296 -10 1 0 0 0 1 0 0 0 1 3021.dat"
        , "1 15 0 -288 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -264 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -240 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -216 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "0 // Mainmast: pole and flag"
        , "1 70 0 -304 0 1 0 0 0 1 0 0 0 1 3957a.dat"
        , "1 4 0 -380 0 1 0 0 0 1 0 0 0 1 2335.dat"
        , "0 // Mizzen: jumper step, lower mast, yard and course"
        , "1 70 0 -56 60 1 0 0 0 1 0 0 0 1 87580.dat"
        , "1 70 0 -200 60 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 70 0 -208 50 1 0 0 0 1 0 0 0 1 3021.dat"
        , "1 15 0 -200 40 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -176 40 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -152 40 1 0 0 0 1 0 0 0 1 3622.dat"
        , "0 // Mizzen: pole and flag"
        , "1 70 0 -216 60 1 0 0 0 1 0 0 0 1 3957a.dat"
        , "1 4 0 -292 60 1 0 0 0 1 0 0 0 1 2335.dat"
        , "0 NOFILE"
        , "0 FILE npc-frigate-hull.ldr"
        , "0 NPC Frigate hull (grouped)"
        , "0 Name: npc-frigate-hull.ldr"
        , "0 Author: Brick Sails"
        , "0 // Keel layer: two rows of 2 x 4 bricks with an inverted slope forefoot"
        , "1 70 -20 0 -70 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 70 -20 0 -20 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 -20 0 60 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 20 0 -70 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 70 20 0 -20 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 20 0 60 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "0 // Gun deck: raked bow, tan and brown checker band with grey gunports"
        , "1 70 -20 -24 -90 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 70 -30 -24 -70 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 70 20 -24 -90 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 70 30 -24 -70 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 70 0 -24 -40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 0 -24 40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 0 -24 90 1 0 0 0 1 0 0 0 1 3004.dat"
        , "1 19 30 -24 -50 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 70 30 -24 -30 0 0 -1 0 1 0 1 0 0 87087.dat"
        , "1 71 48 -14 -30 0 -1 0 1 0 0 0 0 1 6141.dat"
        , "1 19 30 -24 -10 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 70 30 -24 10 0 0 -1 0 1 0 1 0 0 87087.dat"
        , "1 71 48 -14 10 0 -1 0 1 0 0 0 0 1 6141.dat"
        , "1 19 30 -24 30 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 70 30 -24 50 0 0 -1 0 1 0 1 0 0 87087.dat"
        , "1 71 48 -14 50 0 -1 0 1 0 0 0 0 1 6141.dat"
        , "1 19 30 -24 70 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 30 -24 90 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 -30 -24 -50 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 70 -30 -24 -30 0 0 1 0 1 0 -1 0 0 87087.dat"
        , "1 71 -48 -14 -30 0 1 0 -1 0 0 0 0 1 6141.dat"
        , "1 19 -30 -24 -10 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 70 -30 -24 10 0 0 1 0 1 0 -1 0 0 87087.dat"
        , "1 71 -48 -14 10 0 1 0 -1 0 0 0 0 1 6141.dat"
        , "1 19 -30 -24 30 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 70 -30 -24 50 0 0 1 0 1 0 -1 0 0 87087.dat"
        , "1 71 -48 -14 50 0 1 0 -1 0 0 0 0 1 6141.dat"
        , "1 19 -30 -24 70 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 -30 -24 90 1 0 0 0 1 0 0 0 1 3005.dat"
        , "0 // Waist (main deck)"
        , "1 28 0 -32 -20 1 0 0 0 1 0 0 0 1 3031.dat"
        , "1 28 0 -32 30 1 0 0 0 1 0 0 0 1 3710.dat"
        , "0 // Forecastle, one brick up"
        , "1 28 -20 -32 -90 0 0 1 0 1 0 -1 0 0 3021.dat"
        , "1 70 -20 -56 -90 0 0 1 0 1 0 -1 0 0 3002.dat"
        , "1 28 20 -32 -90 0 0 1 0 1 0 -1 0 0 3021.dat"
        , "1 70 20 -56 -90 0 0 1 0 1 0 -1 0 0 3002.dat"
        , "0 // Quarterdeck, one brick up, and the stern castle with lanterns"
        , "1 70 -20 -48 70 0 0 1 0 1 0 -1 0 0 3002.dat"
        , "1 70 20 -48 70 0 0 1 0 1 0 -1 0 0 3002.dat"
        , "1 70 0 -72 90 1 0 0 0 1 0 0 0 1 3010.dat"
        , "1 19 0 -80 90 1 0 0 0 1 0 0 0 1 3069b.dat"
        , "1 14 30 -80 90 1 0 0 0 1 0 0 0 1 6141.dat"
        , "1 14 -30 -80 90 1 0 0 0 1 0 0 0 1 6141.dat"
        , "0 // Bowsprit"
        , "1 308 0 -64 -110 1 0 0 0 1 0 0 0 1 15573.dat"
        , "1 308 0 -72 -140 0 0 1 0 1 0 -1 0 0 3710.dat"
        , "0 NOFILE"
        ]


{-| LDraw MPD text of the sloop: 4 x 9 stud hull (12 studs with the
bowsprit), one mast, four cannons.
-}
sloop : String
sloop =
    String.join "\n"
        [ "0 FILE npc-sloop.ldr"
        , "0 NPC Sloop"
        , "0 Name: npc-sloop.ldr"
        , "0 Author: Brick Sails"
        , "0 !LICENSE Licensed under CC BY 4.0 : see CAreadme.txt"
        , "0 // Microscale single-masted sloop: 4 x 9 stud hull with a raked bow, bowsprit,"
        , "0 // raised quarterdeck with helm and lanterns, four deck cannons and white brick"
        , "0 // square sails (the sail colour, 15)."
        , "0 // Hull: one grouped submodel, moved as one piece"
        , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 npc-sloop-hull.ldr"
        , "0 // Cannons, two a side (32828 plate 1 x 1 round with bar)"
        , "1 0 30 -40 -30 1 0 0 0 1 0 0 0 1 32828.dat"
        , "1 0 -30 -40 -30 -1 0 0 0 1 0 0 0 -1 32828.dat"
        , "1 0 30 -40 30 1 0 0 0 1 0 0 0 1 32828.dat"
        , "1 0 -30 -40 30 -1 0 0 0 1 0 0 0 -1 32828.dat"
        , "0 // Mast: jumper step, lower mast, yard and course"
        , "1 70 0 -40 0 1 0 0 0 1 0 0 0 1 87580.dat"
        , "1 70 0 -184 0 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 70 0 -192 -10 1 0 0 0 1 0 0 0 1 3021.dat"
        , "1 15 0 -184 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -160 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -136 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -112 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "0 // Mast: topmast, topsail yard and topsail"
        , "1 70 0 -216 0 1 0 0 0 1 0 0 0 1 3062b.dat"
        , "1 70 0 -240 0 1 0 0 0 1 0 0 0 1 3062b.dat"
        , "1 70 0 -248 -10 1 0 0 0 1 0 0 0 1 3021.dat"
        , "1 15 0 -240 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "1 15 0 -216 -20 1 0 0 0 1 0 0 0 1 3622.dat"
        , "0 // Mast: pole and flag"
        , "1 70 0 -256 0 1 0 0 0 1 0 0 0 1 3957a.dat"
        , "1 4 0 -332 0 1 0 0 0 1 0 0 0 1 2335.dat"
        , "0 NOFILE"
        , "0 FILE npc-sloop-hull.ldr"
        , "0 NPC Sloop hull (grouped)"
        , "0 Name: npc-sloop-hull.ldr"
        , "0 Author: Brick Sails"
        , "0 // Keel layer: 2 x 4 bricks and an inverted slope forefoot"
        , "1 28 -20 0 -50 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 28 -20 0 0 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 28 -20 0 60 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 28 20 0 -50 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 28 20 0 0 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 28 20 0 60 1 0 0 0 1 0 0 0 1 3003.dat"
        , "0 // Upper hull: raked bow, tan sides with dark gunports"
        , "1 28 -20 -24 -70 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 28 -20 -24 -50 1 0 0 0 1 0 0 0 1 3004.dat"
        , "1 28 20 -24 -70 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 28 20 -24 -50 1 0 0 0 1 0 0 0 1 3004.dat"
        , "1 19 0 -24 -20 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 19 0 -24 40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 19 30 -24 -30 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 30 -24 -10 0 0 -1 0 1 0 1 0 0 87087.dat"
        , "1 72 48 -14 -10 0 -1 0 1 0 0 0 0 1 6141.dat"
        , "1 19 30 -24 10 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 30 -24 30 0 0 -1 0 1 0 1 0 0 87087.dat"
        , "1 72 48 -14 30 0 -1 0 1 0 0 0 0 1 6141.dat"
        , "1 19 30 -24 50 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 30 -24 70 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 -30 -24 -30 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 -30 -24 -10 0 0 1 0 1 0 -1 0 0 87087.dat"
        , "1 72 -48 -14 -10 0 1 0 -1 0 0 0 0 1 6141.dat"
        , "1 19 -30 -24 10 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 -30 -24 30 0 0 1 0 1 0 -1 0 0 87087.dat"
        , "1 72 -48 -14 30 0 1 0 -1 0 0 0 0 1 6141.dat"
        , "1 19 -30 -24 50 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 -30 -24 70 1 0 0 0 1 0 0 0 1 3005.dat"
        , "0 // Deck"
        , "1 70 -20 -32 -60 0 0 1 0 1 0 -1 0 0 3020.dat"
        , "1 70 -20 -32 10 0 0 1 0 1 0 -1 0 0 3021.dat"
        , "1 70 20 -32 -60 0 0 1 0 1 0 -1 0 0 3020.dat"
        , "1 70 20 -32 10 0 0 1 0 1 0 -1 0 0 3021.dat"
        , "0 // Raised quarterdeck with helm and stern lanterns"
        , "1 28 0 -48 60 1 0 0 0 1 0 0 0 1 3001.dat"
        , "1 0 0 -56 50 1 0 0 0 1 0 0 0 1 3829c01.dat"
        , "1 19 0 -56 70 1 0 0 0 1 0 0 0 1 3069b.dat"
        , "1 14 30 -56 70 1 0 0 0 1 0 0 0 1 6141.dat"
        , "1 14 -30 -56 70 1 0 0 0 1 0 0 0 1 6141.dat"
        , "0 // Bowsprit"
        , "1 308 0 -40 -90 1 0 0 0 1 0 0 0 1 15573.dat"
        , "1 308 0 -48 -120 0 0 1 0 1 0 -1 0 0 3710.dat"
        , "0 NOFILE"
        ]
