module Sails.Boats exposing (all, byName, custom, customName, default)

{-| The four toy boats on offer. They differ only a little: a faster boat
turns wider, a nimbler one is a touch slower.

Every boat is a real brick model: the LDraw text below lists the bricks,
plates, slopes, round bricks, flags and tiles it is built from, with the bow
towards LDraw -Z and LDraw -Y up. `Sails.ShipMesh` turns the parts into
geometry through the brick pipeline, so nothing on screen is a made-up shape.

-}

import Sails.Types exposing (Boat, BoatKind(..))


default : Boat
default =
    sloop


all : List Boat
all =
    [ sloop, cutter, schooner, catamaran ]


{-| Name of the boat picker entry for a ship loaded from a build link.
-}
customName : String
customName =
    "Your ship"


{-| A ship loaded from a build link. It handles like the sloop; its look is
the loaded build.
-}
custom : String -> Boat
custom title =
    { kind = CustomShip
    , name = customName
    , description =
        if String.isEmpty (String.trim title) then
            "Your own build."

        else
            "Your build: " ++ title
    , ldraw = ""
    , sailColour = Nothing
    , topSpeed = sloop.topSpeed
    , turnRate = sloop.turnRate
    }


byName : String -> Boat
byName name =
    all
        |> List.filter (\b -> b.name == name)
        |> List.head
        |> Maybe.withDefault default


sloop : Boat
sloop =
    { kind = Sloop
    , name = "Sloop"
    , description = "Balanced all-rounder."
    , ldraw = sloopLdr
    , sailColour = Just 15
    , topSpeed = 9.0
    , turnRate = 95.0
    }


cutter : Boat
cutter =
    { kind = Cutter
    , name = "Cutter"
    , description = "Nimble: turns on a stud."
    , ldraw = cutterLdr
    , sailColour = Just 14
    , topSpeed = 8.6
    , turnRate = 125.0
    }


schooner : Boat
schooner =
    { kind = Schooner
    , name = "Schooner"
    , description = "Two masts, quick on a reach."
    , ldraw = schoonerLdr
    , sailColour = Just 4
    , topSpeed = 9.5
    , turnRate = 85.0
    }


catamaran : Boat
catamaran =
    { kind = Catamaran
    , name = "Catamaran"
    , description = "Fastest, but turns wide."
    , ldraw = catamaranLdr
    , sailColour = Just 322
    , topSpeed = 10.0
    , turnRate = 72.0
    }



-- MODELS
--
-- Layout of every hull: a 4-stud-wide body of 2 x 4 bricks with an inverted
-- 2 x 2 slope bow, a 2-wide keel underneath, a mast of 1 x 1 x 6 round bricks
-- on a 2 x 2 jumper plate, and sails stacked from 1 x N bricks.


{-| LDraw source of the sloop.
-}
sloopLdr : String
sloopLdr =
    String.join "\n"
        [ "0 FILE sloop.ldr"
        , "0 Name: Sloop"
        , "0 Author: Brick Sails"
        , "0 // Hull, upper layer"
        , "1 70 -20 0 -80 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 -20 0 -20 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 -20 0 40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 -20 0 80 0 0 1 0 1 0 -1 0 0 3003.dat"
        , "1 70 -20 0 -110 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 70 20 0 -80 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 20 0 -20 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 20 0 40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 20 0 80 0 0 1 0 1 0 -1 0 0 3003.dat"
        , "1 70 20 0 -110 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "0 // Keel"
        , "1 70 0 24 -40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 70 0 24 20 0 0 1 0 1 0 -1 0 0 3003.dat"
        , "1 70 0 24 -90 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 70 0 24 50 -1 0 0 0 1 0 0 0 -1 3660b.dat"
        , "0 // Cabin"
        , "1 19 0 -24 60 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 70 0 -32 60 1 0 0 0 1 0 0 0 1 3068b.dat"
        , "0 // Mast, boom and pennant"
        , "1 70 0 -8 -20 1 0 0 0 1 0 0 0 1 87580.dat"
        , "1 70 0 -152 -20 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 70 0 -296 -20 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 70 0 -304 -20 1 0 0 0 1 0 0 0 1 3957a.dat"
        , "1 70 0 -40 50 0 0 1 0 1 0 -1 0 0 6636.dat"
        , "1 4 0 -380 -20 1 0 0 0 1 0 0 0 1 2335.dat"
        , "0 // Mainsail and jib"
        , "1 15 0 -64 50 0 0 1 0 1 0 -1 0 0 3009.dat"
        , "1 15 0 -88 50 0 0 1 0 1 0 -1 0 0 3009.dat"
        , "1 15 0 -112 50 0 0 1 0 1 0 -1 0 0 3009.dat"
        , "1 15 0 -136 30 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 15 0 -160 30 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 15 0 -184 20 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 15 0 -208 20 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 15 0 -232 10 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 15 0 -256 10 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 15 0 -280 0 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 15 0 -304 0 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 15 0 -64 -70 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 15 0 -88 -70 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 15 0 -112 -60 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 15 0 -136 -60 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 15 0 -160 -50 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 15 0 -184 -50 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 15 0 -208 -40 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 15 0 -232 -40 0 0 1 0 1 0 -1 0 0 3005.dat"
        ]


{-| LDraw source of the cutter.
-}
cutterLdr : String
cutterLdr =
    String.join "\n"
        [ "0 FILE cutter.ldr"
        , "0 Name: Cutter"
        , "0 Author: Brick Sails"
        , "0 // Hull, upper layer"
        , "1 1 -20 0 -40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 1 -20 0 40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 1 -20 0 -90 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 1 20 0 -40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 1 20 0 40 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 1 20 0 -90 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "0 // Keel"
        , "1 1 0 24 -20 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 1 0 24 -70 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 1 0 24 30 -1 0 0 0 1 0 0 0 -1 3660b.dat"
        , "0 // Cabin and bowsprit"
        , "1 15 0 -24 50 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 1 0 -32 50 1 0 0 0 1 0 0 0 1 3068b.dat"
        , "1 15 0 -8 -120 0 0 1 0 1 0 -1 0 0 2431.dat"
        , "0 // Mast, boom and pennant"
        , "1 15 0 -8 -20 1 0 0 0 1 0 0 0 1 87580.dat"
        , "1 15 0 -152 -20 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 15 0 -296 -20 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 15 0 -40 30 0 0 1 0 1 0 -1 0 0 2431.dat"
        , "1 4 0 -292 -20 1 0 0 0 1 0 0 0 1 2335.dat"
        , "0 // Mainsail, staysail and jib"
        , "1 14 0 -64 30 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 14 0 -88 30 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 14 0 -112 30 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 14 0 -136 20 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 14 0 -160 20 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 14 0 -184 10 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 14 0 -208 10 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 14 0 -232 0 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 14 0 -256 0 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 14 0 -280 0 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 14 0 -64 -60 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 14 0 -88 -60 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 14 0 -112 -50 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 14 0 -136 -50 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 14 0 -160 -40 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 14 0 -184 -40 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 14 0 -64 -110 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 14 0 -88 -110 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 14 0 -112 -100 0 0 1 0 1 0 -1 0 0 3005.dat"
        ]


{-| LDraw source of the schooner.
-}
schoonerLdr : String
schoonerLdr =
    String.join "\n"
        [ "0 FILE schooner.ldr"
        , "0 Name: Schooner"
        , "0 Author: Brick Sails"
        , "0 // Hull, upper layer"
        , "1 72 -20 0 -80 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 72 -20 0 0 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 72 -20 0 80 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 72 -20 0 -130 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 72 20 0 -80 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 72 20 0 0 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 72 20 0 80 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 72 20 0 -130 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "0 // Keel"
        , "1 72 0 24 -60 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 72 0 24 20 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 72 0 24 70 1 0 0 0 1 0 0 0 1 3004.dat"
        , "1 72 0 24 -110 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 72 0 24 90 -1 0 0 0 1 0 0 0 -1 3660b.dat"
        , "0 // Deckhouse"
        , "1 19 0 -24 -10 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 72 0 -32 -10 1 0 0 0 1 0 0 0 1 3068b.dat"
        , "0 // Foremast and mainmast"
        , "1 70 0 -8 -60 1 0 0 0 1 0 0 0 1 87580.dat"
        , "1 70 0 -152 -60 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 70 0 -160 -60 1 0 0 0 1 0 0 0 1 3957a.dat"
        , "1 70 0 -8 40 1 0 0 0 1 0 0 0 1 87580.dat"
        , "1 70 0 -152 40 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 70 0 -296 40 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 70 0 -304 40 1 0 0 0 1 0 0 0 1 3957a.dat"
        , "1 70 0 -40 90 0 0 1 0 1 0 -1 0 0 2431.dat"
        , "1 14 0 -380 40 1 0 0 0 1 0 0 0 1 2335.dat"
        , "0 // Foresail, mainsail and jib"
        , "1 4 0 -64 -10 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 4 0 -88 -10 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 4 0 -112 -20 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 4 0 -136 -20 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 4 0 -160 -30 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 4 0 -184 -30 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 4 0 -208 -40 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 4 0 -64 90 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 4 0 -88 90 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 4 0 -112 90 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 4 0 -136 80 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 4 0 -160 80 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 4 0 -184 70 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 4 0 -208 70 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 4 0 -232 70 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 4 0 -256 60 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 4 0 -280 60 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 4 0 -304 60 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 4 0 -64 -100 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 4 0 -88 -100 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 4 0 -112 -90 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 4 0 -136 -80 0 0 1 0 1 0 -1 0 0 3005.dat"
        ]


{-| LDraw source of the catamaran.
-}
catamaranLdr : String
catamaranLdr =
    String.join "\n"
        [ "0 FILE catamaran.ldr"
        , "0 Name: Catamaran"
        , "0 Author: Brick Sails"
        , "0 // Two hulls"
        , "1 15 -60 0 -60 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 15 -60 0 20 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 15 -60 0 80 0 0 1 0 1 0 -1 0 0 3003.dat"
        , "1 15 -60 0 -110 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "1 15 60 0 -60 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 15 60 0 20 0 0 1 0 1 0 -1 0 0 3001.dat"
        , "1 15 60 0 80 0 0 1 0 1 0 -1 0 0 3003.dat"
        , "1 15 60 0 -110 1 0 0 0 1 0 0 0 1 3660b.dat"
        , "0 // Bridge deck"
        , "1 71 0 -8 -40 1 0 0 0 1 0 0 0 1 3460.dat"
        , "1 71 0 -8 -20 1 0 0 0 1 0 0 0 1 3460.dat"
        , "1 71 0 -8 0 1 0 0 0 1 0 0 0 1 3460.dat"
        , "1 71 0 -8 20 1 0 0 0 1 0 0 0 1 3460.dat"
        , "0 // Mast and pennant"
        , "1 15 0 -16 -10 1 0 0 0 1 0 0 0 1 87580.dat"
        , "1 15 0 -160 -10 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 15 0 -304 -10 1 0 0 0 1 0 0 0 1 43888.dat"
        , "1 15 0 -312 -10 1 0 0 0 1 0 0 0 1 3957a.dat"
        , "1 4 0 -388 -10 1 0 0 0 1 0 0 0 1 2335.dat"
        , "0 // Mainsail and jib"
        , "1 322 0 -72 60 0 0 1 0 1 0 -1 0 0 3009.dat"
        , "1 322 0 -96 60 0 0 1 0 1 0 -1 0 0 3009.dat"
        , "1 322 0 -120 40 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 322 0 -144 40 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 322 0 -168 30 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 322 0 -192 30 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 322 0 -216 20 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 322 0 -240 20 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 322 0 -264 10 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 322 0 -288 10 0 0 1 0 1 0 -1 0 0 3005.dat"
        , "1 322 0 -72 -60 0 0 1 0 1 0 -1 0 0 3010.dat"
        , "1 322 0 -96 -50 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 322 0 -120 -50 0 0 1 0 1 0 -1 0 0 3622.dat"
        , "1 322 0 -144 -40 0 0 1 0 1 0 -1 0 0 3004.dat"
        , "1 322 0 -168 -30 0 0 1 0 1 0 -1 0 0 3005.dat"
        ]
