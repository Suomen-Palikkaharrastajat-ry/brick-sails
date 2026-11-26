module Sails.Scenery exposing
    ( Piece
    , all
    , blueCoatStandard
    , chest
    , fort
    , houses
    , isScenery
    , key
    , metersPerLdu
    , palmTree
    , pier
    , pirateFlag
    , pirateStandard
    , redCoatStandard
    , reef
    , seaArch
    , seabird
    , wreck
    )

{-| The brick-built scenery of the treasure islands: a row of pastel colonial
houses, a harbour pier and the pirates' treasure chest on the islands, and
the palm trees, the fort's watchtower, the coral reefs, the pirate wreck and
the pirate flags on the finish gate around them.

Like the boats in `Sails.Boats`, every piece is a real brick model: the
LDraw text lists the catalog bricks, plates, slopes and round bricks it is
built from, `Main` fetches the parts through the part resolver and
`Sails.ShipMesh.buildScenery` turns them into geometry. Each model stands
on LDraw y = 0 (the ground) with its front towards LDraw -Z, which faces
out from the island when drawn.

The palms, the fort, the reefs, the wreck and the pirate flags are plain
LDraw text for `Game.BrickProps` instead (see `Sails.Render.propSources`):
they are drawn many times, fitted to the size they are drawn at, and are
decoration only, with nothing for the boats to bump into.

-}

import Game.BrickModels as BrickModels


{-| A scenery model: its name and LDraw text.
-}
type alias Piece =
    { name : String
    , ldraw : String
    }


{-| Scale of the scenery: a brick is 1.2 m tall and a stud 1 m apart, so a
house is about the size of a boat's sail on the course.
-}
metersPerLdu : Float
metersPerLdu =
    0.035


all : List Piece
all =
    [ houses, pier, chest, seaArch ]


{-| Key of a piece in `Main`'s documents and meshes, apart from the boats.
-}
key : Piece -> String
key piece =
    "scenery:" ++ piece.name


isScenery : String -> Bool
isScenery name =
    String.startsWith "scenery:" name


upright : String
upright =
    "1 0 0 0 1 0 0 0 1"


turned : String
turned =
    "-1 0 0 0 1 0 0 0 -1"


alongZ : String
alongZ =
    "0 0 1 0 1 0 -1 0 0"


line : Int -> ( Float, Float, Float ) -> String -> String -> String
line colour ( x, y, z ) matrix part =
    String.join " "
        [ "1"
        , String.fromInt colour
        , String.fromFloat x
        , String.fromFloat y
        , String.fromFloat z
        , matrix
        , part
        ]


{-| One house: a 4 x 4 stud block of 2 x 4 bricks `layers` high, and a gable roof of 2 x 4 slopes.
-}
house : Float -> Int -> Int -> Int -> List String
house x layers wall roof =
    let
        layerY k =
            -24 * toFloat (k + 1)

        walls =
            List.range 0 (layers - 1)
                |> List.concatMap
                    (\k ->
                        [ line wall ( x, layerY k, -20 ) upright "3001.dat"
                        , line wall ( x, layerY k, 20 ) upright "3001.dat"
                        ]
                    )

        top =
            layerY layers

        roofs =
            [ line roof ( x, top, -10 ) upright "3037.dat"
            , line roof ( x, top, 10 ) turned "3037.dat"
            ]
    in
    walls ++ roofs


{-| A grand colonial town hall with a four-layer main hall, flat balustrade roof,
and a central clock tower and spire.
-}
townHall : Float -> List String
townHall x =
    let
        layerY k =
            -24 * toFloat (k + 1)

        mainWalls =
            List.range 0 3
                |> List.concatMap
                    (\k ->
                        [ line 18 ( x, layerY k, -20 ) upright "3001.dat"
                        , line 18 ( x, layerY k, 20 ) upright "3001.dat"
                        ]
                    )

        mainRoof =
            [ line 4 ( x, layerY 4, -20 ) upright "3020.dat"
            , line 4 ( x, layerY 4, 20 ) upright "3020.dat"
            ]

        tower =
            [ line 15 ( x, layerY 5, 0 ) upright "3003.dat"
            , line 15 ( x, layerY 6, 0 ) upright "3003.dat"
            ]
    in
    mainWalls ++ mainRoof ++ tower


{-| A colonial Caribbean brick town: five buildings side by side along a cobblestone quay.
-}
houses : Piece
houses =
    { name = "houses"
    , ldraw =
        String.join "\n"
            ([ "0 FILE houses.ldr"
             , "0 Name: Brick Sails island houses"
             , "0 Author: Brick Sails"
             , "0 BFC CERTIFY CCW"
             , "0 // Cobblestone quay walkway"
             , line 71 ( -80, -8, -60 ) upright "3034.dat"
             , line 71 ( 80, -8, -60 ) upright "3034.dat"
             , "0 // Waterfront Tavern"
             ]
                ++ house -160 3 320 72
                ++ [ "0 // Colonial Apothecary" ]
                ++ house -80 3 118 4
                ++ [ "0 // Governor's Town Hall and Belfry" ]
                ++ townHall 0
                ++ [ "0 // Colonial Residence" ]
                ++ house 80 3 77 1
                ++ [ "0 // Customs Warehouse" ]
                ++ house 160 4 28 70
                ++ [ "0 // Quayside Lanterns"
                   , line 0 ( -70, -32, -50 ) upright "3062b.dat"
                   , line 14 ( -70, -40, -50 ) upright "6141.dat"
                   , line 0 ( 70, -32, -50 ) upright "3062b.dat"
                   , line 14 ( 70, -40, -50 ) upright "6141.dat"
                   ]
            )
    }


{-| A wooden harbour pier, four studs wide, with round brick pilings, mooring bollards,
and dock lanterns. Runs from the shore out along -Z.
-}
pier : Piece
pier =
    let
        deckY =
            -48

        post ( x, z ) =
            [ line 72 ( x, -48, z ) upright "3062b.dat"
            , line 72 ( x, -24, z ) upright "3062b.dat"
            ]

        posts =
            [ ( -30, -30 )
            , ( 30, -30 )
            , ( -30, -90 )
            , ( 30, -90 )
            , ( -30, -150 )
            , ( 30, -150 )
            , ( -30, -210 )
            , ( 30, -210 )
            , ( -30, -270 )
            , ( 30, -270 )
            ]

        deckPlates =
            List.concatMap
                (\z ->
                    [ line 70 ( -20, deckY - 8, z ) alongZ "3034.dat"
                    , line 70 ( 20, deckY - 8, z ) alongZ "3034.dat"
                    ]
                )
                [ -80, -240 ]
    in
    { name = "pier"
    , ldraw =
        String.join "\n"
            ([ "0 FILE pier.ldr"
             , "0 Name: Brick Sails harbour pier"
             , "0 Author: Brick Sails"
             , "0 BFC CERTIFY CCW"
             , "0 // Main walkway deck (4 studs wide, 16 studs long)"
             ]
                ++ deckPlates
                ++ [ "0 // Submerged pilings (reach 1.4m down below waterline, clear of wave troughs)" ]
                ++ List.concatMap post posts
                ++ [ "0 // Mooring bollards"
                   , line 0 ( -30, deckY - 32, -30 ) upright "3062b.dat"
                   , line 0 ( 30, deckY - 32, -30 ) upright "3062b.dat"
                   , line 0 ( -30, deckY - 32, -150 ) upright "3062b.dat"
                   , line 0 ( 30, deckY - 32, -150 ) upright "3062b.dat"
                   , line 0 ( -30, deckY - 32, -270 ) upright "3062b.dat"
                   , line 0 ( 30, deckY - 32, -270 ) upright "3062b.dat"
                   , "0 // Dock lanterns"
                   , line 0 ( -30, deckY - 32, -210 ) upright "3062b.dat"
                   , line 14 ( -30, deckY - 40, -210 ) upright "6141.dat"
                   , line 0 ( 30, deckY - 32, -210 ) upright "3062b.dat"
                   , line 14 ( 30, deckY - 40, -210 ) upright "6141.dat"
                   ]
            )
    }


{-| A natural wave-carved stone arch standing in the sea: two grey stone pillars
anchored in the water, bridged by a stone arch span with sea spray corbels and
guano-topped rock peaks.
-}
seaArch : Piece
seaArch =
    let
        layerY k =
            -24 * toFloat (k + 1)

        leftPillar =
            List.range 0 4
                |> List.concatMap
                    (\k ->
                        [ line 72 ( -80, layerY k, -20 ) upright "3001.dat"
                        , line 72 ( -80, layerY k, 20 ) upright "3001.dat"
                        ]
                    )

        rightPillar =
            List.range 0 4
                |> List.concatMap
                    (\k ->
                        [ line 72 ( 80, layerY k, -20 ) upright "3001.dat"
                        , line 72 ( 80, layerY k, 20 ) upright "3001.dat"
                        ]
                    )

        span =
            [ line 71 ( 0, -128, 0 ) upright "3034.dat"
            , line 71 ( 0, -136, 0 ) upright "3020.dat"
            ]

        peaks =
            [ line 72 ( -60, -152, 0 ) upright "3003.dat"
            , line 72 ( 60, -152, 0 ) upright "3003.dat"
            ]
    in
    { name = "seaArch"
    , ldraw =
        String.join "\n"
            ([ "0 FILE seaArch.ldr"
             , "0 Name: Brick Sails coastal sea arch"
             , "0 Author: Brick Sails"
             , "0 BFC CERTIFY CCW"
             , "0 // Pillars"
             ]
                ++ leftPillar
                ++ rightPillar
                ++ [ "0 // Arch span" ]
                ++ span
                ++ [ "0 // Peaks" ]
                ++ peaks
            )
    }


{-| The pirates' chest: a reddish-brown 2 x 4 box with a gold band and
lock, heaped with gold coins, and a black flag on a pole beside it.
-}
chest : Piece
chest =
    { name = "chest"
    , ldraw =
        String.join "\n"
            [ "0 FILE chest.ldr"
            , "0 Name: Brick Sails treasure chest"
            , "0 Author: Brick Sails"
            , "0 BFC CERTIFY CCW"
            , "0 // Box, band and lid"
            , line 70 ( 0, -24, 0 ) upright "3001.dat"
            , line 14 ( 0, -32, 0 ) upright "3020.dat"
            , line 70 ( 0, -56, 0 ) upright "3001.dat"
            , "0 // Gold"
            , line 14 ( -30, -64, -10 ) upright "6141.dat"
            , line 14 ( -10, -64, 10 ) upright "6141.dat"
            , line 14 ( 10, -64, -10 ) upright "6141.dat"
            , line 14 ( 30, -64, 10 ) upright "6141.dat"
            , line 14 ( -10, -72, 10 ) upright "6141.dat"
            , line 14 ( 10, -72, -10 ) upright "6141.dat"
            , line 14 ( 10, -80, -10 ) upright "6141.dat"
            , "0 // Pirate flag"
            , line 0 ( 70, -24, 10 ) upright "3062b.dat"
            , line 0 ( 70, -32, 10 ) upright "3957a.dat"
            , line 0 ( 70, -108, 10 ) upright "2335.dat"
            ]
    }


{-| A palm tree: a leaning trunk of reddish-brown 1 x 1 bricks, coconuts of
dark tan round bricks under the crown, and eight green 2 x 2 slopes drooping
out from a green plate as the fronds. Shared with Brick Racer as
`Game.BrickModels.palmTree`.
-}
palmTree : String
palmTree =
    BrickModels.palmTree


{-| The fort on the first island: a stone keep of 2 x 4 bricks with a
wooden gate and battlements, a pirate flag on one corner, and a watchtower of
grey 2 x 2 bricks carrying a lookout platform with a torch of trans-yellow
round bricks, where `Sails.Render` lights its sweeping beam.
-}
fort : String
fort =
    let
        layerY k =
            -24 * toFloat (k + 1)

        stone k =
            if modBy 2 k == 0 then
                72

            else
                71

        keep =
            List.range 0 1
                |> List.concatMap
                    (\k ->
                        [ line 72 ( 0, layerY k, -20 ) upright "3001.dat"
                        , line 72 ( 0, layerY k, 20 ) upright "3001.dat"
                        ]
                    )

        tower =
            List.range 2 6
                |> List.map (\k -> line (stone k) ( 0, layerY k, 0 ) upright "3003.dat")

        top =
            layerY 6
    in
    String.join "\n"
        ([ "0 FILE fort.ldr"
         , "0 Name: Brick Sails fort and watchtower"
         , "0 Author: Brick Sails"
         , "0 BFC CERTIFY CCW"
         , "0 // Keep, gate and battlements"
         ]
            ++ keep
            ++ [ line 70 ( 0, layerY 0, -50 ) upright "3004.dat"
               , line 71 ( -30, layerY 2, -30 ) upright "3005.dat"
               , line 71 ( 30, layerY 2, -30 ) upright "3005.dat"
               , line 71 ( -30, layerY 2, 30 ) upright "3005.dat"
               , "0 // Pirate flag"
               , line 0 ( 30, layerY 2, 30 ) upright "3062b.dat"
               , line 0 ( 30, layerY 2 - 8, 30 ) upright "3957a.dat"
               , line 0 ( 30, layerY 2 - 84, 30 ) upright "2335.dat"
               , "0 // Watchtower"
               ]
            ++ tower
            ++ [ "0 // Lookout platform and torch"
               , line 70 ( 0, top - 8, 0 ) upright "3020.dat"
               , line 70 ( 0, top - 8, 0 ) alongZ "3020.dat"
               , line 71 ( -30, top - 32, 0 ) upright "3005.dat"
               , line 71 ( 30, top - 32, 0 ) upright "3005.dat"
               , line 71 ( 0, top - 32, -30 ) upright "3005.dat"
               , line 71 ( 0, top - 32, 30 ) upright "3005.dat"
               , line 70 ( 0, top - 32, 0 ) upright "3062b.dat"
               , line 46 ( 0, top - 56, 0 ) upright "3062b.dat"
               , line 46 ( 0, top - 80, 0 ) upright "4589.dat"
               ]
        )


{-| A coral reef breaking the surface: grey rocks of a 2 x 4 brick, a 2 x 2
brick and a slope, with coral, pink and yellow round bricks and a green weed
on top.
-}
reef : String
reef =
    String.join "\n"
        [ "0 FILE reef.ldr"
        , "0 Name: Brick Sails coral reef"
        , "0 Author: Brick Sails"
        , "0 BFC CERTIFY CCW"
        , "0 // Rocks"
        , line 72 ( 0, -24, 0 ) upright "3001.dat"
        , line 71 ( -60, -24, 20 ) upright "3003.dat"
        , line 71 ( 60, -24, -20 ) upright "3039.dat"
        , "0 // Coral and weed"
        , line 353 ( -30, -48, -10 ) upright "3062b.dat"
        , line 353 ( -30, -72, -10 ) upright "3062b.dat"
        , line 13 ( 10, -48, 10 ) upright "3062b.dat"
        , line 14 ( 30, -32, -10 ) upright "6141.dat"
        , line 14 ( -10, -32, -10 ) upright "6141.dat"
        , line 2 ( -60, -48, 20 ) upright "3005.dat"
        , line 10 ( -60, -72, 20 ) upright "3005.dat"
        ]


{-| A pirate wreck run aground: a broken reddish-brown hull with its bow
slope and a split deck of planks, a mast leaning over with a black flag, a
loose barrel and a little spilt gold.
-}
wreck : String
wreck =
    let
        leaning =
            "0.9397 -0.342 0 0.342 0.9397 0 0 0 1"
    in
    String.join "\n"
        [ "0 FILE wreck.ldr"
        , "0 Name: Brick Sails pirate wreck"
        , "0 Author: Brick Sails"
        , "0 BFC CERTIFY CCW"
        , "0 // Hull"
        , line 70 ( -20, -24, -20 ) alongZ "3001.dat"
        , line 70 ( 20, -24, -20 ) alongZ "3001.dat"
        , line 70 ( -20, -24, 60 ) alongZ "3001.dat"
        , line 70 ( 20, -24, 60 ) alongZ "3001.dat"
        , line 70 ( -20, -48, 60 ) alongZ "3001.dat"
        , line 70 ( 20, -48, 60 ) alongZ "3001.dat"
        , line 70 ( 0, -48, -40 ) upright "3037.dat"
        , "0 // Deck"
        , line 28 ( -10, -56, 60 ) alongZ "3460.dat"
        , line 28 ( 10, -56, 60 ) alongZ "3460.dat"
        , "0 // Mast and flag"
        , line 70 ( 0, -80, 60 ) upright "3062b.dat"
        , line 0 ( 0, -88, 60 ) leaning "3957a.dat"
        , line 0 ( 26, -159.4, 60 ) leaning "2335.dat"
        , "0 // Barrel and gold"
        , line 70 ( 70, -24, 0 ) upright "2489.dat"
        , line 14 ( -10, -64, 100 ) upright "6141.dat"
        , line 14 ( 10, -64, 110 ) upright "6141.dat"
        ]


{-| A pirate flag for the finish gate: a black flag on an antenna mast over
a post of reddish-brown round bricks.
-}
pirateFlag : String
pirateFlag =
    String.join "\n"
        [ "0 FILE pirate-flag.ldr"
        , "0 Name: Brick Sails pirate flag"
        , "0 Author: Brick Sails"
        , "0 BFC CERTIFY CCW"
        , line 70 ( 0, -24, 0 ) upright "3062b.dat"
        , line 70 ( 0, -48, 0 ) upright "3062b.dat"
        , line 0 ( 0, -56, 0 ) upright "3957a.dat"
        , line 0 ( 0, -132, 0 ) upright "2335.dat"
        ]


{-| The Pirates' faction standard for citadel piers: a black and red post
ringed with gold studs, flying a black skull flag on an antenna mast.
-}
pirateStandard : String
pirateStandard =
    String.join "\n"
        [ "0 FILE pirate-standard.ldr"
        , "0 Name: Brick Sails pirate faction standard"
        , "0 Author: Brick Sails"
        , "0 BFC CERTIFY CCW"
        , line 14 ( 0, -8, 0 ) upright "6141.dat"
        , line 0 ( 0, -32, 0 ) upright "3062b.dat"
        , line 14 ( 0, -40, 0 ) upright "6141.dat"
        , line 4 ( 0, -64, 0 ) upright "3062b.dat"
        , line 14 ( 0, -72, 0 ) upright "6141.dat"
        , line 0 ( 0, -80, 0 ) upright "3957a.dat"
        , line 0 ( 0, -156, 0 ) upright "2335.dat"
        , line 14 ( 0, -180, 0 ) upright "6141.dat"
        ]


{-| The BlueCoats' imperial faction standard for citadel piers: an alternating
blue and white post topped with an azure blue imperial flag and a golden cone finial.
-}
blueCoatStandard : String
blueCoatStandard =
    String.join "\n"
        [ "0 FILE bluecoat-standard.ldr"
        , "0 Name: Brick Sails bluecoat faction standard"
        , "0 Author: Brick Sails"
        , "0 BFC CERTIFY CCW"
        , line 1 ( 0, -24, 0 ) upright "3062b.dat"
        , line 15 ( 0, -32, 0 ) upright "6141.dat"
        , line 15 ( 0, -56, 0 ) upright "3062b.dat"
        , line 1 ( 0, -64, 0 ) upright "6141.dat"
        , line 15 ( 0, -72, 0 ) upright "3957a.dat"
        , line 1 ( 0, -148, 0 ) upright "2335.dat"
        , line 14 ( 0, -168, 0 ) upright "6141.dat"
        , line 14 ( 0, -192, 0 ) upright "4589.dat"
        ]


{-| The RedCoats' royal faction standard for citadel piers: an alternating
red and white post carrying a royal red flag, crowned with a brass lantern.
-}
redCoatStandard : String
redCoatStandard =
    String.join "\n"
        [ "0 FILE redcoat-standard.ldr"
        , "0 Name: Brick Sails redcoat faction standard"
        , "0 Author: Brick Sails"
        , "0 BFC CERTIFY CCW"
        , line 4 ( 0, -24, 0 ) upright "3062b.dat"
        , line 15 ( 0, -32, 0 ) upright "6141.dat"
        , line 15 ( 0, -56, 0 ) upright "3062b.dat"
        , line 4 ( 0, -64, 0 ) upright "6141.dat"
        , line 4 ( 0, -72, 0 ) upright "3957a.dat"
        , line 4 ( 0, -148, 0 ) upright "2335.dat"
        , line 14 ( 0, -168, 0 ) upright "6141.dat"
        , line 46 ( 0, -192, 0 ) upright "3062b.dat"
        , line 14 ( 0, -216, 0 ) upright "4589.dat"
        ]


{-| A tiny brick-built seagull: white 1x1 plate body, yellow 1x1 round plate beak, and angled 1x1 wings.
-}
seabird : String
seabird =
    String.join "\n"
        [ "0 FILE seabird.ldr"
        , "0 Name: Brick Sails seabird"
        , "0 Author: Brick Sails"
        , "0 BFC CERTIFY CCW"
        , line 15 ( 0, 0, 0 ) upright "3024.dat"
        , line 14 ( 0, 0, -10 ) upright "6141.dat"
        , line 15 ( -10, 0, 0 ) upright "3024.dat"
        , line 15 ( 10, 0, 0 ) upright "3024.dat"
        ]
