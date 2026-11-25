module Sails.Scenery exposing
    ( Piece
    , all
    , chest
    , fort
    , houses
    , isScenery
    , key
    , metersPerLdu
    , palmTree
    , pier
    , pirateFlag
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
    0.05


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


{-| One house: a 4 x 4 stud block of 2 x 4 bricks `layers` high, a door
and two windows on the front, a gable roof of 2 x 4 slopes and a chimney.
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

        front =
            [ line 70 ( x, layerY 0, -50 ) upright "3004.dat"
            , line 70 ( x, layerY 1, -50 ) upright "3004.dat"
            , line 43 ( x - 30, layerY 2, -50 ) upright "3005.dat"
            , line 43 ( x + 30, layerY 2, -50 ) upright "3005.dat"
            ]

        top =
            layerY layers

        roofs =
            [ line roof ( x, top, -20 ) upright "3037.dat"
            , line roof ( x, top, 20 ) turned "3037.dat"
            , line 72 ( x + 30, top, 30 ) upright "3005.dat"
            , line 72 ( x + 30, top - 24, 30 ) upright "3005.dat"
            ]
    in
    walls ++ front ++ roofs


{-| A grand colonial town hall with a four-layer main hall, entrance steps,
and a central clock and bell tower rising three layers higher, topped with a
belfry, a golden bell, a slope roof and a gold spire.
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

        frontEntrance =
            [ line 71 ( x, -8, -50 ) upright "3020.dat"
            , line 70 ( x, layerY 0, -50 ) upright "3004.dat"
            , line 70 ( x, layerY 1, -50 ) upright "3004.dat"
            , line 14 ( x - 20, layerY 2, -50 ) upright "3005.dat"
            , line 14 ( x + 20, layerY 2, -50 ) upright "3005.dat"
            ]

        mainRoof =
            [ line 4 ( x, layerY 4, -20 ) upright "3037.dat"
            , line 4 ( x, layerY 4, 20 ) turned "3037.dat"
            ]

        towerBase =
            [ line 15 ( x - 10, layerY 4, -10 ) upright "3005.dat"
            , line 15 ( x + 10, layerY 4, -10 ) upright "3005.dat"
            , line 15 ( x - 10, layerY 4, 10 ) upright "3005.dat"
            , line 15 ( x + 10, layerY 4, 10 ) upright "3005.dat"
            ]

        belfryOpen =
            [ line 15 ( x - 10, layerY 5, -10 ) upright "3005.dat"
            , line 15 ( x + 10, layerY 5, -10 ) upright "3005.dat"
            , line 15 ( x - 10, layerY 5, 10 ) upright "3005.dat"
            , line 15 ( x + 10, layerY 5, 10 ) upright "3005.dat"
            , line 14 ( x, layerY 5, 0 ) upright "6141.dat"
            , line 14 ( x, layerY 5 - 8, 0 ) upright "3062b.dat"
            ]

        towerRoof =
            [ line 4 ( x, layerY 6, 0 ) upright "3037.dat"
            , line 14 ( x, layerY 6 - 24, 0 ) upright "3957a.dat"
            ]
    in
    mainWalls ++ frontEntrance ++ mainRoof ++ towerBase ++ belfryOpen ++ towerRoof


{-| A waterfront tavern with an overhanging upper storey, stone chimney,
and front entrance.
-}
tavern : Float -> List String
tavern x =
    let
        layerY k =
            -24 * toFloat (k + 1)

        groundFloor =
            [ line 320 ( x, layerY 0, -20 ) upright "3001.dat"
            , line 320 ( x, layerY 0, 20 ) upright "3001.dat"
            , line 70 ( x, layerY 0, -50 ) upright "3004.dat"
            ]

        upperFloors =
            List.range 1 3
                |> List.concatMap
                    (\k ->
                        [ line 70 ( x, layerY k, -20 ) upright "3001.dat"
                        , line 70 ( x, layerY k, 20 ) upright "3001.dat"
                        ]
                    )

        windows =
            [ line 14 ( x - 20, layerY 1, -50 ) upright "3005.dat"
            , line 14 ( x + 20, layerY 1, -50 ) upright "3005.dat"
            , line 14 ( x - 20, layerY 2, -50 ) upright "3005.dat"
            , line 14 ( x + 20, layerY 2, -50 ) upright "3005.dat"
            ]

        top =
            layerY 4

        roof =
            [ line 72 ( x, top, -20 ) upright "3037.dat"
            , line 72 ( x, top, 20 ) turned "3037.dat"
            , line 71 ( x + 20, top, 20 ) upright "3005.dat"
            , line 71 ( x + 20, top - 24, 20 ) upright "3005.dat"
            ]
    in
    groundFloor ++ upperFloors ++ windows ++ roof


{-| A colonial Caribbean brick town: five buildings side by side (a waterfront
tavern, an apothecary shop, the grand town hall with clock and bell tower, a
pink colonial residence, and a customs warehouse) along a wide cobblestone quay
with market stalls, a town well, defence cannons, street lanterns and cargo.
-}
houses : Piece
houses =
    { name = "houses"
    , ldraw =
        String.join "\n"
            ([ "0 FILE houses.ldr"
             , "0 Name: Brick Sails island houses"
             , "0 Author: Brick Sails"
             , "0 // Cobblestone quay street and plaza"
             , line 71 ( -120, -8, -70 ) upright "3034.dat"
             , line 71 ( -40, -8, -70 ) upright "3034.dat"
             , line 71 ( 40, -8, -70 ) upright "3034.dat"
             , line 71 ( 120, -8, -70 ) upright "3034.dat"
             , line 72 ( -50, -8, -90 ) upright "3020.dat"
             , line 72 ( 50, -8, -90 ) upright "3020.dat"
             , "0 // Waterfront Tavern"
             ]
                ++ tavern -140
                ++ [ "0 // Colonial Apothecary" ]
                ++ house -70 3 118 4
                ++ [ "0 // Governor's Town Hall and Belfry" ]
                ++ townHall 0
                ++ [ "0 // Colonial Residence" ]
                ++ house 70 3 77 1
                ++ [ "0 // Customs Warehouse" ]
                ++ house 140 4 28 70
                ++ [ "0 // Town Plaza Well"
                   , line 71 ( 0, -24, -90 ) upright "3062b.dat"
                   , line 1 ( 0, -32, -90 ) upright "6141.dat"
                   , line 70 ( -10, -48, -90 ) upright "3005.dat"
                   , line 70 ( 10, -48, -90 ) upright "3005.dat"
                   , line 70 ( 0, -56, -90 ) upright "3020.dat"
                   , "0 // Market Stalls"
                   , line 70 ( -105, -24, -90 ) upright "3004.dat"
                   , line 70 ( -105, -48, -90 ) upright "3005.dat"
                   , line 4 ( -110, -56, -90 ) upright "3024.dat"
                   , line 15 ( -100, -56, -90 ) upright "3024.dat"
                   , line 14 ( -105, -32, -90 ) upright "6141.dat"
                   , line 70 ( 105, -24, -90 ) upright "3004.dat"
                   , line 70 ( 105, -48, -90 ) upright "3005.dat"
                   , line 1 ( 100, -56, -90 ) upright "3024.dat"
                   , line 14 ( 110, -56, -90 ) upright "3024.dat"
                   , line 28 ( 105, -32, -90 ) upright "3062b.dat"
                   , "0 // Defence Cannons"
                   , line 72 ( -160, -12, -90 ) upright "3004.dat"
                   , line 0 ( -160, -24, -90 ) alongZ "3062b.dat"
                   , line 72 ( 160, -12, -90 ) upright "3004.dat"
                   , line 0 ( 160, -24, -90 ) alongZ "3062b.dat"
                   , "0 // Quayside Cargo"
                   , line 70 ( -30, -24, -90 ) upright "3005.dat"
                   , line 70 ( 30, -24, -90 ) upright "3062b.dat"
                   , "0 // Street Lanterns"
                   , line 0 ( -130, -32, -90 ) upright "3957a.dat"
                   , line 14 ( -130, -128, -90 ) upright "3062b.dat"
                   , line 0 ( 130, -32, -90 ) upright "3957a.dat"
                   , line 14 ( 130, -128, -90 ) upright "3062b.dat"
                   ]
            )
    }


{-| A wooden harbour pier, four studs wide, with a wide cross-dock head,
round brick pilings, stone shore abutment, mooring bollards, cargo crates,
barrels and dock lanterns. Runs from the shore out along -Z.
-}
pier : Piece
pier =
    let
        deckY =
            -56

        post ( x, z ) =
            [ line 72 ( x, -24, z ) upright "3062b.dat"
            , line 72 ( x, -48, z ) upright "3062b.dat"
            ]

        posts =
            [ ( -20, -20 )
            , ( 20, -20 )
            , ( -20, -70 )
            , ( 20, -70 )
            , ( -20, -120 )
            , ( 20, -120 )
            , ( -60, -160 )
            , ( -20, -160 )
            , ( 20, -160 )
            , ( 60, -160 )
            ]
    in
    { name = "pier"
    , ldraw =
        String.join "\n"
            ([ "0 FILE pier.ldr"
             , "0 Name: Brick Sails harbour pier"
             , "0 Author: Brick Sails"
             , "0 // Stone shore threshold"
             , line 71 ( 0, deckY, 0 ) upright "3034.dat"
             , "0 // Main walkway deck (4 studs wide)"
             , line 70 ( -20, deckY, -80 ) alongZ "3034.dat"
             , line 70 ( 20, deckY, -80 ) alongZ "3034.dat"
             , "0 // Cross-dock T-head"
             , line 70 ( -40, deckY, -160 ) upright "3034.dat"
             , line 70 ( 40, deckY, -160 ) upright "3034.dat"
             , "0 // Pilings"
             ]
                ++ List.concatMap post posts
                ++ [ "0 // Mooring bollards"
                   , line 0 ( -30, deckY - 24, -20 ) upright "3062b.dat"
                   , line 0 ( 30, deckY - 24, -20 ) upright "3062b.dat"
                   , line 0 ( -30, deckY - 24, -80 ) upright "3062b.dat"
                   , line 0 ( 30, deckY - 24, -80 ) upright "3062b.dat"
                   , line 0 ( -70, deckY - 24, -160 ) upright "3062b.dat"
                   , line 0 ( 70, deckY - 24, -160 ) upright "3062b.dat"
                   , "0 // Dockside cargo"
                   , line 70 ( -50, deckY - 24, -160 ) upright "3005.dat"
                   , line 70 ( -50, deckY - 48, -160 ) upright "3005.dat"
                   , line 28 ( 50, deckY - 24, -160 ) upright "3062b.dat"
                   , "0 // Dockside cargo derrick crane"
                   , line 70 ( -30, deckY, -140 ) upright "3020.dat"
                   , line 70 ( -30, deckY - 32, -140 ) upright "3957a.dat"
                   , line 70 ( -30, deckY - 72, -130 ) upright "3460.dat"
                   , line 14 ( -30, deckY - 48, -110 ) upright "6141.dat"
                   , line 28 ( -30, deckY - 32, -110 ) upright "3062b.dat"
                   , "0 // Dock lanterns"
                   , line 0 ( -30, deckY - 32, -40 ) upright "3957a.dat"
                   , line 14 ( -30, deckY - 128, -40 ) upright "3062b.dat"
                   , line 0 ( 30, deckY - 32, -120 ) upright "3957a.dat"
                   , line 14 ( 30, deckY - 128, -120 ) upright "3062b.dat"
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
            [ line 71 ( -40, layerY 4, 0 ) upright "3004.dat"
            , line 71 ( 40, layerY 4, 0 ) upright "3004.dat"
            , line 71 ( 0, layerY 4 - 8, 0 ) upright "3034.dat"
            , line 71 ( 0, layerY 4 - 16, 0 ) upright "3020.dat"
            ]

        peaks =
            [ line 72 ( -80, layerY 5, 0 ) upright "3037.dat"
            , line 15 ( -80, layerY 5 - 8, 0 ) upright "3024.dat"
            , line 72 ( 80, layerY 5, 0 ) upright "3037.dat"
            , line 15 ( 80, layerY 5 - 8, 0 ) upright "3024.dat"
            ]
    in
    { name = "seaArch"
    , ldraw =
        String.join "\n"
            ([ "0 FILE seaArch.ldr"
             , "0 Name: Brick Sails coastal sea arch"
             , "0 Author: Brick Sails"
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
            , "0 // Box, band and lid"
            , line 70 ( 0, -24, 0 ) upright "3001.dat"
            , line 14 ( 0, -32, 0 ) upright "3020.dat"
            , line 70 ( 0, -56, 0 ) upright "3001.dat"
            , line 14 ( 0, -40, -30 ) upright "3024.dat"
            , "0 // Gold"
            , line 14 ( -30, -64, -10 ) upright "6141.dat"
            , line 14 ( -10, -64, 10 ) upright "6141.dat"
            , line 14 ( 10, -64, -10 ) upright "6141.dat"
            , line 14 ( 30, -64, 10 ) upright "6141.dat"
            , line 14 ( -10, -72, 10 ) upright "6141.dat"
            , line 14 ( 10, -72, -10 ) upright "6141.dat"
            , line 14 ( 10, -80, -10 ) upright "6141.dat"
            , "0 // Pirate flag"
            , line 0 ( 70, -24, 0 ) upright "3062b.dat"
            , line 0 ( 70, -32, 0 ) upright "3957a.dat"
            , line 0 ( 70, -108, 0 ) upright "2335.dat"
            ]
    }


{-| A palm tree: a leaning trunk of reddish-brown 1 x 1 bricks, coconuts of
dark tan round bricks under the crown, and eight green 2 x 2 slopes drooping
out from a green plate as the fronds.
-}
palmTree : String
palmTree =
    let
        crownX =
            18

        frond colour ( x, y, z ) matrix =
            line colour ( crownX + x, y, z ) matrix "3039.dat"
    in
    String.join "\n"
        [ "0 FILE palm.ldr"
        , "0 Name: Brick Sails palm tree"
        , "0 Author: Brick Sails"
        , "0 // Trunk"
        , line 70 ( 0, -24, 0 ) upright "3005.dat"
        , line 70 ( 0, -48, 0 ) upright "3005.dat"
        , line 70 ( 3, -72, 0 ) upright "3005.dat"
        , line 70 ( 7, -96, 0 ) upright "3005.dat"
        , line 70 ( 12, -120, 0 ) upright "3005.dat"
        , line 70 ( crownX, -144, 0 ) upright "3005.dat"
        , "0 // Coconuts"
        , line 28 ( crownX, -128, -20 ) upright "3062b.dat"
        , line 28 ( crownX + 20, -128, 0 ) upright "3062b.dat"
        , line 28 ( crownX - 20, -128, 0 ) upright "3062b.dat"
        , "0 // Crown and fronds"
        , line 2 ( crownX, -152, 0 ) upright "3022.dat"
        , frond 2 ( 0, -152, -40 ) upright
        , frond 2 ( 0, -152, 40 ) turned
        , frond 2 ( -40, -152, 0 ) alongZ
        , frond 2 ( 40, -152, 0 ) "0 0 -1 0 1 0 1 0 0"
        , frond 10 ( -28, -144, -28 ) "0.7071 0 0.7071 0 1 0 -0.7071 0 0.7071"
        , frond 10 ( -28, -144, 28 ) "-0.7071 0 0.7071 0 1 0 -0.7071 0 -0.7071"
        , frond 10 ( 28, -144, -28 ) "0.7071 0 -0.7071 0 1 0 0.7071 0 0.7071"
        , frond 10 ( 28, -144, 28 ) "-0.7071 0 -0.7071 0 1 0 0.7071 0 -0.7071"
        , line 10 ( crownX, -160, 0 ) upright "3024.dat"
        ]


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
        , line 70 ( 0, -24, 0 ) upright "3062b.dat"
        , line 70 ( 0, -48, 0 ) upright "3062b.dat"
        , line 0 ( 0, -56, 0 ) upright "3957a.dat"
        , line 0 ( 0, -132, 0 ) upright "2335.dat"
        ]


{-| A tiny brick-built seagull: white 1x1 plate body, yellow 1x1 round plate beak, and angled 1x1 wings.
-}
seabird : String
seabird =
    String.join "\n"
        [ "0 FILE seabird.ldr"
        , "0 Name: Brick Sails seabird"
        , "0 Author: Brick Sails"
        , line 15 ( 0, 0, 0 ) upright "3024.dat"
        , line 14 ( 0, 0, -10 ) upright "6141.dat"
        , line 15 ( -10, 0, 0 ) upright "3024.dat"
        , line 15 ( 10, 0, 0 ) upright "3024.dat"
        ]
