module Sails.Scenery exposing
    ( Piece
    , all
    , chest
    , houses
    , isScenery
    , key
    , metersPerLdu
    , pier
    )

{-| The brick-built scenery on the islands: a row of houses, a harbour pier
and the pirates' treasure chest.

Like the boats in `Sails.Boats`, every piece is a real brick model: the
LDraw text lists the catalog bricks, plates, slopes and round bricks it is
built from, `Main` fetches the parts through the part resolver and
`Sails.ShipMesh.buildScenery` turns them into geometry. Each model stands
on LDraw y = 0 (the ground) with its front towards LDraw -Z, which faces
out from the island when drawn.

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
    [ houses, pier, chest ]


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


{-| Three houses side by side behind a cobbled street.
-}
houses : Piece
houses =
    { name = "houses"
    , ldraw =
        String.join "\n"
            ([ "0 FILE houses.ldr"
             , "0 Name: Brick Sails island houses"
             , "0 Author: Brick Sails"
             , "0 // Street"
             , line 71 ( -80, -8, -70 ) upright "3034.dat"
             , line 71 ( 80, -8, -70 ) upright "3034.dat"
             , "0 // Houses"
             ]
                ++ house -120 3 15 4
                ++ house 0 4 14 72
                ++ house 120 3 19 70
            )
    }


{-| A wooden pier, two studs wide and eight long, on round brick posts,
with bollards and a lamp at the end. Runs from the shore out along -Z.
-}
pier : Piece
pier =
    let
        deckY =
            -56

        post z =
            [ line 72 ( -10, -24, z ) upright "3062b.dat"
            , line 72 ( -10, -48, z ) upright "3062b.dat"
            , line 72 ( 10, -24, z ) upright "3062b.dat"
            , line 72 ( 10, -48, z ) upright "3062b.dat"
            ]

        plank z =
            [ line 70 ( -10, deckY, z ) alongZ "3460.dat"
            , line 70 ( 10, deckY, z ) alongZ "3460.dat"
            ]
    in
    { name = "pier"
    , ldraw =
        String.join "\n"
            ([ "0 FILE pier.ldr"
             , "0 Name: Brick Sails harbour pier"
             , "0 Author: Brick Sails"
             , "0 // Deck"
             ]
                ++ plank -80
                ++ [ "0 // Posts" ]
                ++ List.concatMap post [ -10, -80, -150 ]
                ++ [ "0 // Bollards and lamp"
                   , line 0 ( -10, deckY - 24, -150 ) upright "3062b.dat"
                   , line 0 ( 10, deckY - 24, -150 ) upright "3062b.dat"
                   , line 0 ( 10, deckY - 32, -90 ) upright "3957a.dat"
                   , line 14 ( 10, deckY - 128, -90 ) upright "3062b.dat"
                   ]
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
