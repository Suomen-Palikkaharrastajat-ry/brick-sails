module Sails.BuilderDock exposing (document)

{-| The harbour the ship builder shows around the berth, built from studded
plates and authentic LEGO scenery:

  - A side pier of reddish brown (70), dark tan (28) and tan (19) 2 x 4 plates along +X.
  - A stern quay of 1 x 8 and 2 x 8 plates across +Z.
  - Dark stone grey (72) 1 x 8 kerb plates lining the berth edges.
  - Round cylindrical mooring bollards (1x1 round brick topped with a round tile)
    in black (0) along the berth kerb.
  - Solid wooden pilings (1x1 round bricks in reddish brown 70) extending from under
    the deck down into the water.
  - Harbor cargo (a wooden barrel) on the pier corner.

It is fixed scenery, not part of the ship: it is never in the build's
document, its share link or its part count, and the sailing game never sees
it. `Pages.Build` fetches its parts and `Sails.DockFloor` draws it.

Positions are in dock units, as in `Sails.DockFloor`: x across the berth, y
down (the water is at y = 12) and z along the ship, the stern quay at +z.
The open berth is x < 60, z < 140; the side pier covers x 60..140,
z -140..180 and the stern quay x -100..60, z 140..180. Each line stores -z,
because `Sails.ShipMesh.buildScenery` turns LDraw space Y-up by negating
both y and z.

-}

import LDraw.Document as Document exposing (Document)


{-| The harbour as an LDraw document.
-}
document : Document
document =
    Document.parse ldraw


ldraw : String
ldraw =
    String.join "\n"
        ([ "0 FILE builderDock.ldr"
         , "0 Name: Brick Sails builder harbour"
         , "0 Author: Brick Sails"
         , "0 // Side pier deck"
         ]
            ++ List.map sideDeckPlate (List.range 0 7)
            ++ [ "0 // Stern quay deck"
               , line 19 ( -20, deckY, 150 ) upright "3460.dat"
               , line 70 ( -20, deckY, 170 ) upright "3460.dat"
               , "0 // Kerb rails on the berth edges"
               , line 72 ( 70, railY, -60 ) alongZ "3460.dat"
               , line 72 ( 70, railY, 100 ) alongZ "3460.dat"
               , line 72 ( -20, railY, 150 ) upright "3460.dat"
               , "0 // Marine mooring bollards"
               ]
            ++ List.concatMap bollardOnKerb [ -110, -10, 70, 130 ]
            ++ List.concatMap bollardOnSternKerb [ -70, 10 ]
            ++ [ "0 // Under-deck wooden pilings extending into the water" ]
            ++ List.map (\z -> line 70 ( 70, pilingY, z ) upright "3062b.dat") [ -110, -70, -30, 10, 50, 90, 130, 170 ]
            ++ List.map (\z -> line 70 ( 70, pilingY + 24, z ) upright "3062b.dat") [ -110, -70, -30, 10, 50, 90, 130, 170 ]
            ++ List.map (\z -> line 70 ( 130, pilingY, z ) upright "3062b.dat") [ -110, -70, -30, 10, 50, 90, 130, 170 ]
            ++ List.map (\z -> line 70 ( 130, pilingY + 24, z ) upright "3062b.dat") [ -110, -70, -30, 10, 50, 90, 130, 170 ]
            ++ List.map (\x -> line 70 ( x, pilingY, 150 ) upright "3062b.dat") [ -90, -50, -10, 30 ]
            ++ List.map (\x -> line 70 ( x, pilingY + 24, 150 ) upright "3062b.dat") [ -90, -50, -10, 30 ]
            ++ List.map (\x -> line 70 ( x, pilingY, 170 ) upright "3062b.dat") [ -90, -50, -10, 30 ]
            ++ List.map (\x -> line 70 ( x, pilingY + 24, 170 ) upright "3062b.dat") [ -90, -50, -10, 30 ]
            ++ [ "0 // Harbor cargo: wooden barrel and shipping crate on pier"
               , line 70 ( 120, barrelY, 160 ) upright "2489.dat"
               ]
            ++ crate 120 120
        )


{-| Top of the deck plates, 1 unit above the water (a plate is 8 tall, body sits y=3..11, water at y=12).
-}
deckY : Float
deckY =
    3


{-| Top of the kerb plates that stand on the deck.
-}
railY : Float
railY =
    deckY - 8


{-| Top of the under-deck piling bricks (pegs into plate anti-studs at y=11, body extends down to y=35 into water).
-}
pilingY : Float
pilingY =
    deckY + 8


{-| Origin of the 2x2x2 barrel sitting on the deck (barrel body is 40 LDU tall, anti-studs at base).
-}
barrelY : Float
barrelY =
    deckY - 40


{-| Varied natural wooden decking plank colors: reddish brown (70), dark tan (28), and tan (19).
-}
sideDeckPlate : Int -> String
sideDeckPlate k =
    let
        colour =
            case modBy 4 k of
                0 ->
                    70

                1 ->
                    28

                2 ->
                    70

                _ ->
                    19
    in
    line colour ( 100, deckY, -120 + 40 * toFloat k ) upright "3020.dat"


{-| Cylindrical mooring bollard on the side kerb rail:
1x1 round brick (3062b.dat, height 24) topped with 1x1 round tile (98138.dat, height 8) in cast-iron black (0).
-}
bollardOnKerb : Float -> List String
bollardOnKerb z =
    let
        brickY =
            railY - 24

        tileY =
            brickY - 8
    in
    [ line 0 ( 70, brickY, z ) upright "3062b.dat"
    , line 0 ( 70, tileY, z ) upright "98138.dat"
    ]


{-| Cylindrical mooring bollard on the stern kerb rail.
-}
bollardOnSternKerb : Float -> List String
bollardOnSternKerb x =
    let
        brickY =
            railY - 24

        tileY =
            brickY - 8
    in
    [ line 0 ( x, brickY, 150 ) upright "3062b.dat"
    , line 0 ( x, tileY, 150 ) upright "98138.dat"
    ]


{-| Wooden cargo crate on the pier: 2x2 brick (3003.dat) in dark tan (28) topped with 2x2 tile (3068b.dat) in tan (19).
-}
crate : Float -> Float -> List String
crate x z =
    let
        crateY =
            deckY - 24

        lidY =
            crateY - 8
    in
    [ line 28 ( x, crateY, z ) upright "3003.dat"
    , line 19 ( x, lidY, z ) upright "3068b.dat"
    ]


upright : String
upright =
    "1 0 0 0 1 0 0 0 1"


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
        , String.fromFloat (negate z)
        , matrix
        , part
        ]
