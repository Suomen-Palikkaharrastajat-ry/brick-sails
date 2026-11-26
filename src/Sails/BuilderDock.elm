module Sails.BuilderDock exposing (document)

{-| The harbour the ship builder shows around the berth, built from studded
plates: a side pier of reddish brown and tan 2 x 4 plates along +X, a stern
quay of 1 x 8 plates across +Z, dark grey 1 x 8 plates as kerb rails on the
berth edges, and dark grey plate stacks as mooring bollards.

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
               , "0 // Mooring bollards"
               ]
            ++ List.concatMap (\z -> bollard ( 100, z ) "3022.dat") [ -100, -20, 60, 140 ]
            ++ List.concatMap (\x -> bollard ( x, 170 ) "3023.dat") [ -60, 20 ]
        )


{-| Top of the deck plates, 1 unit above the water (a plate is 8 tall).
-}
deckY : Float
deckY =
    3


{-| Top of the plates that stand on the deck.
-}
railY : Float
railY =
    deckY - 8


sideDeckPlate : Int -> String
sideDeckPlate k =
    line
        (if modBy 2 k == 0 then
            70

         else
            19
        )
        ( 100, deckY, -120 + 40 * toFloat k )
        upright
        "3020.dat"


bollard : ( Float, Float ) -> String -> List String
bollard ( x, z ) part =
    [ line 72 ( x, railY, z ) upright part
    , line 72 ( x, railY - 8, z ) upright part
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
