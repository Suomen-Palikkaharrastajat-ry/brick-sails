module Sails.IslandDecor exposing
    ( Look, lookFor
    , TreeKind(..), treeKinds, skerryKinds, treeProp, treeTint
    , HouseStyle(..), houseStyles, houseProp, houseHalfSize, hallProp, hallHalfSize
    , propSources, lighthouseLamp
    , Shape(..), proxyOf
    )

{-| The brick models that dress the islands, at the one real brick scale of
the islands (`Sails.Scenery.metersPerLdu`): seven kinds of tree, undergrowth,
flowers, barrels, street lamps, path tiles, six kinds of house and the town
hall, a fence, a cairn and a tall lighthouse.

Every model is LDraw text built from catalog parts (only parts present in
the catalog's `raw/parts`), standing on LDraw y = 0 with its front towards
LDraw -Z, like `Game.BrickModels`; `propSources` registers them with
`Game.BrickProps` under their names (`treeProp`, `houseProp`, ...). The parts
that change with the season are in colour 16, so one model serves every
season and the draw call paints them (`treeTint`, the house walls): leaves
green, bright, autumn orange, yellow and dark red, or snow white. Roofs and
pine tops have a snowy model of their own for winter. The palm, the leafy
tree and the pine are `Game.BrickModels`' shared models.

`proxyOf` gives each model a few boxes and roof prisms of its size and
colours: `Sails.IslandMesh` merges them into one mesh per island, drawn in
place of the real bricks on islands far from the camera.

@docs Look, lookFor
@docs TreeKind, treeKinds, skerryKinds, treeProp, treeTint
@docs HouseStyle, houseStyles, houseProp, houseHalfSize, hallProp, hallHalfSize
@docs propSources, lighthouseLamp
@docs Shape, proxyOf

-}

import Game.BrickModels as BrickModels
import Game.MapParams exposing (Biome(..), MapParams, Season(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import Sails.Scenery as Scenery



-- BRICKS


{-| One catalog part: its file, its LDraw colour and where it goes (an LDraw
transform in LDraw units).
-}
type alias Brick =
    { part : String
    , colour : Int
    , at : Mat4
    }


yAxis : Vec3
yAxis =
    vec3 0 1 0


{-| A part at a point, unturned.
-}
put : Int -> ( Float, Float, Float ) -> String -> Brick
put colour ( x, y, z ) part =
    { part = part, colour = colour, at = Mat4.makeTranslate3 x y z }


{-| A part at a point, turned `deg` degrees about the LDraw vertical (90 is
`Game.BrickModels.Quarter`: its long X side along Z).
-}
turn : Float -> Int -> ( Float, Float, Float ) -> String -> Brick
turn deg colour ( x, y, z ) part =
    { part = part, colour = colour, at = Mat4.makeTranslate3 x y z |> Mat4.rotate (degrees deg) yAxis }


{-| Move a whole model by an offset.
-}
shift : ( Float, Float, Float ) -> List Brick -> List Brick
shift ( x, y, z ) bricks =
    let
        m =
            Mat4.makeTranslate3 x y z
    in
    List.map (\b -> { b | at = Mat4.mul m b.at }) bricks


recolour : (Int -> Int) -> List Brick -> List Brick
recolour f bricks =
    List.map (\b -> { b | colour = f b.colour }) bricks


{-| The parts of an LDraw model's type-1 lines (comments and other lines are
skipped).
-}
fromLDraw : String -> List Brick
fromLDraw text =
    String.lines text |> List.filterMap parseLine


parseLine : String -> Maybe Brick
parseLine line =
    case String.words line of
        "1" :: c :: rest ->
            let
                nums =
                    List.filterMap String.toFloat (List.take 12 rest)

                file =
                    String.join " " (List.drop 12 rest)
            in
            case ( String.toInt c, nums ) of
                ( Just colour, [ x, y, z, a, b, cc, d, e, f, g, h, i ] ) ->
                    if String.isEmpty file then
                        Nothing

                    else
                        Just
                            { part = file
                            , colour = colour
                            , at =
                                Mat4.fromRecord
                                    { m11 = a
                                    , m12 = b
                                    , m13 = cc
                                    , m14 = x
                                    , m21 = d
                                    , m22 = e
                                    , m23 = f
                                    , m24 = y
                                    , m31 = g
                                    , m32 = h
                                    , m33 = i
                                    , m34 = z
                                    , m41 = 0
                                    , m42 = 0
                                    , m43 = 0
                                    , m44 = 1
                                    }
                            }

                _ ->
                    Nothing

        _ ->
            Nothing


{-| The LDraw type-1 line of a brick, its numbers rounded to 0.001 LDU.
-}
toLine : Brick -> String
toLine b =
    let
        m =
            Mat4.toRecord b.at
    in
    String.join " "
        ("1"
            :: String.fromInt b.colour
            :: List.map num [ m.m14, m.m24, m.m34, m.m11, m.m12, m.m13, m.m21, m.m22, m.m23, m.m31, m.m32, m.m33 ]
            ++ [ b.part ]
        )


num : Float -> String
num v =
    let
        r =
            toFloat (round (v * 1000)) / 1000
    in
    if r == 0 then
        "0"

    else
        String.fromFloat r


toLDraw : String -> List Brick -> String
toLDraw name bricks =
    String.join "\n"
        ([ "0 FILE " ++ String.replace ":" "-" name ++ ".ldr"
         , "0 Name: Brick Sails island " ++ name
         , "0 Author: Brick Sails"
         , "0 BFC CERTIFY CCW"
         ]
            ++ List.map toLine bricks
        )



-- LOOK


{-| The colours of a map's islands: the leaf colours of the season (green in
summer, bright and blossom pink in spring, orange, yellow and dark red in
autumn, white with snow in winter), the birch leaves, whether snow lies on
the roofs and pines, the house walls of the biome, whether flowers bloom, the
bushes, the palm fronds and the path tiles.
-}
type alias Look =
    { leaves : List Int
    , birch : Int
    , snow : Bool
    , walls : List Int
    , flowers : Bool
    , bushes : Int
    , fronds : Int
    , path : Int
    }


{-| The look of a map.
-}
lookFor : MapParams -> Look
lookFor map =
    let
        walls =
            case map.biome of
                Desert ->
                    [ 15, 19, 226, 191, 28 ]

                Boreal ->
                    [ 320, 15, 19, 379, 28 ]

                Farmland ->
                    [ 15, 226, 18, 77, 19 ]

                Fell ->
                    [ 71, 15, 19, 378, 28 ]

        path =
            case map.biome of
                Farmland ->
                    28

                Desert ->
                    28

                _ ->
                    72
    in
    case map.season of
        Spring ->
            { leaves = [ 10, 2, 10, 13 ], birch = 10, snow = False, walls = walls, flowers = True, bushes = 10, fronds = 2, path = path }

        Summer ->
            { leaves = [ 2, 2, 288, 10 ], birch = 2, snow = False, walls = walls, flowers = True, bushes = 2, fronds = 2, path = path }

        Autumn ->
            { leaves = [ 484, 14, 320, 191, 2 ], birch = 14, snow = False, walls = walls, flowers = False, bushes = 484, fronds = 2, path = path }

        Winter ->
            { leaves = [ 15, 15, 71 ], birch = 15, snow = True, walls = walls, flowers = False, bushes = 15, fronds = 288, path = path }


pickFrom : Float -> Int -> List Int -> Int
pickFrom pick fallback colours =
    let
        n =
            List.length colours
    in
    List.drop (min (n - 1) (floor (pick * toFloat n))) colours
        |> List.head
        |> Maybe.withDefault fallback



-- TREES


{-| The trees of the islands: short and tall palms (`Game.BrickModels`'
palm), the leafy tree, the oval tree (3470) on a round 2 x 2 trunk, the
birch (3470) on a white trunk, the pine (3471) and the tall pine on a round
trunk.
-}
type TreeKind
    = Palm
    | TallPalm
    | Leafy
    | Oval
    | Birch
    | Pine
    | TallPine


allTreeKinds : List TreeKind
allTreeKinds =
    [ Palm, TallPalm, Leafy, Oval, Birch, Pine, TallPine ]


{-| The trees of a biome with their weights: at least two kinds each.
-}
treeKinds : Biome -> List ( Float, TreeKind )
treeKinds biome =
    case biome of
        Desert ->
            [ ( 4, Palm ), ( 4, TallPalm ), ( 1, Oval ) ]

        Boreal ->
            [ ( 4, Pine ), ( 3, TallPine ), ( 2, Birch ) ]

        Farmland ->
            [ ( 4, Oval ), ( 3, Leafy ), ( 1, Birch ), ( 1, Pine ) ]

        Fell ->
            [ ( 3, Pine ), ( 2, Birch ), ( 1, TallPine ) ]


{-| The one or two hardy trees a skerry may carry.
-}
skerryKinds : Biome -> List ( Float, TreeKind )
skerryKinds biome =
    case biome of
        Desert ->
            [ ( 1, Palm ) ]

        _ ->
            [ ( 1, Pine ) ]


treeKey : TreeKind -> String
treeKey kind =
    case kind of
        Palm ->
            "palm"

        TallPalm ->
            "tallPalm"

        Leafy ->
            "leafy"

        Oval ->
            "oval"

        Birch ->
            "birch"

        Pine ->
            "pine"

        TallPine ->
            "tallPine"


hasSnowModel : TreeKind -> Bool
hasSnowModel kind =
    kind == Pine || kind == TallPine


{-| The prop name of a tree (the pines have a snow-capped model for winter).
-}
treeProp : Bool -> TreeKind -> String
treeProp snow kind =
    "dec:tree:"
        ++ treeKey kind
        ++ (if snow && hasSnowModel kind then
                ":snow"

            else
                ""
           )


{-| The colour a tree's colour-16 parts are drawn in: its leaves (picked by
`pick`, 0 to 1, from the season's), the birch leaves or the palm fronds.
-}
treeTint : Look -> Float -> TreeKind -> Int
treeTint look pick kind =
    case kind of
        Palm ->
            look.fronds

        TallPalm ->
            look.fronds

        Birch ->
            look.birch

        _ ->
            pickFrom pick 2 look.leaves


palmBricks : List Brick
palmBricks =
    fromLDraw BrickModels.palmTree
        |> recolour
            (\c ->
                if c == 2 || c == 10 then
                    16

                else
                    c
            )


treeBricks : Bool -> TreeKind -> List Brick
treeBricks snow kind =
    let
        cap =
            if snow then
                Just 15

            else
                Nothing

        pine =
            fromLDraw (BrickModels.pineTreeIn { needles = 288, cap = cap })
    in
    case kind of
        Palm ->
            palmBricks

        TallPalm ->
            put 70 ( 0, -24, 0 ) "3005.dat"
                :: put 70 ( 0, -48, 0 ) "3005.dat"
                :: shift ( 0, -48, 0 ) palmBricks

        Leafy ->
            fromLDraw (BrickModels.leafyTreeIn { leaves = 16 })

        Oval ->
            [ put 70 ( 0, -24, 0 ) "3941.dat"
            , put 16 ( 0, -32, 0 ) "3470.dat"
            ]

        Birch ->
            [ put 15 ( 0, -24, 0 ) "3062b.dat"
            , put 15 ( 0, -48, 0 ) "3062b.dat"
            , put 16 ( 0, -56, 0 ) "3470.dat"
            ]

        Pine ->
            pine

        TallPine ->
            put 70 ( 0, -24, 0 ) "3941.dat" :: shift ( 0, -24, 0 ) pine



-- SMALL PROPS


{-| Undergrowth: a 6 x 5 and a 4 x 3 plant leaves crossed, with a round
plate of three leaves on top, all painted the season's bush colour.
-}
bush : List Brick
bush =
    [ put 16 ( 0, -8, 0 ) "2417.dat"
    , turn 90 16 ( 0, -16, 0 ) "2423.dat"
    , put 16 ( 0, -24, 0 ) "32607.dat"
    ]


{-| A patch of red, yellow and white flowers (3742) on a green 2 x 2 plate.
-}
flowers : List Brick
flowers =
    [ put 2 ( 0, -8, 0 ) "3022.dat"
    , put 4 ( -10, -10, -10 ) "3742.dat"
    , put 14 ( 10, -10, 10 ) "3742.dat"
    , put 15 ( 10, -10, -10 ) "3742.dat"
    ]


{-| Two reddish-brown 2 x 2 barrels (2489) side by side and one on top.
-}
barrels : List Brick
barrels =
    [ put 70 ( -20, -40, 0 ) "2489.dat"
    , put 70 ( 20, -40, 0 ) "2489.dat"
    , put 70 ( 0, -80, 0 ) "2489.dat"
    ]


{-| A street lamp: a black post of round 1 x 1 bricks with a yellow headlight
brick (4070) and a black cone on top.
-}
lampPost : List Brick
lampPost =
    [ put 0 ( 0, -24, 0 ) "3062b.dat"
    , put 0 ( 0, -48, 0 ) "3062b.dat"
    , put 0 ( 0, -72, 0 ) "3062b.dat"
    , put 14 ( 0, -96, 0 ) "4070.dat"
    , put 0 ( 0, -120, 0 ) "4589.dat"
    ]


{-| A 2 x 4 tile of the town paths, along X, in the path colour.
-}
pathTile : List Brick
pathTile =
    [ put 16 ( 0, -8, 0 ) "87079.dat" ]


{-| A white fence lattice (3633) 4 studs long between reddish-brown posts.
-}
fence : List Brick
fence =
    [ put 15 ( 0, -24, 0 ) "3633.dat"
    , put 70 ( -50, -24, 0 ) "3005.dat"
    , put 70 ( 50, -24, 0 ) "3005.dat"
    ]



-- HOUSES


{-| The houses of the towns:

  - `Cottage`: 4 x 4, one floor, a door and a window, a 45 degree gable;
  - `TownHouse`: 4 x 4, two floors of windows and a chimney;
  - `LongHouse`: 6 x 4, window, door, window, a gable of 2 x 3 slopes;
  - `Warehouse`: 6 x 6 with an arched gate (6182) and a 33 degree roof;
  - `Tavern`: 6 x 4 of log bricks (30136) with tall windows (60593);
  - `StoneHouse`: 4 x 4 of embossed bricks (98283) under a hipped roof of
    convex slopes (3045).

Every house has a strip of path tiles 3.15 m in front of its door, so a row
of houses lays its own path.

-}
type HouseStyle
    = Cottage
    | TownHouse
    | LongHouse
    | Warehouse
    | Tavern
    | StoneHouse


{-| Every house style.
-}
houseStyles : List HouseStyle
houseStyles =
    [ Cottage, TownHouse, LongHouse, Warehouse, Tavern, StoneHouse ]


styleKey : HouseStyle -> String
styleKey style =
    case style of
        Cottage ->
            "cottage"

        TownHouse ->
            "townHouse"

        LongHouse ->
            "longHouse"

        Warehouse ->
            "warehouse"

        Tavern ->
            "tavern"

        StoneHouse ->
            "stoneHouse"


{-| The prop name of a house (with snow on the roof in winter).
-}
houseProp : Bool -> HouseStyle -> String
houseProp snow style =
    "dec:house:" ++ styleKey style ++ snowSuffix snow


snowSuffix : Bool -> String
snowSuffix snow =
    if snow then
        ":snow"

    else
        ""


{-| The prop name of the town hall.
-}
hallProp : Bool -> String
hallProp snow =
    "dec:hall" ++ snowSuffix snow


{-| Half the width (X) and half the depth (Z) of a house's walls, in metres.
-}
houseHalfSize : HouseStyle -> ( Float, Float )
houseHalfSize style =
    let
        ( w, d ) =
            footprint style
    in
    ( toFloat w * 10 * Scenery.metersPerLdu, toFloat d * 10 * Scenery.metersPerLdu )


footprint : HouseStyle -> ( Int, Int )
footprint style =
    case style of
        Cottage ->
            ( 4, 4 )

        TownHouse ->
            ( 4, 4 )

        LongHouse ->
            ( 6, 4 )

        Warehouse ->
            ( 6, 6 )

        Tavern ->
            ( 6, 4 )

        StoneHouse ->
            ( 4, 4 )


{-| Wall layers and roof colour of a house style.
-}
layersOf : HouseStyle -> Int
layersOf style =
    case style of
        TownHouse ->
            5

        Warehouse ->
            4

        _ ->
            3


roofOf : Bool -> HouseStyle -> Int
roofOf snow style =
    if snow then
        15

    else
        case style of
            Cottage ->
                4

            TownHouse ->
                320

            LongHouse ->
                72

            Warehouse ->
                70

            Tavern ->
                28

            StoneHouse ->
                72


{-| An opening in the front or back wall: its first stud from the left, how
many studs wide, and its parts (placed with the opening's top-left at the
origin of the part).
-}
type alias Opening =
    { from : Int
    , width : Int
    , parts : Float -> Float -> Float -> List Brick
    }


window2 : Int -> Opening
window2 from =
    { from = from, width = 2, parts = \x y z -> [ put 15 ( x, y, z ) "60592.dat", put 47 ( x, y, z ) "60601.dat" ] }


window3 : Int -> Opening
window3 from =
    { from = from, width = 2, parts = \x y z -> [ put 15 ( x, y, z ) "60593.dat", put 47 ( x, y, z ) "60602.dat" ] }


door : Int -> Opening
door from =
    { from = from, width = 2, parts = \x y z -> [ put 70 ( x, y, z ) "87552.dat" ] }


gate : Int -> Opening
gate from =
    { from = from, width = 4, parts = \x y z -> [ put 15 ( x, y, z ) "6182.dat" ] }


{-| The plain parts a wall is laid in.
-}
type WallKind
    = Plain
    | Logs
    | Stone


wallPart : WallKind -> Int -> Maybe String
wallPart kind n =
    case ( kind, n ) of
        ( _, 1 ) ->
            Just "3005.dat"

        ( Plain, 2 ) ->
            Just "3004.dat"

        ( Plain, 3 ) ->
            Just "3622.dat"

        ( Plain, 4 ) ->
            Just "3010.dat"

        ( Plain, 6 ) ->
            Just "3009.dat"

        ( Logs, 2 ) ->
            Just "30136.dat"

        ( Stone, 2 ) ->
            Just "98283.dat"

        _ ->
            Nothing


{-| Lay a run of `n` studs of wall in the biggest parts the kind has, along
X (`alongX`) or along Z, starting at stud `start` of a line whose first stud
centre is at `origin`.
-}
run : WallKind -> Int -> Bool -> ( Float, Float, Float ) -> Int -> Int -> List Brick
run kind colour alongX ( ox, y, oz ) start n =
    let
        biggest left =
            [ 6, 4, 3, 2, 1 ]
                |> List.filter (\k -> k <= left && wallPart kind k /= Nothing)
                |> List.head
                |> Maybe.withDefault 1

        go s left acc =
            if left <= 0 then
                acc

            else
                let
                    k =
                        biggest left

                    centre =
                        (toFloat s + toFloat k / 2) * 20 - 10

                    file =
                        Maybe.withDefault "3005.dat" (wallPart kind k)
                in
                go (s + k)
                    (left - k)
                    ((if alongX then
                        put colour ( ox + centre, y, oz ) file

                      else
                        turn 90 colour ( ox, y, oz + centre ) file
                     )
                        :: acc
                    )
    in
    go start n []


{-| One wall along X, `w` studs from x = -10w, at depth `z`, from layer
`from` for `layers` layers, with openings (each as tall as the wall run).
-}
xWall : WallKind -> Int -> Int -> Float -> Int -> Int -> List Opening -> List Brick
xWall kind colour w z from layers openings =
    let
        x0 =
            -10 * toFloat w + 10

        layerY k =
            -24 * toFloat (k + 1)

        gaps =
            List.foldl
                (\o ( s, acc ) -> ( o.from + o.width, acc ++ [ ( s, o.from - s ) ] ))
                ( 0, [] )
                (List.sortBy .from openings)
                |> (\( s, acc ) -> acc ++ [ ( s, w - s ) ])

        bricks =
            List.range from (from + layers - 1)
                |> List.concatMap
                    (\k -> List.concatMap (\( s, n ) -> run kind colour True ( x0, layerY k, z ) s n) gaps)

        holes =
            List.concatMap
                (\o -> o.parts (x0 + (toFloat o.from + toFloat o.width / 2) * 20 - 10) (-24 * toFloat (from + layers)) z)
                openings
    in
    bricks ++ holes


{-| The side walls of a `w` x `d` house between its front and back rows.
-}
sideWalls : WallKind -> Int -> Int -> Int -> List Brick
sideWalls kind w d layers =
    let
        sideX =
            10 * toFloat w - 10
    in
    List.range 0 (layers - 1)
        |> List.concatMap
            (\k ->
                run kind 16 False ( -sideX, -24 * toFloat (k + 1), -10 * toFloat d + 10 ) 1 (d - 2)
                    ++ run kind 16 False ( sideX, -24 * toFloat (k + 1), -10 * toFloat d + 10 ) 1 (d - 2)
            )


{-| The tiles of path 90 LDU (3.15 m) in front of a `w`-stud front.
-}
frontage : Int -> List Brick
frontage w =
    let
        tile x file =
            put 16 ( x, -8, -90 ) file
    in
    case w of
        8 ->
            [ tile -40 "87079.dat", tile 40 "87079.dat" ]

        6 ->
            [ tile -20 "87079.dat", tile 60 "3068b.dat" ]

        _ ->
            [ tile 0 "87079.dat" ]


{-| A house of a style: walls in colour 16 (painted per house), the roof in
the style's colour or snow, its door facing -Z and its path in front.
-}
house : Bool -> HouseStyle -> List Brick
house snow style =
    let
        ( w, d ) =
            footprint style

        front =
            -10 * toFloat d + 10

        back =
            10 * toFloat d - 10

        roof =
            roofOf snow style

        layers =
            layersOf style

        wall =
            16
    in
    frontage w
        ++ (case style of
                Cottage ->
                    xWall Plain wall w front 0 2 [ door 0, window2 2 ]
                        ++ xWall Plain wall w front 2 1 []
                        ++ xWall Plain wall w back 0 2 [ window2 1 ]
                        ++ xWall Plain wall w back 2 1 []
                        ++ sideWalls Plain w d layers
                        ++ gable45 w layers roof

                TownHouse ->
                    xWall Plain wall w front 0 2 [ door 0, window2 2 ]
                        ++ xWall Plain wall w front 2 1 []
                        ++ xWall Plain wall w front 3 2 [ window2 0, window2 2 ]
                        ++ xWall Plain wall w back 0 2 [ window2 1 ]
                        ++ xWall Plain wall w back 2 1 []
                        ++ xWall Plain wall w back 3 2 [ window2 1 ]
                        ++ sideWalls Plain w d layers
                        ++ gable45 w layers roof
                        ++ [ put 72 ( 20, -24 * 7, 10 ) "3005.dat", put 72 ( 20, -24 * 8, 10 ) "3005.dat" ]

                LongHouse ->
                    xWall Plain wall w front 0 2 [ window2 0, door 2, window2 4 ]
                        ++ xWall Plain wall w front 2 1 []
                        ++ xWall Plain wall w back 0 2 [ window2 0, window2 4 ]
                        ++ xWall Plain wall w back 2 1 []
                        ++ sideWalls Plain w d layers
                        ++ gable45 w layers roof

                Warehouse ->
                    xWall Plain wall w front 0 2 [ gate 1 ]
                        ++ xWall Plain wall w front 2 2 [ window2 2 ]
                        ++ xWall Plain wall w back 0 2 []
                        ++ xWall Plain wall w back 2 2 [ window2 2 ]
                        ++ sideWalls Plain w d layers
                        ++ gable33 w layers roof

                Tavern ->
                    xWall Logs wall w front 0 3 [ window3 0, door 2, window3 4 ]
                        ++ xWall Logs wall w back 0 3 [ window3 2 ]
                        ++ sideWalls Logs w d layers
                        ++ gable45 w layers roof
                        ++ shift ( 100, 0, -50 ) barrels
                        ++ shift ( -80, 0, -50 ) lampPost

                StoneHouse ->
                    xWall Stone wall w front 0 2 [ door 0, window2 2 ]
                        ++ xWall Stone wall w front 2 1 []
                        ++ xWall Stone wall w back 0 3 []
                        ++ sideWalls Stone w d layers
                        ++ hipped layers roof
           )


{-| A gable of 45 degree 2-deep slopes over a `w`-wide, 4-deep house whose
walls are `layers` high: 4- or 3-wide slopes along X, back to back.
-}
gable45 : Int -> Int -> Int -> List Brick
gable45 w layers colour =
    let
        y =
            -24 * toFloat (layers + 1)

        pieces =
            case w of
                6 ->
                    [ ( -30, "3038.dat" ), ( 30, "3038.dat" ) ]

                _ ->
                    [ ( 0, "3037.dat" ) ]
    in
    List.concatMap
        (\( x, file ) ->
            [ put colour ( x, y, -10 ) file
            , turn 180 colour ( x, y, 10 ) file
            ]
        )
        pieces


{-| A 33 degree gable of 3 x 2 slopes over a `w`-wide, 6-deep house.
-}
gable33 : Int -> Int -> Int -> List Brick
gable33 w layers colour =
    let
        y =
            -24 * toFloat (layers + 1)
    in
    List.range 0 (w // 2 - 1)
        |> List.concatMap
            (\k ->
                let
                    x =
                        -10 * toFloat w + 20 + 40 * toFloat k
                in
                [ put colour ( x, y, -10 ) "3298.dat"
                , turn 180 colour ( x, y, 10 ) "3298.dat"
                ]
            )


{-| A hipped roof over a 4 x 4 house: four convex 2 x 2 slopes (3045) round
the middle, with a round 1 x 1 chimney on top.
-}
hipped : Int -> Int -> List Brick
hipped layers colour =
    let
        y =
            -24 * toFloat (layers + 1)
    in
    List.map
        (\k -> { part = "3045.dat", colour = colour, at = Mat4.makeTranslate3 0 y 0 |> Mat4.rotate (degrees (90 * toFloat k)) yAxis |> Mat4.translate3 10 0 -10 })
        [ 0, 1, 2, 3 ]
        ++ [ put 72 ( 10, y - 24, -10 ) "3062b.dat" ]


{-| The town hall: 8 x 6 in white, two white arches (6182) at the front under
a row of four windows, a flat dark grey roof plate (3036) with a fence
lattice balustrade, and a belfry of white 2 x 2 bricks under a cone with a
red flag; a double strip of path in front.
-}
townHall : Bool -> List Brick
townHall snow =
    let
        w =
            8

        d =
            6

        wall =
            15

        roof =
            if snow then
                15

            else
                72

        roofY =
            -24 * 5

        sides =
            List.range 0 4
                |> List.concatMap
                    (\k ->
                        run Plain wall False ( -70, -24 * toFloat (k + 1), -50 ) 1 (d - 2)
                            ++ run Plain wall False ( 70, -24 * toFloat (k + 1), -50 ) 1 (d - 2)
                    )
    in
    frontage w
        ++ xWall Plain wall w -50 0 2 [ gate 0, gate 4 ]
        ++ xWall Plain wall w -50 2 2 [ window2 0, window2 2, window2 4, window2 6 ]
        ++ xWall Plain wall w -50 4 1 []
        ++ xWall Plain wall w 50 0 2 [ window2 1, window2 5 ]
        ++ xWall Plain wall w 50 2 3 []
        ++ sides
        ++ [ put roof ( 0, roofY - 8, 0 ) "3036.dat"
           , put 15 ( -40, roofY - 32, -50 ) "3633.dat"
           , put 15 ( 40, roofY - 32, -50 ) "3633.dat"
           , put 15 ( 0, roofY - 32, 10 ) "3003.dat"
           , put 15 ( 0, roofY - 56, 10 ) "3003.dat"
           , put 0 ( 0, roofY - 64, 10 ) "3022.dat"
           , put 15 ( 0, roofY - 88, 10 ) "3003.dat"
           , put roof ( 0, roofY - 136, 10 ) "3942c.dat"
           , put 0 ( 0, roofY - 136, 10 ) "3957a.dat"
           , turn 90 4 ( 0, roofY - 220, 10 ) "2335.dat"
           ]


{-| Half the width and depth of the town hall's walls, in metres.
-}
hallHalfSize : ( Float, Float )
hallHalfSize =
    ( 80 * Scenery.metersPerLdu, 60 * Scenery.metersPerLdu )



-- LANDMARKS


{-| A lighthouse 10 round 2 x 2 bricks tall in white and red bands of two,
with a black gallery plate, a trans-yellow lamp and a red cone roof.
-}
lighthouse : List Brick
lighthouse =
    List.map
        (\k ->
            put
                (if modBy 2 (k // 2) == 0 then
                    15

                 else
                    4
                )
                ( 0, -24 * toFloat (k + 1), 0 )
                "3941.dat"
        )
        (List.range 0 9)
        ++ [ put 0 ( 0, -248, 0 ) "3022.dat"
           , put 46 ( 0, -272, 0 ) "3941.dat"
           , put 4 ( 0, -320, 0 ) "3942c.dat"
           ]


{-| Height of the lighthouse lamp's middle above its foot, in metres.
-}
lighthouseLamp : Float
lighthouseLamp =
    260 * Scenery.metersPerLdu


{-| A cairn on a bare fell: grey bricks stacked crosswise, a slope and a
round brick on top, with two rocks at its foot.
-}
cairn : List Brick
cairn =
    let
        rock =
            fromLDraw BrickModels.rock
    in
    [ put 72 ( 0, -24, 0 ) "3001.dat"
    , turn 90 71 ( 0, -48, 0 ) "3001.dat"
    , turn 45 72 ( 0, -72, 0 ) "3003.dat"
    , put 71 ( 0, -96, 0 ) "3003.dat"
    , turn 30 72 ( 0, -120, 0 ) "3039.dat"
    , put 71 ( 0, -144, 10 ) "3062b.dat"
    ]
        ++ shift ( 60, 0, 30 ) rock
        ++ shift ( -60, 0, -20 ) rock



-- SOURCES


{-| Every dressing model of a season (snowy or not), by prop name, for
`Game.BrickProps.refresh`. The text is built once: each season's list is a
constant.
-}
propSources : Bool -> List ( String, String )
propSources snow =
    if snow then
        snowySources

    else
        plainSources


plainSources : List ( String, String )
plainSources =
    sourcesFor False


snowySources : List ( String, String )
snowySources =
    sourcesFor True


sourcesFor : Bool -> List ( String, String )
sourcesFor snow =
    let
        model name bricks =
            ( name, toLDraw name bricks )
    in
    List.map (\k -> model (treeProp snow k) (treeBricks snow k)) allTreeKinds
        ++ List.map (\s -> model (houseProp snow s) (house snow s)) houseStyles
        ++ [ model (hallProp snow) (townHall snow)
           , model "dec:bush" bush
           , model "dec:flowers" flowers
           , model "dec:barrels" barrels
           , model "dec:lamp" lampPost
           , model "dec:tile" pathTile
           , model "dec:fence" fence
           , model "dec:lighthouse" lighthouse
           , model "dec:cairn" cairn
           ]



-- FAR PROXIES


{-| A piece of a far proxy, in metres in the model's own frame (y up from
its foot, its front towards -Z... once drawn, +Z): a box (centre and half
sizes) or a gable roof along X (base centre, half width, half depth,
height), in an LDraw colour (16 for the item's paint).
-}
type Shape
    = Box { x : Float, y : Float, z : Float, hx : Float, hy : Float, hz : Float, colour : Int }
    | Roof { x : Float, y : Float, z : Float, hx : Float, hz : Float, h : Float, colour : Int }


box : Int -> ( Float, Float ) -> ( Float, Float ) -> Float -> Shape
box colour ( y0, y1 ) ( w, d ) zc =
    Box { x = 0, y = 0.5 * (y0 + y1), z = zc, hx = 0.5 * w, hy = 0.5 * (y1 - y0), hz = 0.5 * d, colour = colour }


{-| The far proxy of a prop, or nothing for props too small to see far off.
-}
proxyOf : String -> List Shape
proxyOf name =
    let
        s =
            Scenery.metersPerLdu

        lu v =
            v * s

        snow =
            String.endsWith ":snow" name

        roofColour style =
            roofOf snow style

        houseProxy style =
            let
                ( w, d ) =
                    footprint style

                wallTop =
                    lu (24 * toFloat (layersOf style))
            in
            [ box 16 ( 0, wallTop ) ( lu (20 * toFloat w), lu (20 * toFloat d) ) 0
            , Roof { x = 0, y = wallTop, z = 0, hx = lu (10 * toFloat w), hz = lu (10 * toFloat d), h = lu 24, colour = roofColour style }
            ]

        pineProxy y0 =
            [ box 288 ( y0 + 0.3, y0 + 1.7 ) ( 3.0, 3.0 ) 0
            , box 288 ( y0 + 1.7, y0 + 3.0 ) ( 2.2, 2.2 ) 0
            , box 288 ( y0 + 3.0, y0 + 4.2 ) ( 1.5, 1.5 ) 0
            , box
                (if snow then
                    15

                 else
                    288
                )
                ( y0 + 4.2, y0 + 5.4 )
                ( 0.8, 0.8 )
                0
            ]

        ovalProxy y0 =
            [ box 16 ( y0 + 0.9, y0 + 1.8 ) ( 2.2, 2.2 ) 0
            , box 16 ( y0 + 1.8, y0 + 4.0 ) ( 3.0, 3.0 ) 0
            , box 16 ( y0 + 4.0, y0 + 5.0 ) ( 2.0, 2.0 ) 0
            ]

        palmProxy top =
            [ box 70 ( 0, top ) ( 0.6, 0.6 ) 0
            , box 16 ( top, top + 0.5 ) ( 3.4, 1.2 ) 0
            , box 16 ( top, top + 0.5 ) ( 1.2, 3.4 ) 0
            ]
    in
    case String.split ":" name of
        "dec" :: "tree" :: kind :: _ ->
            case kind of
                "palm" ->
                    palmProxy (lu 144)

                "tallPalm" ->
                    palmProxy (lu 192)

                "leafy" ->
                    [ box 70 ( 0, lu 72 ) ( 0.6, 0.6 ) 0
                    , box 16 ( lu 72, lu 120 ) ( 2.8, 2.8 ) 0
                    , box 16 ( lu 120, lu 168 ) ( 1.4, 1.4 ) 0
                    ]

                "oval" ->
                    box 70 ( 0, lu 24 ) ( 1.4, 1.4 ) 0 :: ovalProxy (lu 24)

                "birch" ->
                    box 15 ( 0, lu 60 ) ( 0.6, 0.6 ) 0 :: ovalProxy (lu 48)

                "pine" ->
                    pineProxy 0

                "tallPine" ->
                    box 70 ( 0, lu 24 ) ( 1.4, 1.4 ) 0 :: pineProxy (lu 24)

                _ ->
                    []

        "dec" :: "house" :: style :: _ ->
            case List.filter (\st -> styleKey st == style) houseStyles of
                st :: _ ->
                    houseProxy st

                [] ->
                    []

        "dec" :: "hall" :: _ ->
            [ box 15 ( 0, lu 120 ) ( lu 160, lu 120 ) 0
            , box
                (if snow then
                    15

                 else
                    72
                )
                ( lu 120, lu 128 )
                ( lu 160, lu 120 )
                0
            , box 15 ( lu 128, lu 208 ) ( lu 40, lu 40 ) (lu -10)
            , box 4 ( lu 208, lu 256 ) ( lu 30, lu 30 ) (lu -10)
            ]

        [ "dec", "tile" ] ->
            [ box 16 ( 0, lu 8 ) ( lu 80, lu 40 ) 0 ]

        [ "rock" ] ->
            [ box 72 ( 0, lu 24 ) ( lu 80, lu 40 ) 0
            , box 71 ( lu 24, lu 48 ) ( lu 60, lu 30 ) 0
            ]

        [ "barn" ] ->
            [ box 320 ( 0, lu 72 ) ( lu 160, lu 120 ) 0
            , Roof { x = 0, y = lu 72, z = 0, hx = lu 80, hz = lu 60, h = lu 24, colour = 72 }
            ]

        [ "hayBale" ] ->
            [ box 19 ( 0, lu 56 ) ( lu 80, lu 40 ) 0 ]

        _ ->
            []
