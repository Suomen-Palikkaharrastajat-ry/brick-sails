module Sails.IslandDressing exposing
    ( IslandPlan, Item, Spot, Obstacle(..)
    , plans, planIsland, treeCap, grassBand, treeSpacing, isClear, smallPropRange
    )

{-| What stands on each island: its trees, towns, farm, rocks, undergrowth
and landmark, as a list of brick props (`Sails.IslandDecor`) with where each
stands (`Item`).

Placement (`planIsland`) is a pure function of the archipelago and the
island's index, seeded by the map code, so every seat of a room sees the
same islands (it looks at the docks of every route, not one seat's):

  - each town (`Sails.Types.Town`) gets a cluster round its square: the town
    hall facing the sea, a tavern with barrels and then three to five more
    houses of the six `Sails.IslandDecor.HouseStyle`s either side, in two
    rows facing a path where the grass is wide enough and one row facing the
    sea where it is not; every house lays the path in front of its door, and
    a spur of tiles runs from the square to the town's pier;
  - the trees grow in two or three seeded groves in the grass band
    (`grassBand`), Poisson style (darts kept at least `treeSpacing`, 3 m,
    apart), then a few loose trees, up to `treeCap` per island role, of the
    biome's kinds (`Sails.IslandDecor.treeKinds`) in the season's leaves;
  - they keep clear (`isClear`) of the town bearing (30 degrees either side),
    every building and the path, every dock of every route on the island
    and its pier corridor, the treasure and the hub's landmark;
  - bushes, flowers and street lamps are small props, drawn only within
    `smallPropRange` of the camera.

`Sails.IslandMesh` plans every island once per voyage and merges the far
look of the props into one mesh per island; `Sails.Render` draws the real
bricks of the islands near the camera.

@docs IslandPlan, Item, Spot, Obstacle
@docs plans, planIsland, treeCap, grassBand, treeSpacing, isClear, smallPropRange

-}

import Game.MapParams as MapParams exposing (Biome(..), MapParams)
import Random
import Sails.Archipelago exposing (Archipelago)
import Sails.IslandDecor as Decor exposing (HouseStyle(..), Look, TreeKind(..))
import Sails.Physics as Physics
import Sails.Types exposing (Island, IslandRole(..), Town)
import Sails.World as World exposing (HubLandmark(..))



-- TYPES


{-| A spot on an island: world metres (`x`, `z` on the sea plane and `y`
up) and the compass bearing its front faces.
-}
type alias Spot =
    { x : Float
    , y : Float
    , z : Float
    , bearing : Float
    }


{-| One brick prop on an island: its prop name (`Sails.IslandDecor`), where
it stands, the LDraw colour its colour-16 parts are painted, whether it is a
small prop, and the radius and height of its bounds in metres.
-}
type alias Item =
    { prop : String
    , spot : Spot
    , tint : Int
    , small : Bool
    , reach : Float
    , height : Float
    }


{-| What trees keep clear of: a disc round a point, the compass `Sector` of
a bearing from the island's centre (`half` degrees either side), or a
`Corridor` along a bearing from `from` metres out to the shore, `half`
metres either side of the ray.
-}
type Obstacle
    = Disc { x : Float, z : Float, r : Float }
    | Sector { bearing : Float, half : Float }
    | Corridor { bearing : Float, half : Float, from : Float }


{-| One island's dressing: the trees (also among the items), the obstacles
they kept clear of, and every item.
-}
type alias IslandPlan =
    { index : Int
    , island : Island
    , trees : List ( Spot, TreeKind )
    , obstacles : List Obstacle
    , items : List Item
    }



-- RULES


{-| The most trees an island of a role carries.
-}
treeCap : IslandRole -> Int
treeCap role =
    case role of
        Hub ->
            30

        HarbourIsle ->
            25

        TreasureIsle ->
            8

        Skerry ->
            2


{-| How thickly a biome is wooded, as a share of the cap.
-}
density : Biome -> Float
density biome =
    case biome of
        Farmland ->
            1

        Boreal ->
            1

        Desert ->
            0.6

        Fell ->
            0.45


{-| Least distance between two trees, in metres.
-}
treeSpacing : Float
treeSpacing =
    3


{-| Small props (bushes, flowers, street lamps) are drawn only this near the
camera, in metres.
-}
smallPropRange : Float
smallPropRange =
    80


{-| The flat grass ring of an island, from and to (metres from its centre),
inside the stepped shell of `Sails.IslandMesh`: outside the hill's foot and
inside the cliff top.
-}
grassBand : Island -> ( Float, Float )
grassBand isl =
    let
        ( a, b ) =
            case isl.role of
                Hub ->
                    ( 0.44, 0.78 )

                HarbourIsle ->
                    ( 0.4, 0.78 )

                TreasureIsle ->
                    ( 0.5, 0.74 )

                Skerry ->
                    ( 0.42, 0.58 )
    in
    ( a * isl.radius, b * isl.radius )


{-| Whether a point (world x, z) on an island keeps clear of every obstacle.
-}
isClear : Island -> List Obstacle -> ( Float, Float ) -> Bool
isClear isl obstacles ( x, z ) =
    let
        dx =
            x - isl.x

        dz =
            z - isl.z

        bearing =
            180 / pi * atan2 dx dz

        blocks o =
            case o of
                Disc d ->
                    (x - d.x) ^ 2 + (z - d.z) ^ 2 < d.r * d.r

                Sector s ->
                    angleApart bearing s.bearing < s.half

                Corridor c ->
                    let
                        along =
                            dx * sin (degrees c.bearing) + dz * cos (degrees c.bearing)

                        across =
                            abs (dx * cos (degrees c.bearing) - dz * sin (degrees c.bearing))
                    in
                    along > c.from && across < c.half
    in
    not (List.any blocks obstacles)


angleApart : Float -> Float -> Float
angleApart a b =
    abs (a - b - 360 * toFloat (floor ((a - b + 180) / 360)))



-- PLANNING


{-| The dressing of every island of an archipelago, in `islands` order.
-}
plans : Archipelago -> List IslandPlan
plans arch =
    List.indexedMap (planIsland arch) arch.islands


{-| Point on an island `r` metres out along compass `bearing`.
-}
polar : Island -> Float -> Float -> ( Float, Float )
polar isl bearing r =
    ( isl.x + r * sin (degrees bearing), isl.z + r * cos (degrees bearing) )


bearingOf : Island -> ( Float, Float ) -> Float
bearingOf isl ( x, z ) =
    180 / pi * atan2 (x - isl.x) (z - isl.z)


radiusOf : Island -> ( Float, Float ) -> Float
radiusOf isl ( x, z ) =
    sqrt ((x - isl.x) ^ 2 + (z - isl.z) ^ 2)


item : String -> Int -> Float -> Float -> Spot -> Item
item prop tint reach height spot =
    { prop = prop, spot = spot, tint = tint, small = False, reach = reach, height = height }


smallItem : String -> Int -> Spot -> Item
smallItem prop tint spot =
    { prop = prop, spot = spot, tint = tint, small = True, reach = 1.5, height = 1.2 }


{-| The dressing of the island at `index` in `arch.islands`.
-}
planIsland : Archipelago -> Int -> Island -> IslandPlan
planIsland arch index isl =
    let
        map =
            arch.map

        look =
            Decor.lookFor map

        dress =
            World.dressing map

        seed0 =
            Random.initialSeed (MapParams.streamSeed ("dressing/" ++ String.fromInt index) map)

        grassY =
            Physics.grassHeight isl

        -- Docks of every route on this island, and the hub's final harbour.
        docks =
            List.concatMap
                (\r ->
                    List.filterMap
                        (\( i, b ) ->
                            if i == isl then
                                Just b

                            else
                                Nothing
                        )
                        r.harbours
                        ++ (if isl == arch.hub then
                                [ r.finalBearing ]

                            else
                                []
                           )
                )
                arch.routes

        dockObstacles =
            List.concatMap
                (\b ->
                    [ Sector { bearing = b, half = 15 }
                    , Corridor { bearing = b, half = 3.5, from = 0.45 * isl.radius }
                    ]
                )
                docks

        dockLanes =
            List.concatMap
                (\b ->
                    [ Sector { bearing = b, half = 6 }
                    , Corridor { bearing = b, half = 2.8, from = 0.4 * isl.radius }
                    ]
                )
                docks

        treasureObstacles =
            arch.routes
                |> List.head
                |> Maybe.map
                    (\r ->
                        if r.treasure == isl then
                            let
                                ( tx, tz ) =
                                    polar isl r.treasureBearing (0.86 * isl.radius)
                            in
                            [ Disc { x = tx, z = tz, r = 4 } ]

                        else
                            []
                    )
                |> Maybe.withDefault []

        ( landmarkX, landmarkZ ) =
            ( isl.x + 0.2 * isl.radius, isl.z + 0.1 * isl.radius )

        landmarkObstacles =
            if isl.role == Hub then
                [ Disc { x = landmarkX, z = landmarkZ, r = 6 } ]

            else
                []

        fixedObstacles =
            dockObstacles ++ treasureObstacles ++ landmarkObstacles

        towns =
            List.filter (\t -> t.island == isl) arch.towns

        ( townParts, seed1 ) =
            List.foldl
                (\t ( acc, s ) ->
                    let
                        ( part, s2 ) =
                            -- Houses stand on the grass band, below the
                            -- landmark on the summit: only the docks' piers
                            -- (2.8 m either side) and the treasure limit them.
                            townCluster look isl (dockLanes ++ treasureObstacles) t s
                    in
                    ( acc ++ [ part ], s2 )
                )
                ( [], seed0 )
                towns

        townObstacles =
            List.concatMap .obstacles townParts

        ( farm, seed2 ) =
            if isl.role == HarbourIsle && dress.farm then
                farmOf isl (fixedObstacles ++ townObstacles) seed1

            else
                ( { items = [], obstacles = [] }, seed1 )

        obstacles =
            fixedObstacles ++ townObstacles ++ farm.obstacles

        ( trees, seed3 ) =
            placeTrees map isl obstacles seed2

        ( rocks, seed4 ) =
            if dress.rocks && isl.role /= Skerry && isl.role /= TreasureIsle then
                scatter isl obstacles (List.map Tuple.first trees) 3 seed3

            else
                ( [], seed3 )

        ( undergrowth, _ ) =
            placeUndergrowth look isl obstacles trees seed4

        treeItems =
            List.indexedMap
                (\k ( spot, kind ) ->
                    item (Decor.treeProp look.snow kind) (Decor.treeTint look (fract (0.618 * toFloat k + 0.17 * toFloat index)) kind) 3.6 6.5 spot
                )
                trees

        rockItems =
            List.map (\( x, z ) -> item "rock" 16 2 1.7 { x = x, z = z, y = grassY, bearing = 37 * x }) rocks

        landmark =
            if isl.role == Hub then
                let
                    spot =
                        { x = landmarkX, z = landmarkZ, y = -0.6 + 0.94 * isl.height, bearing = 0 }
                in
                case dress.landmark of
                    Fort ->
                        [ item "fort" 16 5 9.5 spot ]

                    Lighthouse ->
                        [ item "dec:lighthouse" 16 2 11.5 spot ]

                    Cairn ->
                        [ item "dec:cairn" 16 3.5 5 spot ]

            else
                []
    in
    { index = index
    , island = isl
    , trees = trees
    , obstacles = obstacles
    , items = landmark ++ List.concatMap .items townParts ++ farm.items ++ treeItems ++ rockItems ++ undergrowth
    }


fract : Float -> Float
fract v =
    v - toFloat (floor v)


type alias Gen a =
    Random.Generator a


{-| A seeded point in the grass band, even over its area.
-}
bandPoint : Island -> Gen ( Float, Float )
bandPoint isl =
    let
        ( rIn, rOut ) =
            grassBand isl
    in
    Random.map2
        (\a u ->
            polar isl a (sqrt (rIn * rIn + u * (rOut * rOut - rIn * rIn)))
        )
        (Random.float 0 360)
        (Random.float 0 1)


inBand : Island -> Float -> ( Float, Float ) -> Bool
inBand isl margin p =
    let
        ( rIn, rOut ) =
            grassBand isl

        r =
            radiusOf isl p
    in
    r >= rIn + margin && r <= rOut - margin


farFrom : Float -> List ( Float, Float ) -> ( Float, Float ) -> Bool
farFrom gap others ( x, z ) =
    List.all (\( ox, oz ) -> (x - ox) ^ 2 + (z - oz) ^ 2 >= gap * gap) others



-- TREES


{-| Trees in two or three groves, then a few loose ones, kept `treeSpacing`
apart, in the band and clear of the obstacles, up to the role's cap.
-}
placeTrees : MapParams -> Island -> List Obstacle -> Random.Seed -> ( List ( Spot, TreeKind ), Random.Seed )
placeTrees map isl obstacles seed0 =
    let
        cap =
            treeCap isl.role

        ( target, seed1 ) =
            case isl.role of
                Skerry ->
                    if isl.radius < 7 then
                        ( 0, seed0 )

                    else
                        Random.step (Random.int 0 cap) seed0

                _ ->
                    ( min cap (round (toFloat cap * density map.biome)), seed0 )

        kinds =
            if isl.role == Skerry then
                Decor.skerryKinds map.biome

            else
                Decor.treeKinds map.biome

        ok placed p =
            inBand isl 1.2 p && isClear isl obstacles p && farFrom treeSpacing placed p

        ( groveCount, seed2 ) =
            Random.step (Random.int 2 3) seed1

        ( centreDraws, seed3 ) =
            Random.step (Random.list 30 (bandPoint isl)) seed2

        centres =
            List.foldl
                (\p acc ->
                    if List.length acc < groveCount && inBand isl 1.2 p && isClear isl obstacles p && farFrom 8 acc p then
                        acc ++ [ p ]

                    else
                        acc
                )
                []
                centreDraws

        ( rIn, rOut ) =
            grassBand isl

        groveR =
            clamp 3.5 8 (0.5 * (rOut - rIn) + 2)

        dartGen =
            Random.map3 (\g a u -> ( g, a, u )) (Random.int 0 2) (Random.float 0 360) (Random.float 0 1)

        ( darts, seed4 ) =
            Random.step (Random.list (24 * cap) dartGen) seed3

        groveTarget =
            round (0.8 * toFloat target)

        grown =
            case centres of
                [] ->
                    []

                _ ->
                    List.foldl
                        (\( g, a, u ) acc ->
                            if List.length acc >= groveTarget then
                                acc

                            else
                                let
                                    ( cx, cz ) =
                                        List.drop (modBy (List.length centres) g) centres |> List.head |> Maybe.withDefault ( isl.x, isl.z )

                                    d =
                                        groveR * sqrt u

                                    p =
                                        ( cx + d * sin (degrees a), cz + d * cos (degrees a) )
                                in
                                if ok acc p then
                                    acc ++ [ p ]

                                else
                                    acc
                        )
                        []
                        darts

        ( loose, seed5 ) =
            Random.step (Random.list (12 * cap) (bandPoint isl)) seed4

        spots =
            List.foldl
                (\p acc ->
                    if List.length acc >= target then
                        acc

                    else if ok acc p then
                        acc ++ [ p ]

                    else
                        acc
                )
                grown
                loose

        ( picks, seed6 ) =
            Random.step (Random.list (List.length spots) (Random.map2 Tuple.pair (Random.float 0 1) (Random.float 0 360))) seed5

        y =
            Physics.grassHeight isl
    in
    ( List.map2
        (\( x, z ) ( pick, yaw ) ->
            ( { x = x, y = y, z = z, bearing = yaw }, weighted pick Pine kinds )
        )
        spots
        picks
    , seed6
    )


weighted : Float -> a -> List ( Float, a ) -> a
weighted pick default options =
    let
        total =
            List.sum (List.map Tuple.first options)

        go left opts =
            case opts of
                ( w, a ) :: rest ->
                    if left < w || List.isEmpty rest then
                        a

                    else
                        go (left - w) rest

                [] ->
                    default
    in
    go (pick * total) options


{-| `n` loose spots in the band clear of the obstacles and the trees.
-}
scatter : Island -> List Obstacle -> List Spot -> Int -> Random.Seed -> ( List ( Float, Float ), Random.Seed )
scatter isl obstacles trees n seed0 =
    let
        ( draws, seed1 ) =
            Random.step (Random.list (12 * n) (bandPoint isl)) seed0

        treePoints =
            List.map (\s -> ( s.x, s.z )) trees
    in
    ( List.foldl
        (\p acc ->
            if List.length acc < n && inBand isl 1.5 p && isClear isl obstacles p && farFrom 2.5 treePoints p && farFrom 4 acc p then
                acc ++ [ p ]

            else
                acc
        )
        []
        draws
    , seed1
    )


{-| Bushes under about a third of the trees and flowers at the grove edges
(not in autumn or winter).
-}
placeUndergrowth : Look -> Island -> List Obstacle -> List ( Spot, TreeKind ) -> Random.Seed -> ( List Item, Random.Seed )
placeUndergrowth look isl obstacles trees seed0 =
    let
        gen =
            Random.map3 (\p a d -> { p = p, a = a, d = d }) (Random.float 0 1) (Random.float 0 360) (Random.float 1.4 2.0)

        ( draws, seed1 ) =
            Random.step (Random.list (List.length trees) gen) seed0

        treePoints =
            List.map (\( s, _ ) -> ( s.x, s.z )) trees

        one ( spot, _ ) r =
            let
                ( x, z ) =
                    ( spot.x + r.d * sin (degrees r.a), spot.z + r.d * cos (degrees r.a) )

                placed =
                    { x = x, y = spot.y, z = z, bearing = r.a }

                fits =
                    inBand isl 0.8 ( x, z ) && isClear isl obstacles ( x, z ) && farFrom 1.2 treePoints ( x, z )
            in
            if not fits then
                Nothing

            else if r.p < 0.35 then
                Just (smallItem "dec:bush" look.bushes placed)

            else if r.p < 0.5 && look.flowers then
                Just (smallItem "dec:flowers" 16 placed)

            else
                Nothing
    in
    ( List.filterMap identity (List.map2 one trees draws), seed1 )



-- TOWNS


type alias TownPart =
    { items : List Item
    , obstacles : List Obstacle
    }


{-| A town: the hall on the square at the town's bearing, the houses either
side of it round the island (in two rows facing the path between them where
the grass band is wide enough, else one row facing the sea), the spur of
path to the pier, street lamps by the square and flowers by some doors.
-}
townCluster : Look -> Island -> List Obstacle -> Town -> Random.Seed -> ( TownPart, Random.Seed )
townCluster look isl blocked town seed0 =
    let
        ( rIn, rOut ) =
            grassBand isl

        y =
            Physics.grassHeight isl

        twoRows =
            rOut - rIn >= 11

        -- Radii of the path and of the rows of buildings: each row's doors
        -- are 3.15 m from the path, where its frontage tiles lie.
        ( pathR, innerR, outerR ) =
            if twoRows then
                ( 0.5 * (rIn + rOut), 0.5 * (rIn + rOut) - 3.15, 0.5 * (rIn + rOut) + 3.15 )

            else
                ( rIn + 5.55, rIn + 2.4, rIn + 2.4 )

        b0 =
            town.bearing

        ( count, seed1 ) =
            Random.step (Random.int 4 6) seed0

        styleGen =
            Random.uniform Cottage [ TownHouse, LongHouse, Warehouse, StoneHouse, Cottage, TownHouse ]

        colourGen =
            Random.map2 Tuple.pair (Random.float 0 1) (Random.float -6 6)

        ( styles, seed2 ) =
            Random.step (Random.list 10 styleGen) seed1

        ( colours, seed3 ) =
            Random.step (Random.list 12 colourGen) seed2

        -- The tavern comes first, so every town has one by the square.
        houseStyles =
            Tavern :: styles

        pickColour pick list fallback =
            List.drop (min (List.length list - 1) (floor (pick * toFloat (List.length list)))) list |> List.head |> Maybe.withDefault fallback

        hallHalf =
            Tuple.first Decor.hallHalfSize

        -- Lanes of houses out from the square, right and left of the hall
        -- (and, with two rows, right and left of the spur across the path):
        -- each house goes on the open lane that reaches least far, 0.6 m past
        -- its last house; a lane closes where a house would not fit.
        lanes =
            if twoRows then
                [ { r = innerR, dir = 1, extent = hallHalf }
                , { r = innerR, dir = -1, extent = hallHalf }
                , { r = outerR, dir = 1, extent = 2.4 }
                , { r = outerR, dir = -1, extent = 2.4 }
                ]

            else
                [ { r = innerR, dir = 1, extent = hallHalf }
                , { r = innerR, dir = -1, extent = hallHalf }
                ]

        spur =
            Corridor { bearing = b0, half = 2.5, from = pathR }

        footprintClear ( r, u ) halfW halfD =
            let
                b =
                    b0 + u / r * 180 / pi

                corner dr du =
                    polar isl (b + du / (r + dr) * 180 / pi) (r + dr)

                corners =
                    [ corner 0 0
                    , corner halfD halfW
                    , corner halfD -halfW
                    , corner -halfD halfW
                    , corner -halfD -halfW
                    ]
            in
            r - halfD >= rIn - 0.5 && r + halfD <= rOut + 0.3 && List.all (isClear isl (spur :: blocked)) corners

        pack styleLeft open placed =
            case ( styleLeft, List.sortBy .extent open ) of
                ( style :: rest, lane :: others ) ->
                    if List.length placed >= count then
                        placed

                    else
                        let
                            ( hw, hd ) =
                                Decor.houseHalfSize style

                            u =
                                lane.dir * (lane.extent + 0.6 + hw)
                        in
                        if footprintClear ( lane.r, u ) hw hd then
                            pack rest ({ lane | extent = lane.extent + 0.6 + 2 * hw } :: others) (placed ++ [ ( style, ( lane.r, u ) ) ])

                        else if style /= Cottage && style /= Tavern then
                            -- A narrow cottage may still fit where this did not.
                            pack (Cottage :: rest) open placed

                        else
                            pack styleLeft others placed

                _ ->
                    placed

        placeHouses =
            pack houseStyles lanes []

        spotOf ( r, u ) facingOut jitter =
            let
                b =
                    b0 + u / r * 180 / pi

                ( x, z ) =
                    polar isl b r
            in
            { x = x
            , y = y
            , z = z
            , bearing =
                if facingOut then
                    b + jitter

                else
                    b + 180 + jitter
            }

        hall =
            item (Decor.hallProp look.snow) 15 5 9 (spotOf ( innerR, 0 ) True 0)

        houses =
            List.indexedMap
                (\k ( style, ( r, u ) ) ->
                    let
                        ( wallPick, jitter ) =
                            List.drop k colours |> List.head |> Maybe.withDefault ( 0.5, 0 )

                        facingOut =
                            not twoRows || r < pathR
                    in
                    { style = style
                    , slot = ( r, u )
                    , item = item (Decor.houseProp look.snow style) (pickColour wallPick look.walls 15) 4.5 6 (spotOf ( r, u ) facingOut jitter)
                    }
                )
                placeHouses

        spurTiles =
            List.range 1 (floor ((rOut - 0.6 - pathR) / 2.8))
                |> List.map
                    (\k ->
                        let
                            ( x, z ) =
                                polar isl b0 (pathR + 0.7 + 2.8 * (toFloat k - 0.5))
                        in
                        item "dec:tile" look.path 1.6 0.3 { x = x, y = y, z = z, bearing = b0 + 90 }
                    )

        lamps =
            [ smallItem "dec:lamp" 16 (spotOf ( pathR + 1.2, -1.4 ) True 0)
            , smallItem "dec:lamp" 16 (spotOf ( pathR + 1.2, 1.4 ) True 0)
            ]

        flowerBeds =
            if look.flowers then
                houses
                    |> List.indexedMap
                        (\k h ->
                            let
                                ( r, u ) =
                                    h.slot

                                side =
                                    if r < pathR || not twoRows then
                                        2.0

                                    else
                                        -2.0
                            in
                            if modBy 2 k == 1 then
                                Just (smallItem "dec:flowers" 16 (spotOf ( r + side, u + 1.0 ) True 0))

                            else
                                Nothing
                        )
                    |> List.filterMap identity

            else
                []

        disc ( r, u ) reach =
            let
                ( x, z ) =
                    polar isl (b0 + u / r * 180 / pi) r
            in
            Disc { x = x, z = z, r = reach }

        reachU =
            List.map (\h -> abs (Tuple.second h.slot) + 2.5) houses
                |> List.maximum
                |> Maybe.withDefault 4
                |> max (hallHalf + 1)

        pathDiscs =
            List.range (negate (ceiling (reachU / 2))) (ceiling (reachU / 2))
                |> List.map (\k -> disc ( pathR, 2 * toFloat k ) 1.8)

        obstacles =
            Sector { bearing = b0, half = 30 }
                :: Corridor { bearing = b0, half = 3, from = pathR - 1 }
                :: disc ( innerR, 0 ) 5.2
                :: List.map
                    (\h ->
                        let
                            ( hw, hd ) =
                                Decor.houseHalfSize h.style
                        in
                        disc h.slot (sqrt (hw * hw + hd * hd) + 1.3)
                    )
                    houses
                ++ pathDiscs
    in
    ( { items = hall :: List.map .item houses ++ spurTiles ++ lamps ++ flowerBeds
      , obstacles = obstacles
      }
    , seed3
    )



-- FARMS


{-| A barn with hay and a fence on a harbour island of a farming biome, on
the first clear spot of the band.
-}
farmOf : Island -> List Obstacle -> Random.Seed -> ( { items : List Item, obstacles : List Obstacle }, Random.Seed )
farmOf isl obstacles seed0 =
    let
        ( draws, seed1 ) =
            Random.step (Random.list 40 (bandPoint isl)) seed0

        fits p =
            inBand isl 4 p && isClear isl obstacles p
    in
    case List.head (List.filter fits draws) of
        Just p ->
            let
                b =
                    bearingOf isl p

                r =
                    radiusOf isl p

                y =
                    Physics.grassHeight isl

                spotAt dr du facing =
                    let
                        bb =
                            b + du / r * 180 / pi

                        ( x, z ) =
                            polar isl bb (r + dr)
                    in
                    { x = x, y = y, z = z, bearing = bb + facing }
            in
            ( { items =
                    [ item "barn" 16 3.6 3.4 (spotAt 0 0 0)
                    , item "hayBale" 16 1.6 2 (spotAt 1.5 4.2 30)
                    , item "dec:fence" 16 2 1 (spotAt 3.4 -2.5 0)
                    , item "dec:fence" 16 2 1 (spotAt 3.4 1.0 0)
                    ]
              , obstacles = [ Disc { x = Tuple.first p, z = Tuple.second p, r = 6.5 } ]
              }
            , seed1
            )

        Nothing ->
            ( { items = [], obstacles = [] }, seed1 )
