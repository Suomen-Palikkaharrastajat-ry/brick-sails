module IslandDressingTest exposing (suite)

{-| The island dressing: trees keep clear of the towns, docks, treasure and
landmark, stay within the caps and the grass band, are 3 m apart, and the
same on every seat; the decor models use only parts in the catalog.
-}

import Expect
import Game.MapParams as MapParams exposing (Biome(..), MapParams, Season(..), WeatherKind(..))
import LDraw.Document as Document
import Sails.Archipelago as Archipelago
import Sails.IslandDecor as Decor
import Sails.IslandDressing as Dressing exposing (IslandPlan)
import Sails.IslandMesh as IslandMesh
import Sails.Types exposing (Island, IslandRole(..))
import Set
import Test exposing (Test, describe, test)


maps : List MapParams
maps =
    List.concatMap
        (\( biome, weather ) ->
            List.concatMap
                (\season ->
                    List.map (\seed -> { biome = biome, season = season, weather = MapParams.coerce biome season weather, seed = seed }) [ 3, 41 ]
                )
                [ Spring, Summer, Autumn, Winter ]
        )
        [ ( Farmland, Clear ), ( Boreal, Overcast ), ( Desert, Clear ), ( Fell, Fog ) ]


angleApart : Float -> Float -> Float
angleApart a b =
    abs (a - b - 360 * toFloat (floor ((a - b + 180) / 360)))


bearingFrom : Island -> ( Float, Float ) -> Float
bearingFrom isl ( x, z ) =
    180 / pi * atan2 (x - isl.x) (z - isl.z)


dist : ( Float, Float ) -> ( Float, Float ) -> Float
dist ( ax, az ) ( bx, bz ) =
    sqrt ((ax - bx) ^ 2 + (az - bz) ^ 2)


{-| Every way a tree of a plan breaks the rules, as text.
-}
problems : Archipelago.Archipelago -> IslandPlan -> List String
problems arch plan =
    let
        isl =
            plan.island

        points =
            List.map (\( s, _ ) -> ( s.x, s.z )) plan.trees

        ( rIn, rOut ) =
            Dressing.grassBand isl

        townBearings =
            arch.towns |> List.filter (\t -> t.island == isl) |> List.map .bearing

        dockBearings =
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

        chest =
            arch.routes
                |> List.head
                |> Maybe.andThen
                    (\r ->
                        if r.treasure == isl then
                            Just ( isl.x + 0.86 * isl.radius * sin (degrees r.treasureBearing), isl.z + 0.86 * isl.radius * cos (degrees r.treasureBearing) )

                        else
                            Nothing
                    )

        buildings =
            plan.items
                |> List.filter (\it -> String.startsWith "dec:house" it.prop || String.startsWith "dec:hall" it.prop)
                |> List.map (\it -> ( it.spot.x, it.spot.z ))

        landmark =
            if isl.role == Hub then
                Just ( isl.x + 0.2 * isl.radius, isl.z + 0.1 * isl.radius )

            else
                Nothing

        each p =
            let
                r =
                    dist p ( isl.x, isl.z )

                b =
                    bearingFrom isl p
            in
            List.filterMap identity
                [ if r < rIn || r > rOut then
                    Just "outside the grass band"

                  else
                    Nothing
                , if List.any (\tb -> angleApart b tb < 30) townBearings then
                    Just "within 30 degrees of a town"

                  else
                    Nothing
                , if List.any (\db -> angleApart b db < 15) dockBearings then
                    Just "within 15 degrees of a dock"

                  else
                    Nothing
                , if List.any (\h -> dist p h < 2.5) buildings then
                    Just "on a building"

                  else
                    Nothing
                , if Maybe.map (\c -> dist p c < 4) chest |> Maybe.withDefault False then
                    Just "on the treasure"

                  else
                    Nothing
                , if Maybe.map (\l -> dist p l < 6) landmark |> Maybe.withDefault False then
                    Just "on the landmark"

                  else
                    Nothing
                , if not (Dressing.isClear isl plan.obstacles p) then
                    Just "inside an obstacle"

                  else
                    Nothing
                ]

        crowded =
            List.indexedMap
                (\i p ->
                    List.indexedMap
                        (\j q ->
                            if i < j && dist p q < Dressing.treeSpacing - 1.0e-6 then
                                Just "two trees closer than 3 m"

                            else
                                Nothing
                        )
                        points
                        |> List.filterMap identity
                )
                points
                |> List.concat

        overCap =
            if List.length plan.trees > Dressing.treeCap isl.role then
                [ "over the cap" ]

            else
                []
    in
    List.concatMap each points ++ crowded ++ overCap


{-| Parts the decor models may use: all present in the catalog's raw/parts.
-}
catalogParts : List String
catalogParts =
    [ "3470.dat", "3471.dat", "2417.dat", "2423.dat", "32607.dat", "3742.dat", "30136.dat", "98283.dat", "60592.dat", "60593.dat", "60601.dat", "60602.dat", "87552.dat", "6182.dat", "3037.dat", "3038.dat", "3039.dat", "3045.dat", "3298.dat", "3942c.dat", "4589.dat", "3633.dat", "2489.dat", "2335.dat", "3957a.dat", "4070.dat", "3068b.dat", "87079.dat", "3941.dat", "3062b.dat", "3005.dat", "3004.dat", "3010.dat", "3009.dat", "3001.dat", "3003.dat", "3622.dat", "3024.dat", "3022.dat", "3036.dat", "3010.dat", "3008.dat" ]


suite : Test
suite =
    describe "Island dressing"
        [ test "trees keep clear of towns, docks, buildings, treasure and landmark, within the caps and 3 m apart" <|
            \_ ->
                let
                    found =
                        maps
                            |> List.concatMap
                                (\m ->
                                    let
                                        arch =
                                            Archipelago.generate m
                                    in
                                    Dressing.plans arch
                                        |> List.concatMap (\p -> problems arch p |> List.map (\e -> MapParams.toCode m ++ " island " ++ String.fromInt p.index ++ ": " ++ e))
                                )
                in
                Expect.equal [] (List.take 10 found)
        , test "the caps are hub 30, harbour island 25, treasure island 8 and skerry 2" <|
            \_ ->
                Expect.equal [ 30, 25, 8, 2 ] (List.map Dressing.treeCap [ Hub, HarbourIsle, TreasureIsle, Skerry ])
        , test "wooded islands are well wooded and the hub and harbour islands reach their caps on farmland" <|
            \_ ->
                let
                    farm =
                        { biome = Farmland, season = Summer, weather = Clear, seed = 3 }

                    counts =
                        Dressing.plans (Archipelago.generate farm)
                            |> List.filter (\p -> p.island.role == Hub || p.island.role == HarbourIsle)
                            |> List.map (\p -> toFloat (List.length p.trees) / toFloat (Dressing.treeCap p.island.role))
                in
                Expect.atLeast 0.7 (List.sum counts / toFloat (max 1 (List.length counts)))
        , test "the dressing is a pure function of the map: the same on every seat" <|
            \_ ->
                let
                    arch =
                        Archipelago.generate { biome = Boreal, season = Winter, weather = Snow, seed = 77 }

                    itemsOf seat =
                        IslandMesh.forCourse (Archipelago.courseFor arch seat) |> List.map .items
                in
                Expect.all
                    [ \_ -> Expect.equal (itemsOf 0) (itemsOf 1)
                    , \_ -> Expect.equal (itemsOf 0) (itemsOf 2)
                    , \_ -> Expect.equal (Dressing.plans arch) (Dressing.plans (Archipelago.generate arch.map))
                    ]
                    ()
        , test "every biome grows at least two kinds of tree" <|
            \_ ->
                let
                    kindsOf biome =
                        maps
                            |> List.filter (\m -> m.biome == biome)
                            |> List.concatMap (\m -> Dressing.plans (Archipelago.generate m))
                            |> List.concatMap (\p -> List.map (\( _, k ) -> Decor.treeProp False k) p.trees)
                            |> Set.fromList
                            |> Set.size
                in
                Expect.equal [ True, True, True, True ] (List.map (\b -> kindsOf b >= 2) [ Farmland, Boreal, Desert, Fell ])
        , test "every town has a hall, a tavern and four to six houses (two or more on the crowded hub)" <|
            \_ ->
                let
                    towns =
                        maps
                            |> List.concatMap
                                (\m ->
                                    let
                                        arch =
                                            Archipelago.generate m
                                    in
                                    Dressing.plans arch
                                        |> List.concatMap
                                            (\p ->
                                                case List.filter (\t -> t.island == p.island) arch.towns of
                                                    [] ->
                                                        []

                                                    ts ->
                                                        [ { halls = List.length (List.filter (\it -> String.startsWith "dec:hall" it.prop) p.items)
                                                          , taverns = List.length (List.filter (\it -> String.startsWith "dec:house:tavern" it.prop) p.items)
                                                          , houses = List.length (List.filter (\it -> String.startsWith "dec:house" it.prop) p.items)
                                                          , towns = List.length ts
                                                          , least =
                                                                -- The hub's town is squeezed between its docks.
                                                                if p.island.role == Hub then
                                                                    2

                                                                else
                                                                    4
                                                          , code = MapParams.toCode m ++ " " ++ String.fromInt p.index
                                                          }
                                                        ]
                                            )
                                )

                    bad =
                        List.filter (\t -> t.towns /= 1 || t.halls /= 1 || t.taverns /= 1 || t.houses < t.least || t.houses > 6) towns
                in
                Expect.equal [] (List.take 5 bad)
        , test "the decor models use only parts in the catalog" <|
            \_ ->
                let
                    used =
                        (Decor.propSources False ++ Decor.propSources True)
                            |> List.concatMap (\( _, ldraw ) -> Document.flattenedPlacements (Document.parse ldraw) |> List.map .part)
                            |> Set.fromList
                in
                Expect.equal [] (Set.toList (Set.diff used (Set.fromList catalogParts)))
        , test "every non-small prop but the landmark has a far proxy" <|
            \_ ->
                let
                    missing =
                        maps
                            |> List.take 8
                            |> List.concatMap (\m -> Dressing.plans (Archipelago.generate m))
                            |> List.concatMap .items
                            |> List.filter (\it -> not it.small && List.isEmpty (Decor.proxyOf it.prop))
                            |> List.map .prop
                            |> Set.fromList
                            |> Set.toList
                in
                Expect.equal [] (List.filter (\p -> not (List.member p [ "fort", "dec:lighthouse", "dec:cairn", "dec:fence" ])) missing)
        ]
