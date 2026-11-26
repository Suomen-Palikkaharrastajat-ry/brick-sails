module Sails.World exposing
    ( Dressing
    , HubLandmark(..)
    , IslandPalette
    , IslandScale
    , SeaPalette
    , Tree(..)
    , dressing
    , islandModels
    , islandPalette
    , islandScale
    , seaPalette
    , townNames
    , weatherProfile
    , windSpeed
    )

{-| What a map's biome, season and weather mean for Brick Sails: the size
and look of the islands, the colours of the sea and the sky, the course wind
and the weather's spells.

Each axis is its own small table: the biome sets the base (island scale,
dressing, sea colours, base wind), the season tints it and scales the wind,
and the weather picks the spell profile (`Sails.Weather.Profile`) and greys
the sky. `Sails.Archipelago` reads the island scale and the wind,
`Sails.Render` and `Sails.OceanShader` the colours and the dressing.

-}

import Game.BrickLook as BrickLook
import Game.MapParams exposing (Biome(..), MapParams, Season(..), WeatherKind(..))
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Weather as Weather



-- ISLANDS


{-| How the biome scales the harbour islands (radius and height) and how
many skerries (small rocks) lie round the course.
-}
type alias IslandScale =
    { radius : Float
    , height : Float
    , skerries : ( Int, Int )
    }


islandScale : Biome -> IslandScale
islandScale biome =
    case biome of
        Desert ->
            { radius = 1.0, height = 1.0, skerries = ( 2, 3 ) }

        Boreal ->
            { radius = 0.85, height = 1.0, skerries = ( 4, 5 ) }

        Farmland ->
            { radius = 1.0, height = 1.0, skerries = ( 2, 3 ) }

        Fell ->
            { radius = 0.9, height = 1.8, skerries = ( 3, 4 ) }


{-| The trees on the islands.
-}
type Tree
    = Palm
    | Pine
    | Leafy
    | NoTree


{-| What stands on the hub island: the pirates' fort, a lighthouse, or a
cairn of rocks on a bare fell.
-}
type HubLandmark
    = Fort
    | Lighthouse
    | Cairn


{-| The pieces a biome dresses its islands with. `farm` adds a barn and hay
on the harbour islands, `wreck` and `arches` say whether a wreck and rock
sea arches can lie out on the water.
-}
type alias Dressing =
    { tree : Tree
    , landmark : HubLandmark
    , rocks : Bool
    , farm : Bool
    , wreck : Bool
    , arches : Bool
    }


dressing : MapParams -> Dressing
dressing map =
    case map.biome of
        Desert ->
            { tree = Palm, landmark = Fort, rocks = False, farm = False, wreck = True, arches = True }

        Boreal ->
            { tree = Pine, landmark = Lighthouse, rocks = True, farm = False, wreck = True, arches = False }

        Farmland ->
            { tree = Leafy
            , landmark = Lighthouse
            , rocks = False
            , farm = map.season /= Winter
            , wreck = False
            , arches = False
            }

        Fell ->
            { tree = Pine, landmark = Cairn, rocks = True, farm = False, wreck = False, arches = True }


{-| The four tiers of an island model, as LDraw colour codes: the two beach
tiers, the grass and the little hill on top. `key` names the prop model.
-}
type alias IslandPalette =
    { key : String
    , beach : Int
    , shore : Int
    , grass : Int
    , hill : Int
    }


palette : Int -> Int -> Int -> Int -> IslandPalette
palette beach shore grass hill =
    { key = "island-" ++ String.join "-" (List.map String.fromInt [ beach, shore, grass, hill ])
    , beach = beach
    , shore = shore
    , grass = grass
    , hill = hill
    }


islandPalette : MapParams -> IslandPalette
islandPalette map =
    case ( map.biome, map.season ) of
        ( Desert, _ ) ->
            palette 19 19 28 2

        ( Boreal, Winter ) ->
            palette 72 71 15 15

        ( Boreal, _ ) ->
            palette 72 71 2 288

        ( Farmland, Winter ) ->
            palette 19 71 15 15

        ( Farmland, Autumn ) ->
            palette 19 2 28 484

        ( Farmland, Spring ) ->
            palette 19 2 10 10

        ( Farmland, Summer ) ->
            palette 19 2 10 2

        ( Fell, Winter ) ->
            palette 72 72 15 15

        ( Fell, Spring ) ->
            palette 72 71 71 15

        ( Fell, _ ) ->
            palette 72 71 2 71


{-| Every island model a map can use, by prop name, for
`Sails.Render.propSources`.
-}
islandModels : List ( String, String )
islandModels =
    let
        all =
            List.concatMap
                (\biome ->
                    List.map
                        (\season -> islandPalette { biome = biome, season = season, weather = Clear, seed = 1 })
                        [ Spring, Summer, Autumn, Winter ]
                )
                [ Desert, Boreal, Farmland, Fell ]

        add p acc =
            if List.any (\( k, _ ) -> k == p.key) acc then
                acc

            else
                acc ++ [ ( p.key, islandModel p ) ]
    in
    List.foldl add [] all


{-| An island built from real catalog bricks: a sloped beach foundation,
a stepped shore tier, a lush grassy plateau and an inland hill peak.
-}
islandModel : IslandPalette -> String
islandModel p =
    let
        sMat =
            "1 0 0 0 1 0 0 0 1"

        qMat =
            "0 0 1 0 1 0 -1 0 0"

        hMat =
            "-1 0 0 0 1 0 0 0 -1"

        tMat =
            "0 0 -1 0 1 0 1 0 0"

        part colour x y z mat file =
            String.join " "
                [ "1"
                , String.fromInt colour
                , String.fromFloat x
                , String.fromFloat y
                , String.fromFloat z
                , mat
                , file
                ]

        b =
            p.beach

        s =
            p.shore

        g =
            p.grass

        h =
            p.hill

        beachTier =
            [ part b 0 -24 -20 sMat "3001.dat"
            , part b 0 -24 20 sMat "3001.dat"
            , part b -60 -24 0 qMat "3001.dat"
            , part b 60 -24 0 qMat "3001.dat"
            , part b -20 -24 -60 hMat "3039.dat"
            , part b 20 -24 -60 hMat "3039.dat"
            , part b -20 -24 60 sMat "3039.dat"
            , part b 20 -24 60 sMat "3039.dat"
            , part b -80 -24 -20 qMat "3039.dat"
            , part b -80 -24 20 qMat "3039.dat"
            , part b 80 -24 -20 tMat "3039.dat"
            , part b 80 -24 20 tMat "3039.dat"
            , part b -60 -24 -60 sMat "3941.dat"
            , part b 60 -24 -60 sMat "3941.dat"
            , part b -60 -24 60 sMat "3941.dat"
            , part b 60 -24 60 sMat "3941.dat"
            ]

        shoreTier =
            [ part s 0 -48 0 sMat "3001.dat"
            , part s 0 -48 -40 sMat "3003.dat"
            , part s 0 -48 40 sMat "3003.dat"
            , part s -40 -48 -40 hMat "3039.dat"
            , part s 40 -48 -40 hMat "3039.dat"
            , part s -40 -48 40 sMat "3039.dat"
            , part s 40 -48 40 sMat "3039.dat"
            , part s -60 -48 0 qMat "3039.dat"
            , part s 60 -48 0 tMat "3039.dat"
            ]

        grassPlateau =
            [ part g 0 -72 0 sMat "3001.dat"
            , part g -20 -72 -40 hMat "3039.dat"
            , part g 20 -72 -40 hMat "3039.dat"
            , part g -20 -72 40 sMat "3039.dat"
            , part g 20 -72 40 sMat "3039.dat"
            , part g -40 -72 0 qMat "3039.dat"
            , part g 40 -72 0 tMat "3039.dat"
            ]

        hillPeak =
            [ part h 10 -96 -10 sMat "3003.dat"
            , part h 10 -96 10 sMat "3039.dat"
            , part h 10 -96 -30 hMat "3039.dat"
            , part h -10 -96 -10 qMat "3039.dat"
            , part h 30 -96 -10 tMat "3039.dat"
            ]
    in
    String.join "\n"
        ([ "0 FILE " ++ p.key ++ ".ldr"
         , "0 Name: Brick Sails Island"
         , "0 Author: Brick Sails"
         , "0 // Beach Tier"
         ]
            ++ beachTier
            ++ [ "0 // Shore Tier" ]
            ++ shoreTier
            ++ [ "0 // Grass Plateau" ]
            ++ grassPlateau
            ++ [ "0 // Hill Peak" ]
            ++ hillPeak
        )


{-| Names of the five towns of a map: the hub's first, then one for each
harbour island in ring order.
-}
townNames : Biome -> List String
townNames biome =
    case biome of
        Desert ->
            [ "Tortuga Cove", "Smuggler's Reach", "Port Royal Brick", "Dune Landing", "Governor's Anchorage" ]

        Boreal ->
            [ "Majakkasaari", "Kuusiluoto", "Hylkeenniemi", "Saunasalmi", "Kivikari" ]

        Farmland ->
            [ "Millbrook", "Haybarrow", "Clover Quay", "Applewick", "Orchard Pier" ]

        Fell ->
            [ "Cairnhold", "Tunturi Bay", "Kirkko Rock", "Ravenfjord", "Snowgate" ]



-- WIND AND WEATHER


{-| The course wind in m/s: the biome's base wind, scaled by the season and
the weather.
-}
windSpeed : MapParams -> Float
windSpeed map =
    let
        base =
            case map.biome of
                Desert ->
                    7.0

                Boreal ->
                    6.0

                Fell ->
                    8.0

                Farmland ->
                    6.5

        season =
            case map.season of
                Spring ->
                    1.0

                Summer ->
                    0.9

                Autumn ->
                    1.2

                Winter ->
                    1.15
    in
    base * season * (weatherProfile map).windScale


{-| The weather's spell profile (see `Sails.Weather.Profile`).
-}
weatherProfile : MapParams -> Weather.Profile
weatherProfile map =
    case map.weather of
        Clear ->
            Weather.clearProfile

        Overcast ->
            { firstFair = ( 14, 26 )
            , weights = { squall = 0.25, thunder = 0, lull = 0, fairOnly = False }
            , fairDuration = ( 20, 40 )
            , baseRain = 0
            , baseGloom = 0.3
            , baseSea = 1.1
            , windScale = 1.0
            , fogScale = 0.85
            , precip = Weather.RainFall
            }

        Rain ->
            { firstFair = ( 8, 16 )
            , weights = { squall = 0.7, thunder = 0, lull = 0, fairOnly = False }
            , fairDuration = ( 12, 24 )
            , baseRain = 0.45
            , baseGloom = 0.25
            , baseSea = 1.15
            , windScale = 1.05
            , fogScale = 0.8
            , precip = Weather.RainFall
            }

        Storm ->
            Weather.stormProfile

        Fog ->
            { firstFair = ( 10, 20 )
            , weights = { squall = 0, thunder = 0, lull = 0, fairOnly = True }
            , fairDuration = ( 14, 30 )
            , baseRain = 0
            , baseGloom = 0.1
            , baseSea = 0.8
            , windScale = 0.7
            , fogScale = 0.55
            , precip = Weather.RainFall
            }

        Snow ->
            { firstFair = ( 10, 20 )
            , weights = { squall = 0.35, thunder = 0, lull = 0, fairOnly = False }
            , fairDuration = ( 14, 30 )
            , baseRain = 0.5
            , baseGloom = 0.2
            , baseSea = 1.0
            , windScale = 0.95
            , fogScale = 0.6
            , precip = Weather.SnowFall
            }



-- SEA AND SKY


{-| Colours of the sea and the sky of a map: the plates in the troughs
(`deep`) and on the crests (`shallow`), the white caps, the lagoon round the
islands, and the sky at the `horizon` (also the fog) and the `zenith`.
-}
type alias SeaPalette =
    { deep : Vec3
    , shallow : Vec3
    , cap : Vec3
    , lagoon : Vec3
    , horizon : Vec3
    , zenith : Vec3
    }


mixV : Vec3 -> Vec3 -> Float -> Vec3
mixV a b f =
    Vec3.add (Vec3.scale (1 - f) a) (Vec3.scale f b)


greySky : Vec3
greySky =
    vec3 0.55 0.58 0.62


seaPalette : MapParams -> SeaPalette
seaPalette map =
    let
        base =
            case map.biome of
                Desert ->
                    { deep = BrickLook.colour 321
                    , shallow = BrickLook.colour 11
                    , cap = vec3 0.9 0.97 1.0
                    , lagoon = vec3 0.24 0.86 0.8
                    , horizon = vec3 0.8 0.92 0.95
                    , zenith = vec3 0.1 0.5 0.92
                    }

                Boreal ->
                    { deep = vec3 0.06 0.2 0.24
                    , shallow = vec3 0.15 0.36 0.38
                    , cap = vec3 0.88 0.94 0.95
                    , lagoon = vec3 0.2 0.42 0.4
                    , horizon = vec3 0.78 0.86 0.9
                    , zenith = vec3 0.18 0.45 0.78
                    }

                Farmland ->
                    { deep = vec3 0.08 0.24 0.45
                    , shallow = vec3 0.22 0.48 0.66
                    , cap = vec3 0.9 0.96 1.0
                    , lagoon = vec3 0.3 0.6 0.66
                    , horizon = vec3 0.82 0.9 0.96
                    , zenith = vec3 0.15 0.5 0.9
                    }

                Fell ->
                    { deep = vec3 0.1 0.2 0.32
                    , shallow = vec3 0.28 0.4 0.5
                    , cap = vec3 0.9 0.94 0.97
                    , lagoon = vec3 0.35 0.48 0.55
                    , horizon = vec3 0.8 0.85 0.9
                    , zenith = vec3 0.22 0.42 0.7
                    }

        seasoned =
            case map.season of
                Winter ->
                    { base
                        | cap = vec3 0.97 0.99 1.0
                        , horizon = mixV base.horizon (vec3 0.74 0.77 0.82) 0.4
                        , zenith = mixV base.zenith greySky 0.4
                    }

                Autumn ->
                    { base | zenith = mixV base.zenith greySky 0.2 }

                _ ->
                    base
    in
    case map.weather of
        Overcast ->
            { seasoned
                | horizon = mixV seasoned.horizon greySky 0.3
                , zenith = mixV seasoned.zenith greySky 0.6
            }

        Fog ->
            { seasoned
                | horizon = vec3 0.78 0.8 0.82
                , zenith = mixV seasoned.zenith (vec3 0.78 0.8 0.82) 0.5
            }

        Snow ->
            { seasoned | zenith = mixV seasoned.zenith greySky 0.4 }

        _ ->
            seasoned
