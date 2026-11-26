module Sails.MenuIcons exposing
    ( biome
    , dice
    , season
    , weather
    )

{-| Icons of the shipyard menu's world buttons, drawn in the Feather style
(24 × 24 view box, 2 px round strokes in the current colour) for the things
Feather has no icon for: a dice, the four biomes, and spring, autumn, winter
and fog. The rest come from `FeatherIcons`.
-}

import FeatherIcons exposing (Icon)
import Game.MapParams exposing (Biome(..), Season(..), WeatherKind(..))
import Svg exposing (Svg)
import Svg.Attributes as SA


{-| A die showing five, for the button that rolls a new world.
-}
dice : Icon
dice =
    FeatherIcons.customIcon
        [ Svg.rect [ SA.x "3", SA.y "3", SA.width "18", SA.height "18", SA.rx "3", SA.ry "3" ] []
        , pip 8 8
        , pip 16 8
        , pip 12 12
        , pip 8 16
        , pip 16 16
        ]


pip : Int -> Int -> Svg Never
pip cx cy =
    Svg.circle
        [ SA.cx (String.fromInt cx)
        , SA.cy (String.fromInt cy)
        , SA.r "1"
        , SA.fill "currentColor"
        ]
        []


{-| A barn for farmland, a spruce for the boreal forest, a mountain for the
fell and a cactus for the desert.
-}
biome : Biome -> Icon
biome kind =
    case kind of
        Farmland ->
            paths [ "M3 21h18", "M5 21V11l7-6 7 6v10", "M10 21v-5h4v5" ]

        Boreal ->
            paths [ "M12 2L6 10h3l-4 6h14l-4-6h3z", "M12 16v6" ]

        Fell ->
            paths [ "M2 20L9 7l4 6 3-4 6 11z", "M7.5 10l1.5 1 1.5-1" ]

        Desert ->
            paths [ "M10 21V5a2 2 0 0 1 4 0v16", "M10 13H8a2 2 0 0 1-2-2V8", "M14 11h2a2 2 0 0 0 2-2V7", "M5 21h14" ]


{-| A flower for spring, the sun for summer, a leaf for autumn and a
snowflake for winter.
-}
season : Season -> Icon
season kind =
    case kind of
        Spring ->
            FeatherIcons.customIcon
                [ Svg.circle [ SA.cx "12", SA.cy "8", SA.r "3" ] []
                , path "M12 11v11"
                , path "M12 18c-3 0-5-2-5-4 3 0 5 2 5 4z"
                ]

        Summer ->
            FeatherIcons.sun

        Autumn ->
            paths [ "M5 19C5 10 10 5 20 4c-1 10-6 15-15 15z", "M5 19l9-9" ]

        Winter ->
            paths [ "M12 2v20", "M3.3 7l17.4 10", "M3.3 17l17.4-10", "M9 4l3 2 3-2", "M9 20l3-2 3 2" ]


{-| The map's weather: Feather's sun and clouds, and three hazy lines for fog.
-}
weather : WeatherKind -> Icon
weather kind =
    case kind of
        Clear ->
            FeatherIcons.sun

        Overcast ->
            FeatherIcons.cloud

        Rain ->
            FeatherIcons.cloudRain

        Storm ->
            FeatherIcons.cloudLightning

        Fog ->
            paths [ "M7 6h10", "M4 10h16", "M3 14h18", "M6 18h12" ]

        Snow ->
            FeatherIcons.cloudSnow


paths : List String -> Icon
paths ds =
    FeatherIcons.customIcon (List.map path ds)


path : String -> Svg Never
path d =
    Svg.path [ SA.d d ] []
