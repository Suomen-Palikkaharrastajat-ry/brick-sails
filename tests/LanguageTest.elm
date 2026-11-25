module LanguageTest exposing (suite)

{-| The interface language: Finnish by default, English from a `lang=en`
link or saved choice, and every phrase in both languages.
-}

import Expect
import I18n exposing (Language(..))
import Sails.Boats as Boats
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.Types exposing (Faction(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Sails.I18n"
        [ describe "Choosing the language from the language flag"
            [ test "Finnish is the default when no language is given" <|
                \_ -> Expect.equal Fi (T.fromFlag "")
            , test "lang=fi is Finnish" <|
                \_ -> Expect.equal Fi (T.fromFlag "fi")
            , test "lang=en is English" <|
                \_ -> Expect.equal En (T.fromFlag "en")
            , test "a regional English code is English" <|
                \_ -> Expect.equal En (T.fromFlag "en-GB")
            , test "an unknown code falls back to Finnish" <|
                \_ -> Expect.equal Fi (T.fromFlag "sv")
            , test "the saved code round-trips" <|
                \_ -> Expect.equal [ Fi, En ] (List.map (T.toCode >> T.fromFlag) [ Fi, En ])
            ]
        , describe "Every phrase has both languages"
            (List.map
                (\( key, phrase ) ->
                    test key <|
                        \_ ->
                            Expect.all
                                [ \_ -> Expect.notEqual "" (phrase Fi)
                                , \_ -> Expect.notEqual "" (phrase En)
                                , \_ -> Expect.notEqual (phrase En) (phrase Fi)
                                ]
                                ()
                )
                T.allPhrases
            )
        , describe "Game names shown in the player's language"
            [ test "every built-in boat has a Finnish name and description" <|
                \_ ->
                    Boats.all
                        |> List.filter
                            (\b ->
                                T.boatName Fi b.name
                                    == b.name
                                    || T.boatDescription Fi b.description
                                    == b.description
                            )
                        |> List.map .name
                        |> Expect.equal []
            , test "boat names stay English in English" <|
                \_ -> Expect.equal (List.map .name Boats.all) (List.map (.name >> T.boatName En) Boats.all)
            , test "every sail setting has a Finnish name" <|
                \_ ->
                    Physics.sailLevels
                        |> List.map Tuple.first
                        |> List.filter (\name -> T.sailLevelName Fi name == name)
                        |> Expect.equal []
            , test "a loaded build's description keeps its title" <|
                \_ -> Expect.equal "Rakennelmasi: Pole" (T.boatDescription Fi "Your build: Pole")
            , test "a too-big build keeps its part counts in Finnish" <|
                \_ ->
                    T.shipLoadError Fi "That build is too big to sail here (900 parts, at most 400)."
                        |> Expect.equal "Rakennelma on liian suuri purjehdittavaksi (900 osaa, enintään 400)."
            , test "factions have Finnish and English names" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Merirosvot" (T.factionName Fi Pirates)
                        , \_ -> Expect.equal "Pirates" (T.factionName En Pirates)
                        , \_ -> Expect.equal "Sinitakit" (T.factionName Fi BlueCoats)
                        , \_ -> Expect.equal "BlueCoats" (T.factionName En BlueCoats)
                        , \_ -> Expect.equal "Punitakit" (T.factionName Fi RedCoats)
                        , \_ -> Expect.equal "RedCoats" (T.factionName En RedCoats)
                        ]
                        ()
            , test "citadels have Finnish and English names" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Salakuljettajan poukama" (T.citadelName Fi Pirates)
                        , \_ -> Expect.equal "Smuggler's Reach" (T.citadelName En Pirates)
                        , \_ -> Expect.equal "Kuvernöörin ankkuripaikka" (T.citadelName Fi BlueCoats)
                        , \_ -> Expect.equal "Governor's Anchorage" (T.citadelName En BlueCoats)
                        , \_ -> Expect.equal "Port Royal Palikka" (T.citadelName Fi RedCoats)
                        , \_ -> Expect.equal "Port Royal Brick" (T.citadelName En RedCoats)
                        ]
                        ()
            ]
        ]
