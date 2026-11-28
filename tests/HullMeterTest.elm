module HullMeterTest exposing (suite)

{-| The top-left hull meter's colour states (`Sails.Hull.healthState`,
`Sails.HUD.healthColours`).
-}

import Expect
import Html.Attributes as Attr
import I18n exposing (Language(..))
import Sails.HUD as HUD
import Sails.Hull as Hull exposing (HealthState(..))
import Sails.I18n as T
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


hullAt : Float -> Hull.Hull
hullAt share =
    { health = share * Hull.maxHealth, sinceDamage = 0, lastDamage = 0 }


suite : Test
suite =
    describe "Hull meter states"
        [ test "the thresholds are shares of max health" <|
            \_ -> Expect.equal ( 0.6, 0.4, 0.2 ) ( Hull.cautionBelow, Hull.hurtBelow, Hull.dangerBelow )
        , test "states across the range, boundaries included" <|
            \_ ->
                Expect.equal
                    [ HealthOk, HealthOk, HealthCaution, HealthCaution, HealthCaution, HealthHurt, HealthHurt, HealthHurt, HealthDanger, HealthDanger ]
                    (List.map Hull.healthStateFromFraction [ 1, 0.61, 0.6, 0.5, 0.41, 0.4, 0.3, 0.2, 0.19, 0 ])
        , test "a hull's state follows its health share, not an absolute number" <|
            \_ ->
                Expect.equal [ HealthOk, HealthCaution, HealthHurt, HealthDanger ]
                    (List.map (hullAt >> Hull.healthState) [ 0.9, 0.55, 0.25, 0.1 ])
        , test "health over max or under zero still has a state" <|
            \_ ->
                Expect.equal [ HealthOk, HealthDanger ]
                    (List.map Hull.healthState [ { health = 2 * Hull.maxHealth, sinceDamage = 0, lastDamage = 0 }, { health = -5, sinceDamage = 0, lastDamage = 0 } ])
        , test "each state has its own name and colours; Ok keeps the old look" <|
            \_ ->
                let
                    states =
                        [ HealthOk, HealthCaution, HealthHurt, HealthDanger ]
                in
                Expect.equal
                    ( [ "ok", "yellow", "hurt", "red" ]
                    , [ { icon = "text-sky-300", bar = "bg-emerald-400" }
                      , { icon = "text-brand-yellow", bar = "bg-brand-yellow" }
                      , { icon = "text-orange-400", bar = "bg-orange-500" }
                      , { icon = "text-red-400 animate-pulse", bar = "bg-red-500" }
                      ]
                    )
                    ( List.map Hull.healthStateName states, List.map HUD.healthColours states )
        , test "the state has words in both languages" <|
            \_ ->
                let
                    names =
                        [ "ok", "yellow", "hurt", "red" ]
                in
                Expect.all
                    [ \_ -> Expect.equal 4 (List.length (List.filter (\n -> T.hullStateText Fi n /= T.hullStateText En n) names))
                    , \_ -> Expect.equal "Danger: nearly sunk" (T.hullStateText En "red")
                    ]
                    ()
        , test "the meter carries its state and reads it out" <|
            \_ ->
                Query.fromHtml (HUD.viewMinimalHealth En (hullAt 0.3))
                    |> Query.has
                        [ Selector.attribute (Attr.attribute "data-health-state" "hurt")
                        , Selector.attribute (Attr.attribute "aria-label" "Hull 30 %: Badly damaged")
                        ]
        ]
