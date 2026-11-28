module GraphicsPauseTest exposing (suite)

{-| While the 3D canvas's WebGL context is lost (`GraphicsLost`) the voyage
is paused: `Tick` does nothing, and the player's inputs do nothing either,
so no cannonball is queued and the sail setting stays as it was. Once the
context is back (`GraphicsRestored`) the inputs work again.
-}

import Expect
import I18n exposing (Language(..))
import Pages.Sail as Sail exposing (SailModel, SailMsg(..))
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Test exposing (Test, describe, test)


step : SailModel -> SailModel
step m =
    Tuple.first (Sail.update (Tick (1000 / 60)) m)


{-| A voyage under way, its guns loaded.
-}
racing : SailModel
racing =
    let
        m =
            List.foldl (\_ x -> step x) (Sail.initSail En Boats.default Archipelago.defaultCourse True Nothing Nothing { available = False, enabled = False } 800 600) (List.range 1 200)
    in
    { m | cannonCooldown = 0 }


apply : List SailMsg -> SailModel -> SailModel
apply msgs m =
    List.foldl (\msg x -> Tuple.first (Sail.update msg x)) m msgs


suite : Test
suite =
    describe "inputs while the graphics are lost"
        [ test "a voyage under way fires" <|
            \_ ->
                List.length (apply [ FireCannons ] racing).cannonballs
                    |> Expect.greaterThan (List.length racing.cannonballs)
        , test "a voyage under way raises and lowers its sail" <|
            \_ ->
                ( (apply [ RaiseSail ] racing).sailLevel == racing.sailLevel
                , (apply [ LowerSail ] racing).sailLevel == racing.sailLevel
                )
                    |> Expect.equal ( False, False )
        , test "while lost, FireCannons adds no cannonball" <|
            \_ ->
                let
                    lost =
                        apply [ GraphicsLost ] racing
                in
                List.length (apply [ FireCannons, KeyDown "KeyF" ] lost).cannonballs
                    |> Expect.equal (List.length lost.cannonballs)
        , test "while lost, the sail setting does not change" <|
            \_ ->
                let
                    lost =
                        apply [ GraphicsLost ] racing
                in
                [ apply [ RaiseSail ] lost
                , apply [ LowerSail ] lost
                , apply [ KeyDown "KeyQ", KeyDown "ArrowDown" ] lost
                , apply [ KeyDown "KeyE", KeyDown "ArrowUp" ] lost
                , apply [ DropAnchor ] lost
                ]
                    |> List.map .sailLevel
                    |> Expect.equal (List.repeat 5 lost.sailLevel)
        , test "once restored, FireCannons works again" <|
            \_ ->
                let
                    restored =
                        apply [ GraphicsLost, FireCannons, GraphicsRestored ] racing
                in
                List.length (apply [ FireCannons ] restored).cannonballs
                    |> Expect.greaterThan (List.length restored.cannonballs)
        ]
