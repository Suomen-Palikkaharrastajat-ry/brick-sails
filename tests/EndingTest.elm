module EndingTest exposing (suite)

{-| The end of a voyage: when the last harbour is reached the celebration
starts (bricks burst over the boat, the stars pop in), the finish screen
offers sailing again or going back to the main menu, and sailing again
starts a fresh run with no ending.
-}

import Expect
import Html
import Html.Attributes as Attr
import I18n exposing (Language(..))
import Pages.Results as Results
import Pages.Sail as Sail exposing (SailModel, SailMsg(..))
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.Ending as Ending
import Sails.Physics as Physics
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


fresh : SailModel
fresh =
    Sail.initSail En Boats.default Archipelago.defaultCourse True Nothing Nothing { available = False, enabled = False } 800 600


{-| The run as the last harbour is reached: finished after 50 s of sailing
with two hits.
-}
finishing : SailModel
finishing =
    let
        p =
            fresh.progress
    in
    { fresh | progress = { p | finished = True, elapsedSeconds = 50, targetsHit = 2, currentWaypointIndex = p.totalWaypoints } }


{-| One animation frame of `dt` seconds (`Tick` takes milliseconds).
-}
tick : Float -> SailModel -> SailModel
tick dt model =
    Tuple.first (Sail.update (Tick (dt * 1000)) model)


ticks : Int -> Float -> SailModel -> SailModel
ticks n dt model =
    List.foldl (\_ m -> tick dt m) model (List.range 1 n)


type Choice
    = Again
    | NewMap
    | Menu


resultsFor : SailModel -> Query.Single Choice
resultsFor model =
    case model.ending of
        Just ending ->
            Results.viewResults
                { course = model.course
                , progress = model.progress
                , boatName = model.boat.name
                , personalBest = Nothing
                , ending = ending
                , crew = Nothing
                , language = Fi
                , actions = { onSailAgain = Again, onNewMap = NewMap, onBackToMenu = Menu }
                }
                |> Query.fromHtml

        Nothing ->
            Query.fromHtml (Html.text "no ending")


testId : String -> Selector.Selector
testId id =
    Selector.attribute (Attr.attribute "data-testid" id)


suite : Test
suite =
    describe "End of a voyage"
        [ test "a run under way has no ending" <|
            \_ -> Expect.equal Nothing (ticks 10 (1 / 60) fresh).ending
        , test "reaching the last harbour starts the ending and the first brick burst" <|
            \_ ->
                let
                    m =
                        tick (1 / 60) finishing
                in
                Expect.equal
                    ( Just 0, Just (Physics.finalTime finishing.progress), 14 )
                    ( Maybe.map .timer m.ending, Maybe.map .score m.ending, List.length m.debris )
        , test "three bursts go off in the first second and a half, then no more" <|
            \_ ->
                Expect.equal [ 42, 42 ]
                    [ List.length (ticks 100 (1 / 60) finishing).debris
                    , List.length (ticks 200 (1 / 60) finishing).debris
                    ]
        , test "the stars pop in one by one up to the run's stars" <|
            \_ ->
                let
                    ending =
                        Ending.start { targetSeconds = 60, personalBest = Nothing } finishing.progress
                in
                Expect.equal [ 0, 1, 2, 3, 3 ]
                    (List.map (\t -> Ending.starsShown { ending | timer = t }) [ 0.2, 0.5, 0.8, 1.2, 5 ])
        , test "a time over the personal best is not a new record" <|
            \_ ->
                Expect.equal ( True, False )
                    ( (Ending.start { targetSeconds = 60, personalBest = Just 99 } finishing.progress).isNewRecord
                    , (Ending.start { targetSeconds = 60, personalBest = Just 10 } finishing.progress).isNewRecord
                    )
        , test "sailing again starts a fresh run with no ending" <|
            \_ ->
                let
                    ( again, _ ) =
                        Sail.update RestartCourse (ticks 30 (1 / 60) finishing)
                in
                Expect.equal ( Nothing, False, 0 ) ( again.ending, again.progress.finished, List.length again.debris )
        , test "the finish screen shows the headline, the time and menu choice" <|
            \_ ->
                resultsFor (ticks 90 (1 / 60) finishing)
                    |> Expect.all
                        [ Query.has [ Selector.text "SATAMASSA!" ]
                        , Query.find [ testId "ending-menu" ] >> Query.has [ Selector.text "Palaa päävalikkoon" ]
                        ]
        , test "the menu button goes back to the main menu" <|
            \_ ->
                resultsFor (tick (1 / 60) finishing)
                    |> Query.find [ testId "ending-menu" ]
                    |> Event.simulate Event.click
                    |> Event.expect Menu
        ]
