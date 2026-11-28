module GroupLockPageTest exposing (suite)

{-| The ship editor's palette row has two lock buttons: the grouping lock
(`Core.GroupLock`), greyed with nothing selected, LOCKED on one group and
OPEN on loose bricks, and the hull lock, which still toggles `baseLocked`.
-}

import Expect
import Html.Attributes
import I18n
import LDraw.Document as Document
import Pages.Build as Build exposing (Msg(..))
import Sails.Editor as Editor exposing (Base)
import Set
import Studio.Core as Core
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    case Editor.bases of
        base :: _ ->
            tests base

        [] ->
            test "there is a starter hull" <| \_ -> Expect.fail "no starter hulls"


tests : Base -> Test
tests base =
    let
        opened =
            open base

        nothingSelected =
            step (StudioMsg (Core.Select Set.empty)) opened

        hullId =
            Document.mainPlacements base.document |> List.map .id |> List.head |> Maybe.withDefault -1

        hullSelected =
            step (StudioMsg (Core.SelectOnly hullId)) opened

        twoBricks =
            let
                added =
                    opened |> step (AddPart "3001.dat") |> step (AddPart "3001.dat")

                ids =
                    Document.mainPlacements (Build.document added)
                        |> List.filter (\p -> p.part == "3001.dat")
                        |> List.map .id
            in
            step (StudioMsg (Core.Select (Set.fromList ids))) added
    in
    describe "Ship editor lock buttons"
        [ test "the grouping button is disabled with no selection" <|
            \_ ->
                groupLockButton nothingSelected
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.attribute "data-state" "disabled")
                        , Selector.attribute (Html.Attributes.disabled True)
                        ]
        , test "the grouping button does nothing with no selection" <|
            \_ ->
                groupLockButton nothingSelected
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Expect.err
        , test "the grouping button shows LOCKED on a group" <|
            \_ ->
                Expect.all
                    [ \m -> Expect.equal Core.GroupLockLocked (Core.groupLock m.studio)
                    , \m ->
                        groupLockButton m
                            |> Query.has
                                [ Selector.attribute (Html.Attributes.attribute "data-state" "locked")
                                , Selector.attribute (Html.Attributes.disabled False)
                                ]
                    ]
                    hullSelected
        , test "the grouping button shows OPEN on loose bricks and pressing it locks them into one group" <|
            \_ ->
                Expect.all
                    [ \m -> groupLockButton m |> Query.has [ Selector.attribute (Html.Attributes.attribute "data-state" "open") ]
                    , \m ->
                        groupLockButton m
                            |> Event.simulate Event.click
                            |> Event.expect (StudioMsg Core.ToggleGroupLock)
                    , \m -> Expect.equal Core.GroupLockLocked (Core.groupLock (step (StudioMsg Core.ToggleGroupLock) m).studio)
                    ]
                    twoBricks
        , test "the hull lock still toggles baseLocked" <|
            \_ ->
                Expect.equal [ True, False, True ]
                    [ opened.baseLocked
                    , (step ToggleBaseLock opened).baseLocked
                    , (opened |> step ToggleBaseLock |> step ToggleBaseLock).baseLocked
                    ]
        , test "the hull lock button sends ToggleBaseLock" <|
            \_ ->
                Build.view opened
                    |> Query.fromHtml
                    |> Query.find [ Selector.attribute (Html.Attributes.attribute "data-testid" "base-lock") ]
                    |> Event.simulate Event.click
                    |> Event.expect ToggleBaseLock
        ]


groupLockButton : Build.Model -> Query.Single Msg
groupLockButton model =
    Build.view model
        |> Query.fromHtml
        |> Query.find [ Selector.attribute (Html.Attributes.attribute "data-testid" "group-lock") ]


open : Base -> Build.Model
open base =
    Build.init
        { ldrawBase = ""
        , ldrawFallbackBase = ""
        , supportsPointerEvents = True
        , language = I18n.En
        , width = 390
        , height = 844
        }
        base
        base.document
        |> Tuple.first


step : Msg -> Build.Model -> Build.Model
step msg model =
    let
        ( next, _, _ ) =
            Build.update msg model
    in
    next
