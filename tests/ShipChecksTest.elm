module ShipChecksTest exposing (suite)

{-| The ship editor's build checks (`Pages.Build.shipChecks`): a new ship on a
starter hull is prompted for a mast, a sail and a gun; each prompt goes away
once that kind of part is on board, and each helper button adds the part
that clears it (a mast `3957a`, a white 1 x 3 brick sail `3622`, a cannon
`32828`). The builder screen shows the prompts, in English and Finnish, and
no longer asks for a ship's wheel.
-}

import Expect
import Html
import Html.Attributes
import I18n
import LDraw.Document as Document exposing (Document)
import Pages.Build as Build exposing (Msg(..), ShipCheck(..))
import Sails.Editor as Editor exposing (Base)
import Sails.Ingest as Ingest
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "Ship editor build checks"
        (case Editor.bases of
            base :: _ ->
                checkTests base :: viewTests base :: List.map starterTest Editor.bases

            [] ->
                [ test "has starter hulls" (\_ -> Expect.fail "no starter hulls") ]
        )


starterTest : Base -> Test
starterTest base =
    describe base.id
        [ test "a new ship is prompted for a mast, a sail and guns" <|
            \_ -> Expect.equal [ NoMast, NoSail, NoGuns ] (Build.shipChecks base.document)
        , test "the three helpers clear every prompt" <|
            \_ ->
                [ Build.shipCheckFix NoMast, Build.shipCheckFix NoSail, Build.shipCheckFix NoGuns ]
                    |> List.foldl step (open I18n.En base)
                    |> Build.document
                    |> Build.shipChecks
                    |> Expect.equal []
        , test "the sail helper alone clears the sail prompt" <|
            \_ ->
                step (Build.shipCheckFix NoSail) (open I18n.En base)
                    |> Build.document
                    |> Build.shipChecks
                    |> Expect.equal [ NoMast, NoGuns ]
        ]


checkTests : Base -> Test
checkTests base =
    let
        opened =
            open I18n.En base

        after msgs =
            List.foldl step opened msgs |> Build.document
    in
    describe "prompts and helpers"
        [ test "the sail helper adds one white (colour 15) 1 x 3 brick" <|
            \_ ->
                Expect.equal [ ( "3622.dat", 15 ) ]
                    (added base.document (after [ Build.shipCheckFix NoSail ]))
        , test "the sail helper adds a white brick even when another colour is picked" <|
            \_ ->
                Expect.equal [ ( "3622.dat", 15 ) ]
                    (added base.document (after [ PickColour 4, Build.shipCheckFix NoSail ]))
        , test "a white sail brick on the deck clears the sail prompt only" <|
            \_ ->
                Expect.equal [ NoMast, NoGuns ] (Build.shipChecks (after [ Build.shipCheckFix NoSail ]))
        , test "the white sail brick passes the game's sail rule" <|
            \_ ->
                let
                    doc =
                        after [ Build.shipCheckFix NoSail ]

                    placements =
                        Document.flattenedPlacements doc
                in
                Expect.equal [ "3622.dat" ]
                    (placements |> List.filter (Ingest.sailRule placements) |> List.map .part)
        , test "a flag sail also clears the sail prompt" <|
            \_ ->
                Expect.equal False (List.member NoSail (Build.shipChecks (after [ AddPart "2335.dat" ])))
        , test "a 1 x 3 brick in another colour is not a sail" <|
            \_ ->
                Expect.equal True (List.member NoSail (Build.shipChecks (after [ PickColour 4, AddPart "3622.dat" ])))
        , test "the gun helper adds one 32828 cannon" <|
            \_ ->
                Expect.equal [ "32828.dat" ]
                    (added base.document (after [ Build.shipCheckFix NoGuns ]) |> List.map Tuple.first)
        , test "a 32828 cannon clears the gun prompt only" <|
            \_ ->
                Expect.equal [ NoMast, NoSail ] (Build.shipChecks (after [ Build.shipCheckFix NoGuns ]))
        , test "the mast helper adds one 3957a mast" <|
            \_ ->
                Expect.equal [ "3957a.dat" ]
                    (added base.document (after [ Build.shipCheckFix NoMast ]) |> List.map Tuple.first)
        , test "a mast clears the mast prompt only" <|
            \_ ->
                Expect.equal [ NoSail, NoGuns ] (Build.shipChecks (after [ Build.shipCheckFix NoMast ]))
        , test "the editor's mast pillar counts as a mast" <|
            \_ ->
                Expect.equal False (List.member NoMast (Build.shipChecks (after [ AddPart "43888.dat" ])))
        , test "with a mast, a sail and a cannon there are no prompts" <|
            \_ ->
                Expect.equal []
                    (Build.shipChecks
                        (after [ Build.shipCheckFix NoMast, Build.shipCheckFix NoSail, Build.shipCheckFix NoGuns ])
                    )
        , test "a ship's wheel is not asked for and clears nothing" <|
            \_ ->
                Expect.equal [ NoMast, NoSail, NoGuns ] (Build.shipChecks (after [ AddPart "3829c01.dat" ]))
        ]


viewTests : Base -> Test
viewTests base =
    let
        screen lang msgs =
            List.foldl step (open lang base) msgs |> Build.view |> Query.fromHtml
    in
    describe "builder screen"
        [ test "a new ship shows the mast, sail and gun prompts" <|
            \_ ->
                screen I18n.En []
                    |> Query.find [ Selector.attribute (attr "data-testid" "builder-checks") ]
                    |> Expect.all
                        [ Query.has [ Selector.text "Your ship has no mast" ]
                        , Query.has [ Selector.text "Your ship has no sails" ]
                        , Query.has [ Selector.text "Your ship has no guns" ]
                        , Query.has [ Selector.text "+ Add sail" ]
                        , Query.has [ Selector.text "+ Add cannon" ]
                        , Query.has [ Selector.text "+ Add mast" ]
                        , Query.hasNot [ Selector.text "wheel" ]
                        ]
        , test "the prompts are in Finnish too" <|
            \_ ->
                screen I18n.Fi []
                    |> Query.find [ Selector.attribute (attr "data-testid" "builder-checks") ]
                    |> Expect.all
                        [ Query.has [ Selector.text "Laivassasi ei ole mastoa" ]
                        , Query.has [ Selector.text "Laivassasi ei ole purjeita" ]
                        , Query.has [ Selector.text "Laivassasi ei ole tykkejä" ]
                        , Query.has [ Selector.text "+ Lisää purje" ]
                        , Query.has [ Selector.text "+ Lisää tykki" ]
                        ]
        , test "a ship with a white sail brick has no sail prompt" <|
            \_ ->
                screen I18n.En [ Build.shipCheckFix NoSail ]
                    |> Query.findAll [ Selector.attribute (attr "data-testid" "builder-check-sail") ]
                    |> Query.count (Expect.equal 0)
        , test "a ship with a 32828 cannon has no gun prompt" <|
            \_ ->
                screen I18n.En [ AddPart "32828.dat" ]
                    |> Query.findAll [ Selector.attribute (attr "data-testid" "builder-check-guns") ]
                    |> Query.count (Expect.equal 0)
        , test "a ship with a mast has no mast prompt" <|
            \_ ->
                screen I18n.En [ AddPart "3957a.dat" ]
                    |> Query.findAll [ Selector.attribute (attr "data-testid" "builder-check-mast") ]
                    |> Query.count (Expect.equal 0)
        , test "a fully rigged and armed ship shows no prompts" <|
            \_ ->
                screen I18n.En [ AddPart "3957a.dat", Build.shipCheckFix NoSail, AddPart "32828.dat" ]
                    |> Query.findAll [ Selector.attribute (attr "data-testid" "builder-checks") ]
                    |> Query.count (Expect.equal 0)
        , test "the evaluation drawer no longer asks for a ship's wheel" <|
            \_ ->
                screen I18n.En [ SelectTab Build.ReviewTab ]
                    |> Query.find [ Selector.attribute (attr "data-testid" "review-drawer") ]
                    |> Expect.all
                        [ Query.hasNot [ Selector.text "wheel" ]
                        , Query.has [ Selector.text "Your ship has no sails" ]
                        , Query.has [ Selector.text "Your ship has no guns" ]
                        ]
        ]


attr : String -> String -> Html.Attribute Never
attr =
    Html.Attributes.attribute


open : I18n.Language -> Base -> Build.Model
open lang base =
    Build.init
        { ldrawBase = ""
        , ldrawFallbackBase = ""
        , supportsPointerEvents = True
        , language = lang
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


{-| The (part, colour) placements in `doc` that are not in the starter `base`.
-}
added : Document -> Document -> List ( String, Int )
added base doc =
    let
        pairs d =
            Document.flattenedPlacements d |> List.map (\p -> ( p.part, p.color ))

        remove x xs =
            case xs of
                [] ->
                    []

                y :: rest ->
                    if x == y then
                        rest

                    else
                        y :: remove x rest
    in
    List.foldl remove (pairs doc) (pairs base)
