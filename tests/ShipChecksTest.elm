module ShipChecksTest exposing (suite)

{-| The ship editor's build checks (`Pages.Build.shipChecks`): a new ship on a
starter hull is prompted for a mast, a sail and a gun; each prompt goes away
once that kind of part is on board, and each helper button adds the part
that clears it (a mast `3957a`, a white 1 x 3 brick sail `3622`, a cannon
`32828`). A sail that stands on nothing (moved off the ship, with no part
under it) is warned about, without a helper, until it is put on something.
The builder screen shows the prompts, in English and Finnish, and no longer
asks for a ship's wheel.
-}

import Expect
import Html
import Html.Attributes
import I18n
import LDraw.Document as Document exposing (Document)
import Math.Matrix4 as Mat4
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
                checkTests base :: floatingSailTests base :: viewTests base :: roundBrickMastTests :: List.map starterTest Editor.bases

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
                List.concatMap fix [ NoMast, NoSail, NoGuns ]
                    |> List.foldl step (open I18n.En base)
                    |> Build.document
                    |> Build.shipChecks
                    |> Expect.equal []
        , test "the sail helper alone clears the sail prompt" <|
            \_ ->
                List.foldl step (open I18n.En base) (fix NoSail)
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
                    (added base.document (after (fix NoSail)))
        , test "the sail helper adds a white brick even when another colour is picked" <|
            \_ ->
                Expect.equal [ ( "3622.dat", 15 ) ]
                    (added base.document (after (PickColour 4 :: fix NoSail)))
        , test "a white sail brick on the deck clears the sail prompt only" <|
            \_ ->
                Expect.equal [ NoMast, NoGuns ] (Build.shipChecks (after (fix NoSail)))
        , test "the white sail brick passes the game's sail rule" <|
            \_ ->
                let
                    doc =
                        after (fix NoSail)

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
                    (added base.document (after (fix NoGuns)) |> List.map Tuple.first)
        , test "a 32828 cannon clears the gun prompt only" <|
            \_ ->
                Expect.equal [ NoMast, NoSail ] (Build.shipChecks (after (fix NoGuns)))
        , test "the mast helper adds one 3957a mast" <|
            \_ ->
                Expect.equal [ "3957a.dat" ]
                    (added base.document (after (fix NoMast)) |> List.map Tuple.first)
        , test "a mast clears the mast prompt only" <|
            \_ ->
                Expect.equal [ NoSail, NoGuns ] (Build.shipChecks (after (fix NoMast)))
        , test "the editor's mast pillar counts as a mast" <|
            \_ ->
                Expect.equal False (List.member NoMast (Build.shipChecks (after [ AddPart "43888.dat" ])))
        , test "with a mast, a sail and a cannon there are no prompts" <|
            \_ ->
                Expect.equal []
                    (Build.shipChecks
                        (after (List.concatMap fix [ NoMast, NoSail, NoGuns ]))
                    )
        , test "a ship's wheel is not asked for and clears nothing" <|
            \_ ->
                Expect.equal [ NoMast, NoSail, NoGuns ] (Build.shipChecks (after [ AddPart "3829c01.dat" ]))
        ]


floatingSailTests : Base -> Test
floatingSailTests base =
    let
        after msgs =
            List.foldl step (open I18n.En base) msgs |> Build.document

        rigged =
            after (fix NoMast ++ fix NoSail)

        floating =
            moveSail (Mat4.makeTranslate3 1000 0 0) rigged

        isSail doc =
            Ingest.sailRule (Document.flattenedPlacements doc)

        sailCount doc =
            Document.flattenedPlacements doc |> List.filter (isSail doc) |> List.length
    in
    describe "floating sails"
        [ test "a white sail stacked on the deck has no warning" <|
            \_ ->
                let
                    doc =
                        after (fix NoSail ++ fix NoSail)
                in
                Expect.all
                    [ \d -> Expect.equal 2 (sailCount d)
                    , \d -> Expect.equal False (List.member UnsupportedSail (Build.shipChecks d))
                    ]
                    doc
        , test "a mast and a white sail from the helpers have no warning" <|
            \_ -> Expect.equal [ NoGuns ] (Build.shipChecks rigged)
        , test "the first ship (a flag on a mast) has no warning" <|
            \_ ->
                case Editor.firstShip of
                    Just ( _, doc ) ->
                        Expect.equal False (List.member UnsupportedSail (Build.shipChecks doc))

                    Nothing ->
                        Expect.fail "no first ship"
        , test "a white sail moved off the mast column, with nothing under it, is warned about" <|
            \_ ->
                Expect.all
                    [ \d -> Expect.equal 1 (sailCount d)
                    , \d -> Expect.equal [ NoGuns, UnsupportedSail ] (Build.shipChecks d)
                    ]
                    floating
        , test "the warning clears once the sail is put back on the deck" <|
            \_ ->
                floating
                    |> moveSail (Mat4.makeTranslate3 -1000 0 0)
                    |> Build.shipChecks
                    |> Expect.equal [ NoGuns ]
        , test "the warning clears once a brick is put under the sail" <|
            \_ ->
                let
                    sailMatrix =
                        Document.flattenedPlacements floating
                            |> List.filter (\p -> p.part == "3622.dat")
                            |> List.map .matrix
                            |> List.head
                            |> Maybe.withDefault Mat4.identity
                in
                floating
                    |> Document.addPlacement (Document.mainSubmodel floating).name
                        { part = "3005.dat", color = 4, matrix = Mat4.mul (Mat4.makeTranslate3 0 24 0) sailMatrix }
                    |> Tuple.first
                    |> Build.shipChecks
                    |> Expect.equal [ NoGuns ]
        , test "the warning has no helper button" <|
            \_ -> Expect.equal Nothing (Build.shipCheckFix UnsupportedSail)
        ]


{-| A player's ship (a reported build link) on the narrow racing hull: a
jumper plate on the deck carrying a mast of three stacked 1 x 1 round bricks
(`3062b`), a 2 x 3 plate yard on top, two white 1 x 3 brick sails hanging
under the yard and three 32828 cannons. It has a mast, so it is not asked for
one.
-}
roundBrickMastLink : String
roundBrickMastLink =
    "http://localhost:8181/#map=desert-summer-clear-1&b/bc4xDoJAEAXQP-yOYiwgAWtLK3TZDcsarSw4hyB0RpO18BLexWN4LRcrC6Z4U_zk558aiherW-v9_Vn49jL4omsfw_bae3Z15RQbZXUnjdKlNFZrNtppJ029r9hYq8ZIlZyiQQIgwzI4HhGwzqLgDv-KYJLJCEh_KrBAfn4RC9rgE57I33Sk6VGAwEyK0DAPPekhJgH0Xw"


roundBrickMastTests : Test
roundBrickMastTests =
    let
        withRoundBricks n =
            Document.parse
                (String.join "\n"
                    ([ "1 70 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                     , "1 15 0 -24 0 1 0 0 0 1 0 0 0 1 3622.dat"
                     ]
                        ++ List.map (\i -> "1 70 40 " ++ String.fromInt (-24 * (i + 1)) ++ " 0 1 0 0 0 1 0 0 0 1 3062b.dat") (List.range 0 (n - 1))
                    )
                )
    in
    describe "masts of 1 x 1 round bricks"
        [ test "the reported ship with a round brick mast and sails is not asked for a mast" <|
            \_ ->
                case Ingest.decodeShipInput Nothing roundBrickMastLink of
                    Ok ship ->
                        Expect.equal [] (Build.shipChecks ship.document)

                    Err err ->
                        Expect.fail err
        , test "two stacked 1 x 1 round bricks count as a mast" <|
            \_ -> Expect.equal False (List.member NoMast (Build.shipChecks (withRoundBricks 2)))
        , test "a single 1 x 1 round brick is not a mast" <|
            \_ -> Expect.equal True (List.member NoMast (Build.shipChecks (withRoundBricks 1)))
        ]


{-| Move the main model's white 1 x 3 sail brick by `move` (LDraw).
-}
moveSail : Mat4.Mat4 -> Document -> Document
moveSail move doc =
    case Document.mainPlacements doc |> List.filter (\p -> p.part == "3622.dat") of
        sail :: _ ->
            Document.updatePlacement sail.id (\p -> { p | matrix = Mat4.mul move p.matrix }) doc

        [] ->
            doc


viewTests : Base -> Test
viewTests base =
    let
        screen lang msgs =
            List.foldl step (open lang base) msgs |> Build.view |> Query.fromHtml

        floatingScreen lang =
            let
                model =
                    List.foldl step (open lang base) (fix NoMast ++ fix NoSail)
            in
            Build.init
                { ldrawBase = ""
                , ldrawFallbackBase = ""
                , supportsPointerEvents = True
                , language = lang
                , width = 390
                , height = 844
                }
                base
                (moveSail (Mat4.makeTranslate3 1000 0 0) (Build.document model))
                |> Tuple.first
                |> Build.view
                |> Query.fromHtml
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
                screen I18n.En (fix NoSail)
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
                screen I18n.En (AddPart "3957a.dat" :: fix NoSail ++ [ AddPart "32828.dat" ])
                    |> Query.findAll [ Selector.attribute (attr "data-testid" "builder-checks") ]
                    |> Query.count (Expect.equal 0)
        , test "a floating sail shows the warning, in English, without a button" <|
            \_ ->
                floatingScreen I18n.En
                    |> Query.find [ Selector.attribute (attr "data-testid" "builder-check-sail-support") ]
                    |> Expect.all
                        [ Query.has [ Selector.text "A sail is not on anything" ]
                        , Query.has [ Selector.attribute (Html.Attributes.title "Put each sail on the deck or a mast so it has something to stand on.") ]
                        , Query.findAll [ Selector.tag "button" ] >> Query.count (Expect.equal 0)
                        ]
        , test "the floating sail warning is in Finnish too" <|
            \_ ->
                floatingScreen I18n.Fi
                    |> Query.find [ Selector.attribute (attr "data-testid" "builder-check-sail-support") ]
                    |> Expect.all
                        [ Query.has [ Selector.text "Purje ei ole minkään päällä" ]
                        , Query.has [ Selector.attribute (Html.Attributes.title "Aseta jokainen purje kannelle tai mastoon, että sillä on tuki.") ]
                        ]
        , test "a ship whose sails stand on something shows no floating sail warning" <|
            \_ ->
                screen I18n.En (AddPart "3957a.dat" :: fix NoSail)
                    |> Query.findAll [ Selector.attribute (attr "data-testid" "builder-check-sail-support") ]
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


{-| The helper button's message for a check, if it has one.
-}
fix : ShipCheck -> List Msg
fix check =
    Build.shipCheckFix check |> Maybe.map List.singleton |> Maybe.withDefault []


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
