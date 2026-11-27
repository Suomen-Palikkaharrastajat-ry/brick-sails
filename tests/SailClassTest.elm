module SailClassTest exposing (suite)

{-| Tests of the game's one sail rule (`Sails.Ingest.sailRule`): which parts
of a ship go into the sails scene, to be lowered and raised with the sail
level. White 1 x N bricks above the deck and flags are sails; white bricks
down in the hull are not; the NPC frigate and sloop have their white square
sails taken as sails.
-}

import Expect
import LDraw.Document as Document exposing (Placement)
import Sails.Ingest as Ingest
import Sails.NpcShips as NpcShips
import Test exposing (Test, describe, test)


{-| A starter-style hull, one brick and a plate tall (keel bricks with their
tops at y = 0, a deck plate on top at y = -8), and the extra lines.
-}
onHull : List String -> List Placement
onHull extra =
    ([ "0 FILE test-ship.ldr"
     , "1 70 0 0 -50 1 0 0 0 1 0 0 0 1 3660b.dat"
     , "1 70 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
     , "1 70 0 0 60 1 0 0 0 1 0 0 0 1 3003.dat"
     , "1 15 0 -8 0 1 0 0 0 1 0 0 0 1 3034.dat"
     ]
        ++ extra
    )
        |> String.join "\n"
        |> Document.parse
        |> Document.flattenedPlacements


{-| The sail rule's verdict on the placements of a part, by part file.
-}
verdictsFor : String -> List Placement -> List Bool
verdictsFor part placements =
    let
        isSail =
            Ingest.sailRule placements
    in
    placements
        |> List.filter (\p -> p.part == part)
        |> List.map isSail


npcSails : String -> List Placement
npcSails text =
    Document.flattenedPlacements (Document.parse text)


suite : Test
suite =
    describe "Sail rule"
        [ test "a white 1 x 3 brick standing on the deck is a sail" <|
            \_ ->
                onHull [ "1 15 0 -32 0 1 0 0 0 1 0 0 0 1 3622.dat" ]
                    |> verdictsFor "3622.dat"
                    |> Expect.equal [ True ]
        , test "white 1 x 3 bricks stacked above the deck are all sails" <|
            \_ ->
                onHull
                    [ "1 15 0 -32 0 1 0 0 0 1 0 0 0 1 3622.dat"
                    , "1 15 0 -56 0 1 0 0 0 1 0 0 0 1 3622.dat"
                    , "1 15 0 -80 0 1 0 0 0 1 0 0 0 1 3622.dat"
                    ]
                    |> verdictsFor "3622.dat"
                    |> Expect.equal [ True, True, True ]
        , test "a white brick at deck level, in the hull, is hull" <|
            \_ ->
                onHull [ "1 15 30 0 0 1 0 0 0 1 0 0 0 1 3622.dat" ]
                    |> verdictsFor "3622.dat"
                    |> Expect.equal [ False ]
        , test "a white plate (the deck) is hull" <|
            \_ ->
                onHull []
                    |> verdictsFor "3034.dat"
                    |> Expect.equal [ False ]
        , test "a coloured 1 x 3 brick above the deck is not a sail" <|
            \_ ->
                onHull [ "1 4 0 -32 0 1 0 0 0 1 0 0 0 1 3622.dat" ]
                    |> verdictsFor "3622.dat"
                    |> Expect.equal [ False ]
        , test "a flag is still a sail" <|
            \_ ->
                onHull [ "1 4 0 -100 0 1 0 0 0 1 0 0 0 1 2335.dat" ]
                    |> verdictsFor "2335.dat"
                    |> Expect.equal [ True ]
        , test "a build of white bricks only has no white sails" <|
            \_ ->
                [ "0 FILE white.ldr"
                , "1 15 0 0 0 1 0 0 0 1 0 0 0 1 3622.dat"
                , "1 15 0 -24 0 1 0 0 0 1 0 0 0 1 3622.dat"
                ]
                    |> String.join "\n"
                    |> Document.parse
                    |> Document.flattenedPlacements
                    |> verdictsFor "3622.dat"
                    |> Expect.equal [ False, False ]
        , describe "NPC ships"
            (List.concatMap
                (\( name, text ) ->
                    let
                        placements =
                            npcSails text

                        isSail =
                            Ingest.sailRule placements

                        white =
                            List.filter (\p -> p.color == NpcShips.sailColour) placements
                    in
                    [ test (name ++ ": every white sail brick is a sail") <|
                        \_ ->
                            Expect.all
                                [ \ws -> Expect.greaterThan 4 (List.length ws)
                                , \ws -> Expect.equal [] (List.filter (not << isSail) ws |> List.map .part)
                                ]
                                white
                    , test (name ++ ": the hull and rig are not sails") <|
                        \_ ->
                            placements
                                |> List.filter (\p -> p.color /= NpcShips.sailColour && not (Ingest.isSailPart p.part))
                                |> List.filter isSail
                                |> List.map .part
                                |> Expect.equal []
                    , test (name ++ ": the flags are sails") <|
                        \_ ->
                            placements
                                |> List.filter (\p -> p.part == "2335.dat")
                                |> List.map isSail
                                |> List.all identity
                                |> Expect.equal True
                    ]
                )
                [ ( "frigate", NpcShips.frigate ), ( "sloop", NpcShips.sloop ) ]
            )
        ]
