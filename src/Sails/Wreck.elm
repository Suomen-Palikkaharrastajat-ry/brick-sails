module Sails.Wreck exposing
    ( Piece
    , breakUp
    , pieceCount
    , step
    )

{-| A wrecked boat breaking up into its bricks.

When the hull gives out (`Sails.Hull.wrecked`) the boat is drawn no more;
in its place `pieceCount` loose bricks burst out of it, tumble through the
air, splash down and float, bobbing on the swell and slowly spreading out.

The pieces come from the boat's position and heading alone, so a wreck
looks the same every time it is replayed: there is no randomness.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Physics as Physics
import Sails.Types exposing (SailingState)


{-| One loose brick: where it is and how fast it moves, its spin (yaw and
tumble, in radians, and their rates), its part ("3001.dat" or "3003.dat")
and LDraw colour code, and whether it has landed on the water.
-}
type alias Piece =
    { position : Vec3
    , velocity : Vec3
    , yaw : Float
    , tumble : Float
    , spin : Float
    , part : String
    , colour : Int
    , afloat : Bool
    }


pieceCount : Int
pieceCount =
    18


{-| Classic brick colours of a toy boat: red, white, brown, tan, blue,
yellow and black.
-}
palette : List Int
palette =
    [ 4, 15, 70, 19, 1, 14, 15, 0 ]


{-| Burst a boat into its bricks. Pieces fly out all round, more of them
forwards (the boat was moving), up to about ten metres.
-}
breakUp : SailingState -> List Piece
breakUp state =
    let
        p =
            Vec3.toRecord state.position

        rad =
            degrees state.heading

        fwd =
            ( sin rad, cos rad )

        piece i =
            let
                fi =
                    toFloat i

                -- Golden-angle spread: evenly round, with no two the same.
                angle =
                    fi * 2.39996

                out =
                    2.5 + 3.5 * frac (fi * 0.618034)

                up =
                    4 + 4 * frac (fi * 0.4142)

                carry =
                    0.45 * state.speed

                ox =
                    sin angle

                oz =
                    cos angle

                height =
                    0.6 + 3.0 * frac (fi * 0.7071)
            in
            { position = vec3 (p.x + ox * 0.8) (p.y + height) (p.z + oz * 0.8)
            , velocity =
                vec3 (ox * out + Tuple.first fwd * carry)
                    up
                    (oz * out + Tuple.second fwd * carry)
            , yaw = angle
            , tumble = 0
            , spin = 3 + 4 * frac (fi * 0.3183)
            , part =
                if modBy 3 i == 0 then
                    "3001.dat"

                else
                    "3003.dat"
            , colour =
                List.drop (modBy (List.length palette) i) palette
                    |> List.head
                    |> Maybe.withDefault 15
            , afloat = False
            }
    in
    List.map piece (List.range 0 (pieceCount - 1))


frac : Float -> Float
frac x =
    x - toFloat (floor x)


{-| Move the pieces one step in a sea state `sea` at time `t`: through the
air under gravity, then floating on the water, drifting apart slowly.
-}
step : Float -> Float -> Float -> List Piece -> List Piece
step dt sea t pieces =
    List.map (stepPiece dt sea t) pieces


stepPiece : Float -> Float -> Float -> Piece -> Piece
stepPiece dt sea t piece =
    let
        p =
            Vec3.toRecord piece.position

        v =
            Vec3.toRecord piece.velocity
    in
    if piece.afloat then
        let
            drag =
                max 0 (1 - 0.6 * dt)

            nx =
                p.x + v.x * dt

            nz =
                p.z + v.z * dt
        in
        { piece
            | position = vec3 nx (Physics.seaElevation sea nx nz t) nz
            , velocity = vec3 (v.x * drag) 0 (v.z * drag)
            , yaw = piece.yaw + piece.spin * 0.08 * dt
            , tumble = piece.tumble * max 0 (1 - 3 * dt)
        }

    else
        let
            vy =
                v.y - 9.81 * dt

            nx =
                p.x + v.x * dt

            ny =
                p.y + vy * dt

            nz =
                p.z + v.z * dt

            water =
                Physics.seaElevation sea nx nz t
        in
        if ny <= water then
            { piece
                | position = vec3 nx water nz
                , velocity = vec3 (v.x * 0.3) 0 (v.z * 0.3)
                , afloat = True
            }

        else
            { piece
                | position = vec3 nx ny nz
                , velocity = vec3 v.x vy v.z
                , yaw = piece.yaw + piece.spin * dt
                , tumble = piece.tumble + piece.spin * 0.7 * dt
            }
