module Sails.Wreck exposing
    ( Look(..)
    , Piece
    , Source(..)
    , breakUp
    , pieceCount
    , step
    )

{-| A wrecked boat breaking up into its own bricks.

When a boat's hull gives out (`Sails.Hull.wrecked`), or a pirate ship sinks,
the boat is drawn no more; in its place `pieceCount` pieces burst out of it.
Each one is a real part of that boat (`ShipPart`: one of the bricks it was
built from, drawn in its own colours, see `Sails.ShipMesh.parts`), so the
wreck comes apart into the very bricks it was made of. The bright bursts of
the finish are generic bricks (`Brick`). The pieces tumble through the air,
splash down and float, bobbing on the swell and slowly spreading out.

The pieces come from the boat's position and heading alone, so a wreck
looks the same every time it is replayed: there is no randomness.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Physics as Physics
import Sails.Types exposing (SailingState)


{-| Whose boat broke up: the player's own, or a pirate ship. Its parts are
looked up when the wreck is drawn.
-}
type Source
    = OwnBoat
    | PirateShip


{-| What a piece looks like: one of the parts of a boat (`ShipPart` the
boat, and a slot that is taken modulo the boat's part count when drawn), or
a generic brick of part "3001.dat" or "3003.dat" and an LDraw colour code.
-}
type Look
    = ShipPart Source Int
    | Brick String Int


{-| One loose piece: where it is and how fast it moves, its spin (yaw and
tumble, in radians, and their rates), what it looks like, and whether it has
landed on the water.
-}
type alias Piece =
    { position : Vec3
    , velocity : Vec3
    , yaw : Float
    , tumble : Float
    , spin : Float
    , look : Look
    , afloat : Bool
    }


pieceCount : Int
pieceCount =
    18


{-| Burst a boat into its pieces. Pieces fly out all round, more of them
forwards (the boat was moving), up to about ten metres. Each piece is
another part of the boat, spread over all its parts.
-}
breakUp : Source -> SailingState -> List Piece
breakUp source state =
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
            , look = ShipPart source (floor (frac (fi * 0.7548776662) * 1000000))
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
