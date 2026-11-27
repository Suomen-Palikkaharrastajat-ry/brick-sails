module Sails.Wreck exposing
    ( Look(..)
    , Piece
    , Source(..)
    , breakUp
    , burst
    , partSlot
    , pieceCount
    , sourceScale
    , step
    )

{-| A wrecked boat breaking up into its own bricks.

When a boat's hull gives out (`Sails.Hull.wrecked`), a pirate ship sinks,
another player's ship is wrecked or a harbour pier is smashed (see
`Sails.Harbours`), it is drawn no more; in its place up to `pieceCount`
pieces burst out of it. Each one is a real part of that very model
(`ShipPart`: one of the bricks it was built from, drawn in its own colours,
see `Sails.ShipMesh.parts`), so a sunk frigate comes apart into frigate
bricks and a sloop into sloop bricks, at the size the ship was drawn
(`Source` carries the model's name and scale). A model with more parts
than `pieceCount` gives an even sample of them (`partSlot`). The bright
bursts of the finish and the ghost ship's are generic bricks (`Brick`). The pieces tumble through the air,
splash down and float, bobbing on the swell and slowly spreading out.

The pieces come from the boat's position and heading alone, so a wreck
looks the same every time it is replayed: there is no randomness.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Physics as Physics
import Sails.Types exposing (SailingState)


{-| What broke up, and so which model's parts its pieces are:

  - `Ship`: a ship drawn from a model looked up by `name` (the player's own
    boat or build, or a patrol ship's `npc:frigate` / `npc:sloop`), at
    `scale` times the boats' brick scale (`Sails.Pirates.shipScale`; 1 for
    the player's boat).
  - `RemoteShip`: another player's ship, by its vehicle id.
  - `GhostShip`: the spectral ghost ship (generic green bricks).
  - `HarbourPier`: a harbour's pier (`Sails.Scenery.pier`).

Its parts are looked up when the wreck is drawn.

-}
type Source
    = Ship { name : String, scale : Float }
    | RemoteShip String
    | GhostShip
    | HarbourPier


{-| The size a source's parts are drawn at, relative to the model's own
brick scale.
-}
sourceScale : Source -> Float
sourceScale source =
    case source of
        Ship ship ->
            ship.scale

        _ ->
            1


{-| What a piece looks like: one of the parts of a model (`ShipPart` the
model, and the piece's index, which `partSlot` turns into one of the model's
parts when drawn), or a generic brick of part "3001.dat" or "3003.dat" and
an LDraw colour code.
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


{-| The most pieces one wreck breaks into (the cap): a model with fewer
parts breaks into just its parts, one with more into an even sample of them.
-}
pieceCount : Int
pieceCount =
    20


{-| Which of a model's `partCount` parts piece number `index` is: every part
once for a small model (pieces past its last part show nothing), an even
spread over all of them for a model of more than `pieceCount` parts.
-}
partSlot : Int -> Int -> Maybe Int
partSlot partCount index =
    if index < 0 || index >= pieceCount || partCount <= 0 then
        Nothing

    else if partCount <= pieceCount then
        if index < partCount then
            Just index

        else
            Nothing

    else
        Just ((index * partCount + partCount // 2) // pieceCount)


{-| Burst a boat into its pieces. Pieces fly out all round, more of them
forwards (the boat was moving), up to about ten metres. Each piece is
another part of the boat, spread over all its parts.
-}
breakUp : Source -> SailingState -> List Piece
breakUp source state =
    burst source state.position state.heading state.speed


{-| Burst something at `position`, moving at `speed` (m/s) along compass
`heading` (degrees), into its pieces (see `breakUp`).
-}
burst : Source -> Vec3 -> Float -> Float -> List Piece
burst source position heading speed =
    let
        p =
            Vec3.toRecord position

        rad =
            degrees heading

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
                    0.45 * speed

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
            , look =
                case source of
                    GhostShip ->
                        Brick
                            (if modBy 2 i == 0 then
                                "3001.dat"

                             else
                                "3003.dat"
                            )
                            20

                    _ ->
                        ShipPart source i
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
