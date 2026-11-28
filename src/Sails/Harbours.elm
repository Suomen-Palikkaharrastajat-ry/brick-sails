module Sails.Harbours exposing
    ( Damage
    , Footprint
    , checkHits
    , collide
    , dockIsland
    , footprint
    , footprints
    , hitsLeft
    , hitsToDestroy
    , intactFootprints
    , isWrecked
    , ramSpeed
    , recordHits
    , skipWrecked
    , smash
    , wreck
    )

{-| The harbours' piers are solid, and can be smashed.

Every harbour on the course (`Sails.Types.Waypoint`) has a pier
(`Sails.Scenery.pier`) running from the beach of the island it docks at out
through the harbour, as `Sails.Render` draws it. Its `footprint` is the box
of that pier out on the water: `pierWidth` across, from just inside the beach
to the pier's head.

  - A ship that runs into an intact pier slower than `ramSpeed` stops dead
    against it: it is pushed back out of the box along the box's normal
    (never further in, so it is never trapped) and its speed is set to 0.
    Mooring alongside works as before (`Sails.Physics.stepProgress`).
  - A ship that runs into it at `ramSpeed` or faster smashes the pier: it
    bursts into its own bricks (`smash`, `Sails.Wreck`) and the ship sails
    on at half speed. The harbour the ship is to dock at next is the
    exception: it only stops the ship, so coming in fast never costs the
    harbour the boat is mooring at.
  - The player's cannon balls hit a pier too (`checkHits`); after
    `hitsToDestroy` hits it is smashed the same way.

A smashed harbour (`isWrecked`) is no longer solid and can no longer be
docked at, and it is not rebuilt until the course is sailed again (a
restart or a new course start with every pier whole). The voyage does not
get stuck on it: when the harbour the boat is to dock at next is smashed,
`skipWrecked` marks that leg done (its split is taken, without the
harbour's coins) and the boat sails on to the next harbour; smashing the
final harbour ends the voyage.

-}

import Dict exposing (Dict)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Physics as Physics
import Sails.Structures as Structures
import Sails.Types exposing (Cannonball, CombatImpact, Course, CourseProgress, Island, SailingState, Waypoint)
import Sails.Wreck as Wreck


{-| Cannon ball hits taken, by harbour (waypoint) index.
-}
type alias Damage =
    Dict Int Int


{-| A pier's solid box on the water: its centre (`x`, `z`), the compass
direction it runs out along (`dirX`, `dirZ`, a unit vector from the island
outwards), and its half length along that direction and half width across
it, in metres.
-}
type alias Footprint =
    { x : Float
    , z : Float
    , dirX : Float
    , dirZ : Float
    , halfLength : Float
    , halfWidth : Float
    }


{-| Speed (m/s) at or above which a ship running into a pier smashes it:
about a boat's cruising speed under full sail (the boats' top speeds are
7.4 to 10.2 m/s). The harbour the boat docks at next is never rammed down
(see `collide`).
-}
ramSpeed : Float
ramSpeed =
    7.5


{-| Cannon ball hits that smash a pier: the `HarbourPier` entry of the
structures' durability table (`Sails.Structures.durability`).
-}
hitsToDestroy : Int
hitsToDestroy =
    Structures.durability Structures.HarbourPier


{-| A pier's width across (m): four studs and its round pilings.
-}
pierWidth : Float
pierWidth =
    2.8


{-| Where the pier starts and ends, in metres along its bearing from the
island's shore: `Sails.Render.pierInView` sets it 5 m inside the shore, and
it is 320 LDU (11.2 m) long, so its head stands 6.2 m out on the water. The
box starts 1 m inside the shore, which the island's own rock already covers.
-}
pierInner : Float
pierInner =
    -1


pierOuter : Float
pierOuter =
    6.2


{-| How far (m) round a pier's box a cannon ball still hits it, and how high
over the water (the deck, its bollards and lanterns).
-}
ballMargin : Float
ballMargin =
    0.5


pierTop : Float
pierTop =
    3.5


{-| The island a harbour at `p` docks at: the one whose shore is nearest.
-}
dockIsland : List Island -> { x : Float, y : Float, z : Float } -> Maybe Island
dockIsland islands p =
    islands
        |> List.sortBy (\isl -> sqrt ((p.x - isl.x) ^ 2 + (p.z - isl.z) ^ 2) - isl.radius)
        |> List.head


{-| The pier box of a harbour on a course with these islands.
-}
footprint : List Island -> Waypoint -> Maybe Footprint
footprint islands wp =
    let
        p =
            Vec3.toRecord wp.position
    in
    dockIsland islands p
        |> Maybe.map
            (\isl ->
                let
                    bearing =
                        atan2 (p.x - isl.x) (p.z - isl.z)

                    dirX =
                        sin bearing

                    dirZ =
                        cos bearing

                    mid =
                        isl.radius + 0.5 * (pierInner + pierOuter)
                in
                { x = isl.x + dirX * mid
                , z = isl.z + dirZ * mid
                , dirX = dirX
                , dirZ = dirZ
                , halfLength = 0.5 * (pierOuter - pierInner)
                , halfWidth = 0.5 * pierWidth
                }
            )


{-| Every harbour's pier box, by harbour index.
-}
footprints : Course -> List ( Int, Footprint )
footprints course =
    List.filterMap (\wp -> footprint course.islands wp |> Maybe.map (Tuple.pair wp.index)) course.waypoints


{-| The piers still standing.
-}
intactFootprints : Damage -> Course -> List ( Int, Footprint )
intactFootprints damage course =
    List.filter (\( i, _ ) -> not (isWrecked damage i)) (footprints course)


{-| Whether harbour `index` has been smashed.
-}
isWrecked : Damage -> Int -> Bool
isWrecked damage index =
    Dict.get index damage |> Maybe.withDefault 0 |> (\hits -> hits >= hitsToDestroy)


{-| Hits a harbour can still take before it is smashed.
-}
hitsLeft : Damage -> Int -> Int
hitsLeft damage index =
    max 0 (hitsToDestroy - (Dict.get index damage |> Maybe.withDefault 0))


{-| Smash harbour `index` at once (a ship rammed it).
-}
wreck : Int -> Damage -> Damage
wreck index damage =
    Dict.insert index hitsToDestroy damage


{-| Count cannon ball hits on harbours (one index per ball). Returns the
damage after them and the harbours they smashed.
-}
recordHits : List Int -> Damage -> ( Damage, List Int )
recordHits hits damage =
    List.foldl
        (\i ( d, smashed ) ->
            if isWrecked d i then
                ( d, smashed )

            else
                let
                    after =
                        Dict.insert i ((Dict.get i d |> Maybe.withDefault 0) + 1) d
                in
                if isWrecked after i then
                    ( after, smashed ++ [ i ] )

                else
                    ( after, smashed )
        )
        ( damage, [] )
        hits


{-| A point in a pier's own frame: metres along it (out from the island)
and across it.
-}
toLocal : Footprint -> Float -> Float -> ( Float, Float )
toLocal fp px pz =
    let
        dx =
            px - fp.x

        dz =
            pz - fp.z
    in
    ( dx * fp.dirX + dz * fp.dirZ, dx * fp.dirZ - dz * fp.dirX )


toWorld : Footprint -> ( Float, Float ) -> ( Float, Float )
toWorld fp ( u, v ) =
    ( fp.x + u * fp.dirX + v * fp.dirZ, fp.z + u * fp.dirZ - v * fp.dirX )


{-| Where a ship of radius `radius` at (`px`, `pz`) must be moved to stand
clear of a pier, if it overlaps it: out along the normal of the nearest
side of the box (or end of it, but never towards the island).
-}
pushOut : Float -> Footprint -> Float -> Float -> Maybe ( Float, Float )
pushOut radius fp px pz =
    let
        ( u, v ) =
            toLocal fp px pz

        cu =
            clamp -fp.halfLength fp.halfLength u

        cv =
            clamp -fp.halfWidth fp.halfWidth v

        du =
            u - cu

        dv =
            v - cv

        d =
            sqrt (du * du + dv * dv)

        sign x =
            if x < 0 then
                -1

            else
                1
    in
    if d >= radius then
        Nothing

    else if d > 0.001 then
        Just (toWorld fp ( cu + du / d * radius, cv + dv / d * radius ))

    else
        -- The ship's centre is inside the box: out the nearest side, or off
        -- the pier's head when that is nearer.
        let
            acrossDepth =
                fp.halfWidth - abs v

            headDepth =
                fp.halfLength - u
        in
        if headDepth < acrossDepth then
            Just (toWorld fp ( fp.halfLength + radius, v ))

        else
            Just (toWorld fp ( u, sign v * (fp.halfWidth + radius) ))


{-| Run a ship (of `Sails.Physics.boatRadius`) against the standing piers.
Slower than `ramSpeed` it stops dead against a pier it touches, pushed back
out; at `ramSpeed` or faster it smashes the pier and sails on at half
speed. The harbour the ship is to dock at next (`docking`) is never rammed
down: the ship only stops against it, at any speed, so a boat that comes in
under full sail can still moor there. Returns the ship and the harbours it
smashed.
-}
collide : Maybe Int -> List ( Int, Footprint ) -> SailingState -> ( SailingState, List Int )
collide docking piers ship =
    List.foldl
        (\( i, fp ) ( s, rammed ) ->
            let
                p =
                    Vec3.toRecord s.position
            in
            case pushOut Physics.boatRadius fp p.x p.z of
                Just ( nx, nz ) ->
                    if s.speed >= ramSpeed && docking /= Just i then
                        ( { s | speed = s.speed * 0.5, bumped = True }, rammed ++ [ i ] )

                    else
                        ( { s | position = vec3 nx p.y nz, speed = 0, bumped = True }, rammed )

                Nothing ->
                    ( s, rammed )
        )
        ( ship, [] )
        piers


{-| Cannon balls hitting the standing piers: a ball within `ballMargin` of a
pier's box, below its top, hits it and is gone. Returns the balls still
flying, their hit flashes and the harbour hit by each ball that hit.
-}
checkHits : List ( Int, Footprint ) -> List Cannonball -> ( List Cannonball, List CombatImpact, List Int )
checkHits piers balls =
    let
        hits ball ( _, fp ) =
            let
                b =
                    Vec3.toRecord ball.position

                ( u, v ) =
                    toLocal fp b.x b.z
            in
            abs u <= fp.halfLength + ballMargin && abs v <= fp.halfWidth + ballMargin && b.y <= pierTop

        step ball ( alive, flashes, hit ) =
            case List.filter (hits ball) piers |> List.head of
                Just ( i, _ ) ->
                    ( alive, { id = ball.id, position = ball.position, age = 0, isHit = True } :: flashes, hit ++ [ i ] )

                Nothing ->
                    ( alive ++ [ ball ], flashes, hit )
    in
    List.foldl step ( [], [], [] ) balls


{-| A smashed pier bursting into its own bricks (`Sails.Wreck`), carried on
along compass `heading` at `speed` (a ship ramming it; 0 for a shot).
-}
smash : Footprint -> Float -> Float -> List Wreck.Piece
smash fp heading speed =
    Wreck.burst Wreck.HarbourPier (vec3 fp.x 0 fp.z) heading speed


{-| Mark smashed harbours done on the way: while the harbour the boat is to
dock at next is smashed, its leg is done (its split taken, no coins for it)
and the next harbour is the one to dock at; past the last one the voyage is
finished. Returns the progress and the harbours skipped.
-}
skipWrecked : Damage -> CourseProgress -> ( CourseProgress, List Int )
skipWrecked damage progress =
    if progress.finished || not (isWrecked damage progress.currentWaypointIndex) then
        ( progress, [] )

    else
        let
            next =
                progress.currentWaypointIndex + 1

            ( after, skipped ) =
                skipWrecked damage
                    { progress
                        | currentWaypointIndex = next
                        , splits = progress.splits ++ [ progress.elapsedSeconds ]
                        , finished = next >= progress.totalWaypoints
                        , berthSeconds = 0
                    }
        in
        ( after, progress.currentWaypointIndex :: skipped )
