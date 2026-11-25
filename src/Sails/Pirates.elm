module Sails.Pirates exposing
    ( Pirate
    , boardingCooldown
    , boardingRange
    , fleet
    , fleetSize
    , harbourClearance
    , patrolRoutes
    , shipName
    , shipSlot
    , stateAt
    )

{-| Pirate ships: black-sailed schooners that patrol the course.

Each pirate sails round and round a small circle centred on the middle of
one leg between two harbours, so it crosses the route twice a lap and the
player meets it on the way. Which legs, where on the circle each starts,
which way round and how fast all come from a seed (the race's own seed:
the course and run, or the room), and its position is a function of time
alone, so the same seed always gives the same pirates in the same places.

  - A circle is only used when it keeps `harbourClearance` metres clear of
    every harbour's mooring circle and the start, clear of the islands, and
    a few metres off the floating targets: a pirate never sits on a harbour,
    so the course can always be finished.
  - Pirates do not push the boat about. A pirate the boat comes within
    `boardingRange` of boards it: the hull takes one cannon hit's damage,
    then no pirate can board again for `boardingCooldown` seconds.
  - A cannon ball that hits a pirate sinks it for the rest of the run, and
    counts as a target hit (the same time bonus).

-}

import Math.Vector3 as Vec3 exposing (vec3)
import Sails.Physics as Physics
import Sails.Types exposing (Course, SailingState)


{-| A pirate's patrol: its id, the centre and radius of its circle, the
seconds a lap takes, where on the circle it is at time 0 (a share of a
lap) and which way round it sails (1 clockwise seen from above, -1 the
other way).
-}
type alias Pirate =
    { id : Int
    , centreX : Float
    , centreZ : Float
    , radius : Float
    , lapSeconds : Float
    , phase : Float
    , direction : Float
    }


{-| How many pirates a race has (fewer if the course has fewer legs with
room for a patrol).
-}
fleetSize : Int
fleetSize =
    3


{-| The built-in boat a pirate is drawn as (with black sails).
-}
shipName : String
shipName =
    "Schooner"


{-| Metres a pirate's patrol keeps clear of every harbour's mooring circle.
-}
harbourClearance : Float
harbourClearance =
    8


{-| How close (m) a pirate comes before it boards the boat.
-}
boardingRange : Float
boardingRange =
    5


{-| Seconds after a boarding before a pirate can board again.
-}
boardingCooldown : Float
boardingCooldown =
    5


{-| The cannon slot of pirate `id`, apart from the players' slots (1 to 8),
for `Sails.Physics.checkShipHits`.
-}
shipSlot : Int -> Int
shipSlot id =
    100 + id


patrolRadius : Float
patrolRadius =
    9


{-| Every patrol circle the course has room for, one per leg between
harbours (no lap timing yet: id, centre and radius only), in course order.
-}
patrolRoutes : Course -> List Pirate
patrolRoutes course =
    let
        harbours =
            List.map .position course.waypoints

        legs =
            List.map2 Tuple.pair harbours (List.drop 1 harbours)

        route i ( a, b ) =
            let
                mid =
                    Vec3.scale 0.5 (Vec3.add a b)
                        |> Vec3.toRecord
            in
            { id = i
            , centreX = mid.x
            , centreZ = mid.z
            , radius = patrolRadius
            , lapSeconds = 12
            , phase = 0
            , direction = 1
            }
    in
    List.indexedMap route legs
        |> List.filter (isClear course)


{-| Whether a patrol circle stays clear of the harbours, the start, the
islands and the targets.
-}
isClear : Course -> Pirate -> Bool
isClear course p =
    let
        fromCentre x z =
            sqrt ((x - p.centreX) ^ 2 + (z - p.centreZ) ^ 2)

        gap pos =
            let
                r =
                    Vec3.toRecord pos
            in
            fromCentre r.x r.z - p.radius

        offTarget pos =
            let
                r =
                    Vec3.toRecord pos
            in
            abs (fromCentre r.x r.z - p.radius) >= 4
    in
    List.all (\w -> gap w.position >= w.radius + harbourClearance) course.waypoints
        && (gap course.startPosition >= Physics.harbourRadius + harbourClearance)
        && List.all (\i -> fromCentre i.x i.z - p.radius >= i.radius + 4) course.islands
        && List.all (.position >> offTarget) course.targets


{-| The pirates of a race with this seed: `fleetSize` of the course's
patrol routes, each with its own start, way round and lap time.
-}
fleet : Int -> Course -> List Pirate
fleet seed course =
    let
        routes =
            patrolRoutes course

        pick ( chosen, available, s ) =
            if List.length chosen >= fleetSize || List.isEmpty available then
                ( chosen, available, s )

            else
                let
                    ( r, s1 ) =
                        random s

                    i =
                        min (List.length available - 1) (floor (r * toFloat (List.length available)))

                    ( phase, s2 ) =
                        random s1

                    ( way, s3 ) =
                        random s2

                    ( pace, s4 ) =
                        random s3
                in
                case List.drop i available of
                    route :: _ ->
                        pick
                            ( { route
                                | phase = phase
                                , direction =
                                    if way < 0.5 then
                                        1

                                    else
                                        -1
                                , lapSeconds = 11 + 3 * pace
                              }
                                :: chosen
                            , List.take i available ++ List.drop (i + 1) available
                            , s4
                            )

                    [] ->
                        ( chosen, available, s4 )

        ( picked, _, _ ) =
            pick ( [], routes, clamp 1 (modulus - 1) seed )
    in
    List.sortBy .id picked


{-| Where pirate `p` is, and which way it sails, at race time `t` (s), on
the sea surface `y` = 0 (the view lifts it onto the waves).
-}
stateAt : Float -> Pirate -> SailingState
stateAt t p =
    let
        angle =
            2 * pi * (p.phase + p.direction * t / p.lapSeconds)

        x =
            p.centreX + p.radius * sin angle

        z =
            p.centreZ + p.radius * cos angle

        -- The way along the circle: the derivative of the position.
        dx =
            p.direction * cos angle

        dz =
            -p.direction * sin angle
    in
    { position = vec3 x 0 z
    , heading = atan2 dx dz * 180 / pi
    , speed = 2 * pi * p.radius / p.lapSeconds
    , steer = 0
    , sailPower = 1
    , sailAngle = 30
    , heel = -8 * p.direction
    , pitch = 0
    , bumped = False
    , sail = 1
    }


modulus : Int
modulus =
    2147483647


random : Int -> ( Float, Int )
random seed =
    let
        next =
            modBy modulus (seed * 16807)
    in
    ( toFloat (next - 1) / toFloat (modulus - 1), next )
