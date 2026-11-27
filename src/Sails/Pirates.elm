module Sails.Pirates exposing
    ( Pirate
    , boardingCooldown
    , boardingRange
    , collisionRadius
    , fleet
    , fleetSize
    , harbourClearance
    , isPirateSlot
    , patrolRoutes
    , reach
    , reloadSeconds
    , sailTint
    , shipName
    , shipNameFor
    , shipSlot
    , stateAt
    )

{-| Pirate and patrol ships: three faction NPC vessels that patrol the course.

Each ship sails round and round a small circle centred on the middle of one leg
between two harbours, so it crosses the route twice a lap and the player meets
it on the way. Each faction (Pirates, BlueCoats, RedCoats) has a patrol ship
assigned to the leg closest to its island, with faction-specific sail tints and
vessel models. Where on the circle each starts, which way round and how fast all
come from a seed.

  - A circle is only used when it keeps `harbourClearance` metres clear of
    every harbour's mooring circle and the start, clear of the islands, and
    a few metres off the floating targets: a patrol ship never sits on a harbour,
    so the course can always be finished.
  - A patrol ship is a ship of its own size: the boat cannot sail through it, and
    bumps off it as it does off an island (`collisionRadius`).
  - A patrol ship the boat comes within `boardingRange` of boards it: the hull
    takes one cannon hit's damage (the crew fights the boarders off and wins
    `Sails.Coins.pirateBoarded` coins), then no ship can board again for
    `boardingCooldown` seconds.
  - Patrol ships shoot back. A ship within `reach` of the boat fires a ball at
    it every `reloadSeconds`; a ball that hits takes one cannon hit's damage.
  - A cannon ball that hits a patrol ship sinks it for the rest of the run,
    counts as a target hit and wins `Sails.Coins.pirateSunk` coins.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.NpcShips as NpcShips
import Sails.Physics as Physics
import Sails.Types exposing (Course, Faction(..), SailingState)


{-| A pirate's patrol: its id, the centre and radius of its circle, the
seconds a lap takes, where on the circle it is at time 0 (a share of a
lap), which way round it sails (1 clockwise seen from above, -1 the other
way) and its faction.
-}
type alias Pirate =
    { id : Int
    , centreX : Float
    , centreZ : Float
    , radius : Float
    , lapSeconds : Float
    , phase : Float
    , direction : Float
    , faction : Faction
    }


{-| How many pirates a race has (fewer if the course has fewer legs with
room for a patrol).
-}
fleetSize : Int
fleetSize =
    3


{-| Radius (m) of a pirate ship for collisions with the boat: the boat
bumps off it at this distance from the pirate's centre.
-}
collisionRadius : Float
collisionRadius =
    2.3


{-| How close (m) the boat must be for a pirate to fire at it.
-}
reach : Float
reach =
    24


{-| Seconds between one pirate's shots. Each pirate has its own pace, so
they do not all fire together.
-}
reloadSeconds : Pirate -> Float
reloadSeconds pirate =
    4.5 + 0.8 * toFloat (modBy 3 pirate.id)


{-| The built-in boat a pirate is drawn as (with black sails). Kept for
backwards compatibility; use `shipNameFor` for faction-specific vessels.
-}
shipName : String
shipName =
    "Schooner"


{-| Sail color tint for each faction's patrol vessel:

  - Pirates: deep black/charcoal
  - BlueCoats: imperial blue
  - RedCoats: royal crimson red

-}
sailTint : Pirate -> Vec3
sailTint pirate =
    case pirate.faction of
        Pirates ->
            vec3 0.08 0.08 0.1

        BlueCoats ->
            vec3 0.12 0.38 0.85

        RedCoats ->
            vec3 0.82 0.16 0.12


{-| Model a faction's vessel is drawn with (see `Sails.NpcShips`): Pirates
and RedCoats sail three-masted frigates, while BlueCoats patrol in nimble
sloops.
-}
shipNameFor : Pirate -> String
shipNameFor pirate =
    case pirate.faction of
        Pirates ->
            NpcShips.frigateName

        BlueCoats ->
            NpcShips.sloopName

        RedCoats ->
            NpcShips.frigateName


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


{-| Whether a cannon slot (see `shipSlot`) belongs to a pirate.
-}
isPirateSlot : Int -> Bool
isPirateSlot slot =
    slot >= shipSlot 0


{-| Assigns a patrol coordinate to the nearest faction's stronghold island:

  - Smuggler's Reach (North) -> Pirates
  - Governor's Anchorage (South-West) -> BlueCoats
  - Port Royal Brick (East) -> RedCoats

-}
closestFaction : Float -> Float -> Course -> Faction
closestFaction x z course =
    let
        islandPos f defaultX defaultZ =
            course.towns
                |> List.filterMap
                    (\t ->
                        if t.faction == Just f then
                            Just ( t.island.x, t.island.z )

                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault ( defaultX, defaultZ )

        ( sx, sz ) =
            islandPos Pirates 40 330

        ( ax, az ) =
            islandPos BlueCoats -170 230

        ( hx, hz ) =
            islandPos RedCoats 150 90

        dist2 ix iz =
            (x - ix) ^ 2 + (z - iz) ^ 2

        dS =
            dist2 sx sz

        dA =
            dist2 ax az

        dH =
            dist2 hx hz
    in
    if dS <= dA && dS <= dH then
        Pirates

    else if dA <= dH then
        BlueCoats

    else
        RedCoats


{-| Every patrol circle the course has room for, one per leg between
harbours (no lap timing yet: id, centre, radius and faction only), in course order.
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
            , faction = closestFaction mid.x mid.z course
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


{-| The patrol fleet of a race with this seed: 3 ships, one for each
faction (Pirates, BlueCoats, RedCoats), each guarding the patrol route
closest to its stronghold island.
-}
fleet : Int -> Course -> List Pirate
fleet seed course =
    let
        routes =
            patrolRoutes course

        factions =
            [ Pirates, BlueCoats, RedCoats ]

        pickFaction f ( chosen, available, s ) =
            let
                ( r, s1 ) =
                    random s

                ( phase, s2 ) =
                    random s1

                ( way, s3 ) =
                    random s2

                ( pace, s4 ) =
                    random s3

                matching =
                    List.filter (\route -> route.faction == f) available

                candidates =
                    if List.isEmpty matching then
                        available

                    else
                        matching
            in
            if List.isEmpty candidates then
                ( chosen, available, s4 )

            else
                let
                    i =
                        min (List.length candidates - 1) (floor (r * toFloat (List.length candidates)))
                in
                case List.drop i candidates of
                    route :: _ ->
                        ( { route
                            | faction = f
                            , phase = phase
                            , direction =
                                if way < 0.5 then
                                    1

                                else
                                    -1
                            , lapSeconds = 11 + 3 * pace
                          }
                            :: chosen
                        , List.filter (\rt -> rt.id /= route.id) available
                        , s4
                        )

                    [] ->
                        ( chosen, available, s4 )

        ( picked, _, _ ) =
            List.foldl pickFaction ( [], routes, clamp 1 (modulus - 1) seed ) factions
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
    , carriedStandard = Nothing
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
