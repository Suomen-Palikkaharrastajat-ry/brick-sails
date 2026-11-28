module Sails.Pirates exposing
    ( Pirate
    , ShipKind(..)
    , allKinds
    , boardingCooldown
    , boardingRange
    , collisionRadius
    , fleet
    , fleetSize
    , harbourClearance
    , hitRadius
    , hitsToSink
    , isPirateSlot
    , kindOf
    , patrolRoutes
    , patrolSeparation
    , reach
    , reloadSeconds
    , sailTint
    , shipNameFor
    , shipScale
    , shipSlot
    , stateAt
    )

{-| Pirate and patrol ships: a fleet of faction NPC vessels of three kinds
that patrol the course.

Each ship sails round and round a small circle centred on a leg between two
harbours (half way along it, or a quarter of the way from either end), so it
crosses the route twice a lap and the player meets it on the way. The fleet
has `fleetSize` ships, two for each faction (Pirates, BlueCoats, RedCoats),
each faction's pair of two different kinds (`ShipKind`): a big `Frigate`, a
`Corvette` (on the frigate's model) and a smaller `Sloop`, all at the
player's brick scale. A ship guards a patrol spot near
its faction's island where the course has one, with faction-specific sail
tints. Which spot, where on the circle each starts, which way round and how
fast all come from a seed.

  - A circle is only used when it keeps `harbourClearance` metres clear of
    every harbour's mooring circle and the start, and clear of the islands:
    a patrol ship never sits on a harbour, so the course can always be
    finished. Two ships' circles keep `patrolSeparation` metres apart
    (centre to centre), so the ships never sail through each other.
  - A patrol ship is a ship of its own size: the boat cannot sail through it, and
    bumps off it as it does off an island (`collisionRadius`, by kind).
  - A patrol ship the boat comes within `boardingRange` of (by kind) boards
    it: the hull takes one cannon hit's damage (the crew fights the boarders
    off and wins `Sails.Coins.pirateBoarded` coins), then no ship can board
    again for `boardingCooldown` seconds.
  - Patrol ships shoot back. A ship within `reach` of the boat fires a ball at
    it every `reloadSeconds`; a ball that hits takes one cannon hit's damage.
  - A cannon ball that comes within a ship's `hitRadius` (by kind) sinks it
    for the rest of the run, counts as a ship sunk and wins
    `Sails.Coins.pirateSunk` coins.

-}

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.NpcShips as NpcShips
import Sails.Physics as Physics
import Sails.Types exposing (Course, Faction(..), SailingState)


{-| The three kinds of patrol ship, each drawn with one of the NPC models of
`Sails.NpcShips` at the boats' own brick scale (`Sails.ShipMesh.studScale`,
`shipScale` is 1), so its bricks and studs are the size of the player's:

  - `Frigate`: large, the three-masted frigate (`npc:frigate`), 9.8 m long
    (14 studs with the bowsprit), as a player's build on the frigate hull.
  - `Corvette`: the same frigate model, as large as the frigate.
  - `Sloop`: small, the single-masted sloop (`npc:sloop`), 8.4 m long
    (12 studs with the bowsprit), as a player's build on the sloop hull.

-}
type ShipKind
    = Frigate
    | Corvette
    | Sloop


{-| Every kind of patrol ship, largest first.
-}
allKinds : List ShipKind
allKinds =
    [ Frigate, Corvette, Sloop ]


{-| A pirate's patrol: its id, the centre and radius of its circle, the
seconds a lap takes, where on the circle it is at time 0 (a share of a
lap), which way round it sails (1 clockwise seen from above, -1 the other
way), its faction and its kind of ship.
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
    , kind : ShipKind
    }


{-| How many pirates a race has (fewer if the course has fewer patrol spots
clear of the harbours, the islands and each other).
-}
fleetSize : Int
fleetSize =
    List.length fleetOrder


{-| The ships of a full fleet, in the order they are placed: two for each
faction, each pair of two different kinds, so the first three already have
one of each kind.
-}
fleetOrder : List ( Faction, ShipKind )
fleetOrder =
    [ ( Pirates, Frigate )
    , ( BlueCoats, Sloop )
    , ( RedCoats, Corvette )
    , ( Pirates, Sloop )
    , ( BlueCoats, Corvette )
    , ( RedCoats, Frigate )
    ]


{-| The kind of ship a pirate sails.
-}
kindOf : Pirate -> ShipKind
kindOf pirate =
    pirate.kind


{-| How many times the boats' brick scale (`Sails.ShipMesh.studScale`) a
kind of ship is drawn at: 1 for every kind, so a patrol ship is the same
size as the player's build of the same model (bricks and studs 1:1). A
ship's size comes from its model (`shipNameFor`), never from a stretch.
-}
shipScale : ShipKind -> Float
shipScale _ =
    1


{-| Radius (m) of a kind of pirate ship for collisions with the boat: the
boat bumps off it at this distance plus its own radius
(`Sails.Physics.boatRadius`) from the pirate's centre. A circle about 0.24
times the hull's drawn length (the frigate model is 9.8 m long, the sloop
8.4 m, at `shipScale` 1), wider than half the 3.6 m beam: frigates and
corvettes share the frigate model, so they share its radius.
-}
collisionRadius : ShipKind -> Float
collisionRadius kind =
    case kind of
        Frigate ->
            2.35

        Corvette ->
            2.35

        Sloop ->
            2.0


{-| How many cannon hits a pirate ship takes before it sinks: it lasts a few
shots, and every hit flashes it.
-}
hitsToSink : Int
hitsToSink =
    3


{-| How close (m) a cannon ball must come to a kind of ship to hit it: its
`collisionRadius` and 0.6 m more for the rig above the deck.
-}
hitRadius : ShipKind -> Float
hitRadius kind =
    collisionRadius kind + 0.6


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


{-| Model a pirate's vessel is drawn with (see `Sails.NpcShips`), by its
kind: frigates and corvettes are three-masted frigates, sloops are
sloops, all at the player's brick scale (`shipScale`). A sunk pirate breaks up into the
parts of this same model (`Sails.Wreck`).
-}
shipNameFor : Pirate -> String
shipNameFor pirate =
    case pirate.kind of
        Frigate ->
            NpcShips.frigateName

        Corvette ->
            NpcShips.frigateName

        Sloop ->
            NpcShips.sloopName


{-| Metres a pirate's patrol keeps clear of every harbour's mooring circle.
-}
harbourClearance : Float
harbourClearance =
    8


{-| How close (m) a kind of pirate comes before it boards the boat: just
past where the boat bumps off it (`Sails.Physics.boatRadius` plus
`collisionRadius`), so a boat run alongside is always boarded.
-}
boardingRange : ShipKind -> Float
boardingRange kind =
    Physics.boatRadius + collisionRadius kind + 0.7


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


{-| Metres between the centres of two pirates' patrol circles at the least:
both circles and the largest hull either side, with room to spare, so two
pirates never sail into each other.
-}
patrolSeparation : Float
patrolSeparation =
    2 * patrolRadius + 2 * collisionRadius Frigate + 0.4


{-| Shares of the way along a leg between harbours where a patrol circle
may sit, the middle first.
-}
legShares : List Float
legShares =
    [ 0.5, 0.25, 0.75 ]


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


{-| Every patrol circle the course has room for: on each leg between
harbours, half way along and a quarter of the way from either end
(`legShares`), when clear of the harbours, the start and the islands (no
lap timing yet: id, centre, radius and faction only; the kind is set by
`fleet`), in course order. Circles of one leg may lie closer together than
`patrolSeparation`; `fleet` never picks two of those.
-}
patrolRoutes : Course -> List Pirate
patrolRoutes course =
    let
        harbours =
            List.map .position course.waypoints

        legs =
            List.map2 Tuple.pair harbours (List.drop 1 harbours)

        spot legIndex ( k, share ) ( a, b ) =
            let
                at =
                    Vec3.add a (Vec3.scale share (Vec3.sub b a))
                        |> Vec3.toRecord
            in
            { id = legIndex * List.length legShares + k
            , centreX = at.x
            , centreZ = at.z
            , radius = patrolRadius
            , lapSeconds = 12
            , phase = 0
            , direction = 1
            , faction = closestFaction at.x at.z course
            , kind = Frigate
            }

        leg i ab =
            List.indexedMap (\k share -> spot i ( k, share ) ab) legShares
    in
    List.indexedMap leg legs
        |> List.concat
        |> List.filter (isClear course)


{-| Whether a patrol circle stays clear of the harbours, the start and the
islands.
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
    in
    List.all (\w -> gap w.position >= w.radius + harbourClearance) course.waypoints
        && (gap course.startPosition >= Physics.harbourRadius + harbourClearance)
        && List.all (\i -> fromCentre i.x i.z - p.radius >= i.radius + 4) course.islands


{-| Whether two patrol circles keep `patrolSeparation` apart.
-}
apart : Pirate -> Pirate -> Bool
apart a b =
    sqrt ((a.centreX - b.centreX) ^ 2 + (a.centreZ - b.centreZ) ^ 2) >= patrolSeparation


{-| The patrol fleet of a race with this seed: up to `fleetSize` ships, two
for each faction (Pirates, BlueCoats, RedCoats) of two different kinds
(`fleetOrder`), each on a patrol spot near its stronghold island where
there is one left, else on any spot left, every one `patrolSeparation`
from the others.
-}
fleet : Int -> Course -> List Pirate
fleet seed course =
    let
        pick ( f, kind ) ( chosen, available, s ) =
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

                i =
                    min (List.length candidates - 1) (floor (r * toFloat (List.length candidates)))
            in
            case List.drop i candidates of
                route :: _ ->
                    ( { route
                        | faction = f
                        , kind = kind
                        , phase = phase
                        , direction =
                            if way < 0.5 then
                                1

                            else
                                -1
                        , lapSeconds = baseLapSeconds kind + 3 * pace
                      }
                        :: chosen
                    , List.filter (apart route) available
                    , s4
                    )

                [] ->
                    ( chosen, available, s4 )

        ( picked, _, _ ) =
            List.foldl pick ( [], patrolRoutes course, clamp 1 (modulus - 1) seed ) fleetOrder
    in
    List.sortBy .id picked


{-| The shortest lap (s) of a kind of ship round its circle: the small
sloops sail fastest, the big frigates slowest.
-}
baseLapSeconds : ShipKind -> Float
baseLapSeconds kind =
    case kind of
        Frigate ->
            12

        Corvette ->
            11

        Sloop ->
            10


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
