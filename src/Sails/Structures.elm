module Sails.Structures exposing
    ( Kind(..)
    , Rubble
    , Shape(..)
    , Smashed
    , State
    , Structure
    , aimPoint
    , cannonTargets
    , checkHits
    , contains
    , durability
    , fromCourse
    , hitsLeft
    , inReach
    , init
    , isBuilding
    , isDestroyed
    , islandsFor
    , kindName
    , kindOfProp
    , kinds
    , landmarkDown
    , npcTarget
    , pierAims
    , recordHits
    , rivalBuildings
    , rubble
    , standing
    , topPoint
    , townPiersDown
    )

{-| The buildings and piers on and round the islands can be shot down.

Every structure has a durability (`durability`, a small table by `Kind`):
the cannon ball hits it takes before it falls. The houses of the towns, the
town halls, the farms' barns, the hub's fort, lighthouse or cairn and the
towns' piers are listed once per voyage from the island dressing
(`fromCourse`, on `Sails.IslandMesh`'s plan); the harbours' piers keep their
own damage in `Sails.Harbours` (its `hitsToDestroy` is this table's
`HarbourPier`).

  - A ball within a structure's `Shape` (`contains`) hits it and is gone
    (`checkHits`): a building is a round tower of its dressing item's reach
    and height, a town pier the box of its deck out on the water, as a
    harbour's pier.
  - The player's own balls hit every structure; the patrol ships' balls hit
    only buildings, never a pier (`isBuilding`).
  - At its durability a structure is destroyed (`recordHits`): a building
    leaves the island's dressing (its far proxy too: the island's far mesh
    is baked again once, without it) and a low heap of rubble stays where it
    stood (`rubble`); a town pier bursts into its own bricks
    (`Sails.Wreck.HarbourPier`), as a smashed harbour does. Nothing is
    rebuilt until the course is sailed again.

The cannons aim at structures too (`cannonTargets`): foes (ships) within
`Sails.Physics.cannonRange` come first, so ship combat is as before; with no
foe in range the structures in range are added and the cannons' usual
choice (nearest to the ship, ranged to its distance) picks among them. The
patrol ships fire at the boat when it is in their reach, else at the
nearest building in reach on a rival faction's island (`npcTarget`).

-}

import Dict exposing (Dict)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.IslandDressing exposing (Item)
import Sails.IslandMesh as IslandMesh exposing (CourseMesh, IslandMeshes)
import Sails.Physics as Physics
import Sails.Types exposing (Cannonball, CombatImpact, Course, Faction)
import Sails.World exposing (IslandPalette)
import WebGL



-- KINDS


{-| What a structure is. The six house styles of `Sails.IslandDecor`, the
town hall, a farm's barn, the hub's landmarks, a town's pier and a
harbour's pier.
-}
type Kind
    = Cottage
    | TownHouse
    | LongHouse
    | Warehouse
    | Tavern
    | StoneHouse
    | TownHall
    | Barn
    | Fort
    | Lighthouse
    | Cairn
    | TownPier
    | HarbourPier


{-| Every kind, for tables and tests.
-}
kinds : List Kind
kinds =
    [ Cottage, TownHouse, LongHouse, Warehouse, Tavern, StoneHouse, TownHall, Barn, Fort, Lighthouse, Cairn, TownPier, HarbourPier ]


{-| Cannon ball hits a structure of a kind takes before it falls: the
tuning table. Timber is quick to knock down, stone takes longer and the
fort longest. A harbour's pier keeps the 4 hits it always took.
-}
durability : Kind -> Int
durability kind =
    case kind of
        Cottage ->
            2

        TownHouse ->
            3

        LongHouse ->
            3

        Warehouse ->
            3

        Tavern ->
            3

        StoneHouse ->
            4

        TownHall ->
            5

        Barn ->
            2

        Fort ->
            8

        Lighthouse ->
            6

        Cairn ->
            4

        TownPier ->
            3

        HarbourPier ->
            4


{-| A short name of a kind (for keys and tests).
-}
kindName : Kind -> String
kindName kind =
    case kind of
        Cottage ->
            "cottage"

        TownHouse ->
            "townHouse"

        LongHouse ->
            "longHouse"

        Warehouse ->
            "warehouse"

        Tavern ->
            "tavern"

        StoneHouse ->
            "stoneHouse"

        TownHall ->
            "townHall"

        Barn ->
            "barn"

        Fort ->
            "fort"

        Lighthouse ->
            "lighthouse"

        Cairn ->
            "cairn"

        TownPier ->
            "townPier"

        HarbourPier ->
            "harbourPier"


{-| The kind of structure a dressing prop (`Sails.IslandDressing.Item`'s
`prop`) is, or Nothing for trees, rocks, fences and the other props that
are not structures.
-}
kindOfProp : String -> Maybe Kind
kindOfProp prop =
    let
        house =
            String.dropLeft (String.length "dec:house:") prop
                |> String.split ":"
                |> List.head
                |> Maybe.withDefault ""
    in
    if String.startsWith "dec:house:" prop then
        case house of
            "cottage" ->
                Just Cottage

            "townHouse" ->
                Just TownHouse

            "longHouse" ->
                Just LongHouse

            "warehouse" ->
                Just Warehouse

            "tavern" ->
                Just Tavern

            "stoneHouse" ->
                Just StoneHouse

            _ ->
                Nothing

    else if String.startsWith "dec:hall" prop then
        Just TownHall

    else
        case prop of
            "barn" ->
                Just Barn

            "fort" ->
                Just Fort

            "dec:lighthouse" ->
                Just Lighthouse

            "dec:cairn" ->
                Just Cairn

            _ ->
                Nothing


{-| A building on an island, rather than a pier on the water.
-}
isBuilding : Kind -> Bool
isBuilding kind =
    kind /= TownPier && kind /= HarbourPier



-- STRUCTURES


{-| Where a structure stands and what a ball must come within to hit it:
a `Round` tower of a radius (m) round its centre, or a `Pier` box along
the compass direction (`dirX`, `dirZ`) it runs out on, with its half length
along that and half width across.
-}
type Shape
    = Round Float
    | Pier { dirX : Float, dirZ : Float, halfLength : Float, halfWidth : Float }


{-| One structure: its `id` (its place in the voyage's list), kind, centre
on the sea plane, foot (`base`, m up) and height, shape, the island it
stands on (its index in the course's islands), the dressing item it is
(buildings) or the town it belongs to (index in the course's towns, town
piers), and the faction of that island's town, if it has one.
-}
type alias Structure =
    { id : Int
    , kind : Kind
    , x : Float
    , z : Float
    , base : Float
    , height : Float
    , shape : Shape
    , island : Int
    , item : Maybe Item
    , town : Maybe Int
    , faction : Maybe Faction
    }


{-| How far (m) round a structure's shape a ball still hits it.
-}
ballMargin : Float
ballMargin =
    0.4


{-| A town pier's box, as a harbour's (`Sails.Harbours`): it runs from 1 m
inside the shore to 6.2 m out, four studs and its pilings wide, and stands
up to 3.5 m over the water.
-}
pierInner : Float
pierInner =
    -1


pierOuter : Float
pierOuter =
    6.2


pierHalfWidth : Float
pierHalfWidth =
    1.4


pierTop : Float
pierTop =
    3.5


{-| Every structure of a voyage: the buildings of each island's dressing
(`IslandMesh.IslandMeshes.items`, in order) and then each town's pier.
-}
fromCourse : Course -> CourseMesh -> List Structure
fromCourse course meshes =
    let
        factionOf isl =
            course.towns
                |> List.filter (\t -> t.island == isl)
                |> List.filterMap .faction
                |> List.head

        indexOf isl =
            List.indexedMap Tuple.pair course.islands
                |> List.filter (\( _, i ) -> i == isl)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault -1

        buildings =
            List.indexedMap
                (\islandIndex entry ->
                    List.filterMap
                        (\it ->
                            kindOfProp it.prop
                                |> Maybe.map
                                    (\kind ->
                                        { id = 0
                                        , kind = kind
                                        , x = it.spot.x
                                        , z = it.spot.z
                                        , base = it.spot.y
                                        , height = it.height
                                        , shape = Round (max 1.5 (0.7 * it.reach))
                                        , island = islandIndex
                                        , item = Just it
                                        , town = Nothing
                                        , faction = factionOf entry.island
                                        }
                                    )
                        )
                        entry.items
                )
                meshes
                |> List.concat

        piers =
            List.indexedMap
                (\ti town ->
                    let
                        dirX =
                            sin (degrees town.bearing)

                        dirZ =
                            cos (degrees town.bearing)

                        mid =
                            town.island.radius + 0.5 * (pierInner + pierOuter)
                    in
                    { id = 0
                    , kind = TownPier
                    , x = town.island.x + dirX * mid
                    , z = town.island.z + dirZ * mid
                    , base = -2
                    , height = pierTop + 2
                    , shape = Pier { dirX = dirX, dirZ = dirZ, halfLength = 0.5 * (pierOuter - pierInner), halfWidth = pierHalfWidth }
                    , island = indexOf town.island
                    , item = Nothing
                    , town = Just ti
                    , faction = town.faction
                    }
                )
                course.towns
    in
    List.indexedMap (\i s -> { s | id = i }) (buildings ++ piers)


{-| Whether a point (a cannon ball) is within a structure's shape.
-}
contains : Structure -> Vec3 -> Bool
contains s point =
    let
        p =
            Vec3.toRecord point

        dx =
            p.x - s.x

        dz =
            p.z - s.z

        upright =
            p.y >= s.base - ballMargin && p.y <= s.base + s.height + ballMargin
    in
    upright
        && (case s.shape of
                Round r ->
                    dx * dx + dz * dz <= (r + ballMargin) ^ 2

                Pier fp ->
                    abs (dx * fp.dirX + dz * fp.dirZ) <= fp.halfLength + ballMargin && abs (dx * fp.dirZ - dz * fp.dirX) <= fp.halfWidth + ballMargin
           )


{-| The point the cannons aim at: a third of the way up a building (the
lob comes down 0.6 m over the aim, see `Sails.Physics.lobFrom`), a pier's
deck.
-}
aimPoint : Structure -> Vec3
aimPoint s =
    case s.shape of
        Round _ ->
            vec3 s.x (s.base + 0.35 * s.height - 0.6) s.z

        Pier _ ->
            vec3 s.x 0 s.z


{-| Just over the top of a structure, where its durability bar shows.
-}
topPoint : Structure -> Vec3
topPoint s =
    vec3 s.x (s.base + s.height + 1.2) s.z



-- STATE


{-| The voyage's structures and what has befallen them: hits taken by id,
the structures destroyed (latest last) and the islands baked again without
their fallen buildings (by island index).
-}
type alias State =
    { structures : List Structure
    , damage : Dict Int Int
    , fallen : List Structure
    , islands : Dict Int IslandMeshes
    }


{-| Every structure of the voyage standing, unhurt.
-}
init : Course -> CourseMesh -> State
init course meshes =
    { structures = fromCourse course meshes
    , damage = Dict.empty
    , fallen = []
    , islands = Dict.empty
    }


{-| Hits a structure can still take.
-}
hitsLeft : State -> Structure -> Int
hitsLeft state s =
    max 0 (durability s.kind - (Dict.get s.id state.damage |> Maybe.withDefault 0))


{-| Whether a structure has fallen.
-}
isDestroyed : State -> Structure -> Bool
isDestroyed state s =
    hitsLeft state s <= 0


{-| The structures still standing.
-}
standing : State -> List Structure
standing state =
    List.filter (not << isDestroyed state) state.structures


{-| Balls hitting the `targets` (standing structures): a ball within one's
shape hits it and is gone. Returns the balls still flying, their hit
flashes and the id of the structure each ball that hit struck.
-}
checkHits : List Structure -> List Cannonball -> ( List Cannonball, List CombatImpact, List Int )
checkHits targets balls =
    let
        step ball ( alive, flashes, hit ) =
            case List.filter (\s -> contains s ball.position) targets |> List.head of
                Just s ->
                    ( alive, { id = ball.id, position = ball.position, age = 0, isHit = True } :: flashes, hit ++ [ s.id ] )

                Nothing ->
                    ( alive ++ [ ball ], flashes, hit )
    in
    List.foldl step ( [], [], [] ) balls


{-| What fell this frame: the structures, and the extra flashes of their
collapse (a puff round the foot and the middle of each building).
-}
type alias Smashed =
    { structures : List Structure
    , flashes : List CombatImpact
    }


{-| Count hits (one structure id per ball) against the structures. A
structure brought to its durability falls: a fallen building leaves its
island's dressing, whose far mesh is baked again (in `palette`, from the
voyage's `meshes`). Returns the new state and what fell.
-}
recordHits : IslandPalette -> CourseMesh -> List Int -> State -> ( State, Smashed )
recordHits palette meshes hits state =
    let
        byId id =
            List.filter (\s -> s.id == id) state.structures |> List.head

        ( damage, fell ) =
            List.foldl
                (\id ( d, down ) ->
                    case byId id of
                        Just s ->
                            let
                                taken =
                                    Dict.get id d |> Maybe.withDefault 0
                            in
                            if taken >= durability s.kind then
                                ( d, down )

                            else if taken + 1 >= durability s.kind then
                                ( Dict.insert id (taken + 1) d, down ++ [ s ] )

                            else
                                ( Dict.insert id (taken + 1) d, down )

                        Nothing ->
                            ( d, down )
                )
                ( state.damage, [] )
                hits

        islands =
            List.foldl (clearRuin palette meshes) state.islands fell

        flashes =
            List.concatMap
                (\s ->
                    if isBuilding s.kind then
                        [ { id = -1 - s.id, position = vec3 s.x (s.base + 0.3) s.z, age = 0, isHit = True }
                        , { id = -1 - s.id, position = vec3 s.x (s.base + 0.5 * s.height) s.z, age = 0.15, isHit = True }
                        ]

                    else
                        []
                )
                fell
    in
    ( { state | damage = damage, fallen = state.fallen ++ fell, islands = islands }
    , { structures = fell, flashes = flashes }
    )


{-| Take a fallen building out of its island's dressing: its real bricks,
its landmark entry and its far proxy (the far mesh is baked again, once).
-}
clearRuin : IslandPalette -> CourseMesh -> Structure -> Dict Int IslandMeshes -> Dict Int IslandMeshes
clearRuin palette meshes s islands =
    case ( s.item, Dict.get s.island islands |> orElse (List.drop s.island meshes |> List.head) ) of
        ( Just gone, Just entry ) ->
            let
                keep it =
                    it /= gone

                items =
                    List.filter keep entry.items
            in
            Dict.insert s.island
                { entry
                    | items = items
                    , placed = List.filter (\p -> keep p.item) entry.placed
                    , landmarks = List.filter (\p -> keep p.item) entry.landmarks
                    , far = WebGL.triangles (IslandMesh.triangles palette entry.island ++ IslandMesh.proxyTriangles entry.island items)
                }
                islands

        _ ->
            islands


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback first =
    case first of
        Just _ ->
            first

        Nothing ->
            fallback



-- TARGETS


{-| Whether a point is a target in the cannons' reach from `from`: more
than 1.5 m and at most `range` m away on the sea plane, as
`Sails.Physics.fireCannons` judges a foe.
-}
inReach : Float -> Vec3 -> Vec3 -> Bool
inReach range from p =
    let
        d =
            Physics.horizontalDistance from p
    in
    d > 1.5 && d <= range


{-| Aim points of the harbours' standing piers (`Sails.Harbours`
footprints, by harbour index), leaving out the harbour the boat is to dock
at next: the boat would only shoot its own mooring away.
-}
pierAims : Maybe Int -> List ( Int, { a | x : Float, z : Float } ) -> List Vec3
pierAims docking piers =
    piers
        |> List.filter (\( i, _ ) -> docking /= Just i)
        |> List.map (\( _, fp ) -> vec3 fp.x 0 fp.z)


{-| What the player's cannons choose among, from a ship at `ship`: the
`foes` (ships) alone while any is within `Sails.Physics.cannonRange`, so a
fight with ships is never drawn off by a house; with none in range, the
foes and the `structures` (aim points) in range, so the nearest of those
in range is fired at.
-}
cannonTargets : Vec3 -> List Vec3 -> List Vec3 -> List Vec3
cannonTargets ship foes structures =
    if List.any (inReach Physics.cannonRange ship) foes then
        foes

    else
        foes ++ List.filter (inReach Physics.cannonRange ship) structures


{-| What a patrol ship at `from` with `reach` (m) fires at: the `boat`
while it is in reach, as always; otherwise the nearest of the `buildings`
(aim points) in reach, or Nothing.
-}
npcTarget : Float -> Vec3 -> Vec3 -> List Vec3 -> Maybe Vec3
npcTarget reach from boat buildings =
    if inReach reach from boat then
        Just boat

    else
        buildings
            |> List.filter (inReach reach from)
            |> List.sortBy (Physics.horizontalDistance from)
            |> List.head


{-| The standing buildings a ship of `faction` fires on: those on an
island whose town flies another faction's flag (no ship shells its own
side or an island with no flag).
-}
rivalBuildings : Faction -> State -> List Structure
rivalBuildings faction state =
    standing state
        |> List.filter (\s -> isBuilding s.kind && s.faction /= Nothing && s.faction /= Just faction)



-- DRAWING


{-| The voyage's island meshes with the islands baked again without their
fallen buildings.
-}
islandsFor : State -> CourseMesh -> CourseMesh
islandsFor state meshes =
    if Dict.isEmpty state.islands then
        meshes

    else
        List.indexedMap (\i entry -> Dict.get i state.islands |> Maybe.withDefault entry) meshes


{-| The towns (by index in the course's towns) whose pier has fallen.
-}
townPiersDown : State -> List Int
townPiersDown state =
    List.filterMap .town state.fallen


{-| Whether the hub's fort or lighthouse has fallen (its light is out).
-}
landmarkDown : State -> Bool
landmarkDown state =
    List.any (\s -> s.kind == Fort || s.kind == Lighthouse) state.fallen


{-| A heap of rubble where a building stood: its foot, its bearing and its
size (m across).
-}
type alias Rubble =
    { at : Vec3
    , bearing : Float
    , size : Float
    }


{-| The rubble of the fallen buildings, one heap each.
-}
rubble : State -> List Rubble
rubble state =
    List.filterMap
        (\s ->
            case ( s.item, s.shape ) of
                ( Just it, Round r ) ->
                    Just { at = vec3 s.x s.base s.z, bearing = it.spot.bearing, size = 1.4 * r }

                _ ->
                    Nothing
        )
        state.fallen
