module StructuresTest exposing (suite)

{-| Buildings and piers that can be shot down (`Sails.Structures`): the
durability table, cannon balls wearing a structure down until it falls and
leaves its island's dressing (or, for a town pier, bursts into bricks), the
patrol ships' balls hitting buildings only, and the cannons choosing a
structure in range only while no ship is (the player's built cannons with a
raised lob onto a building up on an island, the patrol ships at the
nearest rival building in their reach).
-}

import Dict
import Expect
import I18n exposing (Language(..))
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Pages.Sail as Sail exposing (SailModel, SailMsg(..))
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.Harbours as Harbours
import Sails.Physics as Physics
import Sails.Pirates as Pirates exposing (ShipKind(..))
import Sails.ShipMesh exposing (Muzzle)
import Sails.Structures as Structures exposing (Kind(..), Structure)
import Sails.Types exposing (Cannonball, Faction(..))
import Sails.Wreck as Wreck
import Test exposing (Test, describe, test)


{-| A fresh voyage with no patrol ships about.
-}
fresh : SailModel
fresh =
    let
        m =
            Sail.initSail En Boats.default Archipelago.defaultCourse True Nothing Nothing { available = False, enabled = False } 800 600
    in
    { m | pirates = [] }


step : SailModel -> SailModel
step m =
    Tuple.first (Sail.update (Tick (1000 / 60)) m)


{-| Step until `done` holds, at most `n` frames.
-}
stepUntil : Int -> (SailModel -> Bool) -> SailModel -> SailModel
stepUntil n done m =
    if n <= 0 || done m then
        m

    else
        stepUntil (n - 1) done (step m)


firstOf : Kind -> SailModel -> Maybe Structure
firstOf kind m =
    m.structures.structures |> List.filter (\s -> s.kind == kind) |> List.head


{-| A ball of `slot` sitting in a structure (its aim point, where a lob
comes down 0.6 m over it), hardly moving.
-}
ballIn : Int -> Int -> Structure -> Cannonball
ballIn slot id s =
    { id = id, sourceSlot = slot, position = Vec3.add (Structures.aimPoint s) (vec3 0 0.6 0), velocity = vec3 0 0.1 0, age = 0 }


{-| Fire `n` of the boat's own balls into a structure, one frame each.
-}
shell : Int -> Int -> Structure -> SailModel -> SailModel
shell slot n s m =
    List.foldl (\i acc -> step { acc | cannonballs = [ ballIn slot (900 + i) s ] }) m (List.range 1 n)


hitsTaken : SailModel -> Structure -> Int
hitsTaken m s =
    Structures.durability s.kind - Structures.hitsLeft m.structures s


starboardGun : Muzzle
starboardGun =
    { position = vec3 1.2 1.1 0.4, side = 1, direction = vec3 1 0 0 }


suite : Test
suite =
    describe "Sails.Structures"
        [ describe "durability table"
            [ test "every kind takes at least one hit" <|
                \_ -> Expect.equal [] (List.filter (\k -> Structures.durability k < 1) Structures.kinds)
            , test "a harbour pier takes the hits it always took" <|
                \_ -> Expect.equal ( 4, 4 ) ( Harbours.hitsToDestroy, Structures.durability HarbourPier )
            , test "stone outlasts timber, the fort outlasts all" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.atLeast (Structures.durability Cottage + 1) (Structures.durability StoneHouse)
                        , \_ -> Expect.equal [ Fort ] (List.filter (\k -> List.all (\o -> Structures.durability o <= Structures.durability k) Structures.kinds) Structures.kinds)
                        ]
                        ()
            , test "dressing props map to their kinds, trees and rocks to none" <|
                \_ ->
                    Expect.equal
                        [ Just Cottage, Just StoneHouse, Just TownHall, Just TownHall, Just Fort, Just Lighthouse, Just Cairn, Just Barn, Nothing, Nothing, Nothing ]
                        (List.map Structures.kindOfProp [ "dec:house:cottage", "dec:house:stoneHouse:snow", "dec:hall", "dec:hall:snow", "fort", "dec:lighthouse", "dec:cairn", "barn", "rock", "dec:fence", "dec:lamp" ])
            ]
        , describe "the voyage's structures"
            [ test "the islands' buildings and one pier per town, each with its own id" <|
                \_ ->
                    let
                        all =
                            fresh.structures.structures
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length fresh.course.towns) (List.length (List.filter (\s -> s.kind == TownPier) all))
                        , \_ -> Expect.atLeast (List.length fresh.course.towns) (List.length (List.filter (\s -> s.kind == TownHall) all))
                        , \_ -> Expect.equal (List.range 0 (List.length all - 1)) (List.map .id all)
                        , \_ -> Expect.equal True (List.all (\s -> Structures.contains s (Vec3.add (Structures.aimPoint s) (vec3 0 0.6 0))) all)
                        ]
                        ()
            , test "a ball well clear of a building misses it" <|
                \_ ->
                    case firstOf TownHall fresh of
                        Just s ->
                            Expect.equal False (Structures.contains s (vec3 s.x (s.base + s.height + 3) s.z) || Structures.contains s (vec3 (s.x + 12) (s.base + 1) s.z))

                        Nothing ->
                            Expect.fail "no town hall"
            ]
        , describe "damage and destruction"
            [ test "a hit counts and shows the structure's durability bar" <|
                \_ ->
                    case firstOf TownHall fresh of
                        Just s ->
                            let
                                after =
                                    shell 1 1 s fresh
                            in
                            Expect.all
                                [ \m -> Expect.equal 1 (hitsTaken m s)
                                , \m -> Expect.equal [ ( "structure:" ++ String.fromInt s.id, Structures.durability TownHall - 1, Structures.durability TownHall ) ] (List.map (\k -> ( k.key, k.left, k.total )) m.damageMarks)
                                , \m -> Expect.equal [] (List.filter (\b -> b.id > 900) m.cannonballs)
                                ]
                                after

                        Nothing ->
                            Expect.fail "no town hall"
            , test "at its durability a building falls: out of the dressing, rubble in its place" <|
                \_ ->
                    case firstOf TownHall fresh of
                        Just s ->
                            let
                                n =
                                    Structures.durability TownHall

                                almost =
                                    shell 1 (n - 1) s fresh

                                down =
                                    shell 1 1 s almost

                                islandOf m =
                                    Structures.islandsFor m.structures m.islandMeshes |> List.drop s.island |> List.head

                                stillDressed m =
                                    islandOf m |> Maybe.map (\e -> List.any (\p -> Just p.item == s.item) e.placed && List.any (\it -> Just it == s.item) e.items) |> Maybe.withDefault False
                            in
                            Expect.all
                                [ \_ -> Expect.equal False (Structures.isDestroyed almost.structures s)
                                , \_ -> Expect.equal True (stillDressed almost)
                                , \_ -> Expect.equal True (Structures.isDestroyed down.structures s)
                                , \_ -> Expect.equal False (stillDressed down)
                                , \_ -> Expect.equal 1 (List.length (Structures.rubble down.structures))
                                , \_ -> Expect.equal False (List.member s (Structures.standing down.structures))
                                , \_ -> Expect.equal True (stillDressed fresh)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "no town hall"
            , test "a fallen building takes no more hits: balls fly on through" <|
                \_ ->
                    case firstOf Cottage fresh of
                        Just s ->
                            let
                                down =
                                    shell 1 (Structures.durability Cottage) s fresh

                                again =
                                    step { down | cannonballs = [ ballIn 1 990 s ] }
                            in
                            Expect.all
                                [ \m -> Expect.equal (Structures.durability Cottage) (hitsTaken m s)
                                , \m -> Expect.equal 1 (List.length (List.filter (\b -> b.id == 990) m.cannonballs))
                                ]
                                again

                        Nothing ->
                            Expect.fail "no cottage"
            , test "a town pier shot down bursts into bricks and is no longer drawn" <|
                \_ ->
                    case firstOf TownPier fresh of
                        Just s ->
                            let
                                down =
                                    shell 1 (Structures.durability TownPier) s fresh
                            in
                            Expect.all
                                [ \m -> Expect.equal (List.filterMap identity [ s.town ]) (Structures.townPiersDown m.structures)
                                , \m -> Expect.atLeast Wreck.pieceCount (List.length m.debris)
                                ]
                                down

                        Nothing ->
                            Expect.fail "no town pier"
            , test "the patrol ships' balls hit buildings but not piers" <|
                \_ ->
                    case ( firstOf TownHall fresh, firstOf TownPier fresh ) of
                        ( Just hall, Just pier ) ->
                            let
                                after =
                                    step { fresh | cannonballs = [ ballIn (Pirates.shipSlot 0) 901 hall, ballIn (Pirates.shipSlot 0) 902 pier ] }
                            in
                            Expect.equal ( 1, 0 ) ( hitsTaken after hall, hitsTaken after pier )

                        _ ->
                            Expect.fail "no hall or pier"
            , test "sailing the course again stands everything up again" <|
                \_ ->
                    case firstOf Cottage fresh of
                        Just s ->
                            let
                                restarted =
                                    Tuple.first (Sail.update RestartCourse (shell 1 (Structures.durability Cottage) s fresh))
                            in
                            Expect.equal ( Dict.empty, [] ) ( restarted.structures.damage, restarted.structures.fallen )

                        Nothing ->
                            Expect.fail "no cottage"
            ]
        , describe "choosing a target"
            [ test "ships in range come first" <|
                \_ ->
                    let
                        foe =
                            vec3 20 0 0

                        house =
                            vec3 8 4 0
                    in
                    Expect.equal [ foe ] (Structures.cannonTargets (vec3 0 0 0) [ foe ] [ house ])
            , test "with no ship in range, the structures in range join the foes" <|
                \_ ->
                    let
                        farFoe =
                            vec3 80 0 0

                        near =
                            vec3 12 4 0

                        tooFar =
                            vec3 0 4 (Physics.cannonRange + 5)
                    in
                    Expect.equal [ farFoe, near ] (Structures.cannonTargets (vec3 0 0 0) [ farFoe ] [ near, tooFar ])
            , test "the harbour the boat docks at next is never a target" <|
                \_ ->
                    Expect.equal [ vec3 5 0 6 ] (Structures.pierAims (Just 0) [ ( 0, { x = 1, z = 2 } ), ( 1, { x = 5, z = 6 } ) ])
            , test "a patrol ship fires at the boat in reach, else the nearest building in reach" <|
                \_ ->
                    let
                        ship =
                            vec3 0 0 0

                        a =
                            vec3 10 5 0

                        b =
                            vec3 0 5 6

                        out =
                            vec3 0 5 (Pirates.reach + 3)
                    in
                    Expect.equal
                        [ Just (vec3 15 0 0), Just b, Nothing ]
                        [ Structures.npcTarget Pirates.reach ship (vec3 15 0 0) [ a, b ]
                        , Structures.npcTarget Pirates.reach ship (vec3 90 0 0) [ a, b, out ]
                        , Structures.npcTarget Pirates.reach ship (vec3 90 0 0) [ out ]
                        ]
            , test "a ship never shells its own side or a town with no flag" <|
                \_ ->
                    Expect.equal [] (List.filter (\s -> s.faction == Just Pirates || s.faction == Nothing) (Structures.rivalBuildings Pirates fresh.structures))
            , test "a built cannon lobs onto a target high over its muzzle" <|
                \_ ->
                    let
                        state =
                            let
                                s =
                                    Physics.initSailingState fresh.course
                            in
                            { s | position = vec3 0 0 0, heading = 0, speed = 0 }

                        muzzle =
                            Physics.muzzleWorld state starboardGun

                        target =
                            Vec3.add muzzle (vec3 18 5 0)

                        v =
                            Vec3.toRecord (Physics.cannonShotVelocity state starboardGun (Just (Vec3.sub target (vec3 0 0.6 0))))

                        t =
                            18 / v.x

                        rise =
                            v.y * t - 0.5 * 9.81 * t * t
                    in
                    Expect.within (Expect.Absolute 0.05) 5 rise
            , test "with no ship about, the F key's cannon shot comes down on the nearest building" <|
                \_ ->
                    let
                        -- Every pier down, so the nearest target is a building.
                        piersDown =
                            fresh.structures.structures
                                |> List.filter (\s -> s.kind == TownPier)
                                |> List.foldl (\s d -> Dict.insert s.id (Structures.durability TownPier) d) Dict.empty

                        cleared =
                            let
                                st =
                                    fresh.structures
                            in
                            { fresh
                                | structures = { st | damage = piersDown }
                                , harbourDamage = List.foldl (\( i, _ ) d -> Harbours.wreck i d) Dict.empty (Harbours.footprints fresh.course)
                            }
                    in
                    case firstOf TownHall cleared of
                        Just hall ->
                            let
                                isl =
                                    List.drop hall.island cleared.course.islands |> List.head |> Maybe.withDefault { x = 0, z = 0, radius = 1, height = 1, role = Sails.Types.Hub }

                                bearing =
                                    atan2 (hall.x - isl.x) (hall.z - isl.z)

                                at =
                                    vec3 (isl.x + sin bearing * (isl.radius + 3)) 0 (isl.z + cos bearing * (isl.radius + 3))

                                nearest =
                                    Structures.standing cleared.structures
                                        |> List.sortBy (\s -> Physics.horizontalDistance at (Structures.aimPoint s))
                                        |> List.head
                                        |> Maybe.withDefault hall

                                aim =
                                    Vec3.toRecord (Vec3.sub (Structures.aimPoint nearest) at)

                                heading =
                                    atan2 (negate aim.z) aim.x * 180 / pi

                                st0 =
                                    cleared.state

                                ready =
                                    { cleared
                                        | state = { st0 | position = at, heading = heading, speed = 0 }
                                        , cannons = [ starboardGun ]
                                        , anchored = True
                                        , sailLevel = 0
                                    }

                                fired =
                                    Tuple.first (Sail.update FireCannons ready)

                                landed =
                                    stepUntil 150 (\m -> hitsTaken m nearest > 0) fired
                            in
                            Expect.all
                                [ \_ -> Expect.equal True (Structures.isBuilding nearest.kind)
                                , \_ -> Expect.atMost Physics.cannonRange (Physics.horizontalDistance at (Structures.aimPoint nearest))
                                , \_ -> Expect.equal 1 (List.length fired.cannonballs)
                                , \_ -> Expect.equal 1 (hitsTaken landed nearest)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "no town hall"
            , test "a patrol ship with the boat far off shells a rival building in its reach" <|
                \_ ->
                    case Structures.rivalBuildings Pirates fresh.structures |> List.head of
                        Just target ->
                            let
                                pirate =
                                    { id = 0, centreX = target.x + 12, centreZ = target.z, radius = 0.01, lapSeconds = 10000, phase = 0, direction = 1, faction = Pirates, kind = Sloop }

                                st0 =
                                    fresh.state

                                far =
                                    { fresh | pirates = [ pirate ], state = { st0 | position = vec3 (target.x + 300) 0 target.z } }

                                -- The ball may come down on a house of the same
                                -- town on its way in: any of the rival town's.
                                rivals =
                                    List.filter (\s -> s.faction == target.faction) (Structures.rivalBuildings Pirates fresh.structures)

                                shelled m =
                                    List.filter (\s -> hitsTaken m s > 0) fresh.structures.structures

                                after =
                                    stepUntil 240 (\m -> not (List.isEmpty (shelled m))) far
                            in
                            Expect.all
                                [ \m -> Expect.equal 1 (List.length (shelled m))
                                , \m -> Expect.equal True (List.all (\s -> List.member s rivals) (shelled m))
                                ]
                                after

                        Nothing ->
                            Expect.fail "no rival building"
            ]
        ]
