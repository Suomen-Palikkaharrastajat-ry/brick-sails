module HarbourTest exposing (suite)

{-| Solid, breakable harbours (`Sails.Harbours`) and wrecks that break up
into their own model's parts (`Sails.Wreck`): a ship bumping a pier slowly
stops against it, a fast one smashes it (but never the harbour it is to
dock at), cannon balls wear a pier down and
smash it, an intact harbour still docks, and a sunk frigate breaks up into
frigate parts.
-}

import Dict
import Expect
import I18n exposing (Language(..))
import Math.Vector3 as Vec3 exposing (vec3)
import Pages.Sail as Sail exposing (SailModel, SailMsg(..))
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.Harbours as Harbours exposing (Footprint)
import Sails.NpcShips as NpcShips
import Sails.Physics as Physics
import Sails.Pirates as Pirates exposing (ShipKind(..))
import Sails.Types exposing (Cannonball)
import Sails.Wreck as Wreck
import Test exposing (Test, describe, test)


fresh : SailModel
fresh =
    Sail.initSail En Boats.default Archipelago.defaultCourse True Nothing Nothing { available = False, enabled = False } 800 600


frame : Float
frame =
    1 / 60


step : SailModel -> SailModel
step m =
    Tuple.first (Sail.update (Tick (frame * 1000)) m)


{-| The first harbour's pier: the harbour the boat docks at first.
-}
firstPier : Maybe ( Int, Footprint )
firstPier =
    Harbours.footprints fresh.course |> List.head


{-| The second harbour's pier: one the boat is not docking at yet.
-}
secondPier : Maybe ( Int, Footprint )
secondPier =
    Harbours.footprints fresh.course |> List.drop 1 |> List.head


{-| A point `along` metres out from a pier's centre and `across` metres to
its side.
-}
onPier : Footprint -> Float -> Float -> Vec3.Vec3
onPier fp along across =
    vec3 (fp.x + along * fp.dirX + across * fp.dirZ) 0 (fp.z + along * fp.dirZ - across * fp.dirX)


{-| The boat at `at`, sailing at `speed` along compass `heading`.
-}
placed : Vec3.Vec3 -> Float -> Float -> SailModel -> SailModel
placed at heading speed m =
    let
        s =
            m.state
    in
    { m | state = { s | position = at, heading = heading, speed = speed } }


{-| Heading (degrees) straight in at a pier's head, from out on the water.
-}
towardsIsland : Footprint -> Float
towardsIsland fp =
    180 / pi * atan2 -fp.dirX -fp.dirZ


{-| How far (m) a point is from a pier's box.
-}
clearance : Footprint -> Vec3.Vec3 -> Float
clearance fp at =
    let
        p =
            Vec3.toRecord at

        dx =
            p.x - fp.x

        dz =
            p.z - fp.z

        u =
            dx * fp.dirX + dz * fp.dirZ

        v =
            dx * fp.dirZ - dz * fp.dirX

        du =
            abs u - fp.halfLength |> max 0

        dv =
            abs v - fp.halfWidth |> max 0
    in
    sqrt (du * du + dv * dv)


ballAt : Int -> Vec3.Vec3 -> Cannonball
ballAt id at =
    { id = id, sourceSlot = 1, position = Vec3.add at (vec3 0 1 0), velocity = vec3 0 0 0, age = 0 }


{-| Fire one ball onto a pier per frame, `n` times.
-}
shell : Int -> Footprint -> SailModel -> SailModel
shell n fp m =
    List.foldl (\i x -> step { x | cannonballs = [ ballAt (900 + i) (onPier fp 0 0) ] }) m (List.range 1 n)


harbourPieces : SailModel -> Int
harbourPieces m =
    m.debris
        |> List.filter (\p -> p.look == Wreck.ShipPart Wreck.HarbourPier (indexOf p.look))
        |> List.length


indexOf : Wreck.Look -> Int
indexOf look =
    case look of
        Wreck.ShipPart _ i ->
            i

        Wreck.Brick _ _ ->
            -1


{-| Off the pier head, out on the water, where nothing else is.
-}
offHead : Footprint -> Vec3.Vec3
offHead fp =
    onPier fp (fp.halfLength + 1.0) 0


suite : Test
suite =
    describe "Harbours and wrecks"
        [ describe "Piers are solid"
            [ test "a ship bumping a harbour slowly stops and is pushed back out" <|
                \_ ->
                    case firstPier of
                        Just ( i, fp ) ->
                            let
                                after =
                                    step (placed (offHead fp) (towardsIsland fp) 3 fresh)
                            in
                            Expect.all
                                [ \_ -> Expect.equal 0 after.state.speed
                                , \_ -> Expect.atLeast (Physics.boatRadius - 0.01) (clearance fp after.state.position)
                                , \_ -> Expect.equal False (Harbours.isWrecked after.harbourDamage i)
                                , \_ -> Expect.equal 0 (harbourPieces after)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "no harbours"
            , test "a ship inside a pier's box is pushed out, never trapped" <|
                \_ ->
                    case firstPier of
                        Just ( _, fp ) ->
                            let
                                s =
                                    fresh.state

                                ( pushed, rammed ) =
                                    Harbours.collide Nothing [ ( 0, fp ) ] { s | position = onPier fp 0.5 0.2, speed = 1 }
                            in
                            Expect.all
                                [ \_ -> Expect.equal [] rammed
                                , \_ -> Expect.atLeast (Physics.boatRadius - 0.01) (clearance fp pushed.position)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "no harbours"
            , test "a ship at ramming speed only stops against the harbour it is to dock at" <|
                \_ ->
                    case firstPier of
                        Just ( i, fp ) ->
                            let
                                after =
                                    step (placed (offHead fp) (towardsIsland fp) (Harbours.ramSpeed + 1.5) fresh)
                            in
                            Expect.all
                                [ \_ -> Expect.equal 0 after.progress.currentWaypointIndex
                                , \_ -> Expect.equal False (Harbours.isWrecked after.harbourDamage i)
                                , \_ -> Expect.equal 0 after.state.speed
                                , \_ -> Expect.equal 0 (harbourPieces after)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "no harbours"
            , test "a ship at ramming speed breaks a later harbour and its pieces fly" <|
                \_ ->
                    case secondPier of
                        Just ( i, fp ) ->
                            let
                                after =
                                    step (placed (offHead fp) (towardsIsland fp) (Harbours.ramSpeed + 1.5) fresh)
                            in
                            Expect.all
                                [ \_ -> Expect.equal True (Harbours.isWrecked after.harbourDamage i)
                                , \_ -> Expect.equal Wreck.pieceCount (harbourPieces after)
                                , \_ -> Expect.greaterThan 0 after.state.speed
                                , \_ -> Expect.equal [] (List.filter (Tuple.first >> (==) i) (Harbours.intactFootprints after.harbourDamage after.course))
                                ]
                                ()

                        Nothing ->
                            Expect.fail "no harbours"
            ]
        , describe "Piers can be shot"
            [ test "cannon balls damage a harbour and enough hits destroy it" <|
                \_ ->
                    case firstPier of
                        Just ( i, fp ) ->
                            let
                                once =
                                    shell 1 fp fresh

                                almost =
                                    shell (Harbours.hitsToDestroy - 1) fp fresh

                                smashed =
                                    shell Harbours.hitsToDestroy fp fresh
                            in
                            Expect.all
                                [ \_ -> Expect.equal (Just 1) (Dict.get i once.harbourDamage)
                                , \_ -> Expect.equal (Harbours.hitsToDestroy - 1) (Harbours.hitsLeft once.harbourDamage i)
                                , \_ -> Expect.equal False (Harbours.isWrecked almost.harbourDamage i)
                                , \_ -> Expect.equal 0 (harbourPieces almost)
                                , \_ -> Expect.equal True (Harbours.isWrecked smashed.harbourDamage i)
                                , \_ -> Expect.equal Wreck.pieceCount (harbourPieces smashed)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "no harbours"
            , test "a destroyed harbour cannot be docked at: its leg is marked done, without its coins" <|
                \_ ->
                    case firstPier of
                        Just ( i, fp ) ->
                            let
                                smashed =
                                    shell Harbours.hitsToDestroy fp fresh
                            in
                            Expect.all
                                [ \_ -> Expect.equal 0 i
                                , \_ -> Expect.equal 1 smashed.progress.currentWaypointIndex
                                , \_ -> Expect.equal 1 (List.length smashed.progress.splits)
                                , \_ -> Expect.equal fresh.progress.coins smashed.progress.coins
                                , \_ -> Expect.equal False smashed.progress.finished
                                ]
                                ()

                        Nothing ->
                            Expect.fail "no harbours"
            , test "smashing every harbour ends the voyage" <|
                \_ ->
                    let
                        all =
                            List.foldl (\( i, _ ) d -> Harbours.wreck i d) Dict.empty (Harbours.footprints fresh.course)
                    in
                    Expect.equal True (Tuple.first (Harbours.skipWrecked all fresh.progress)).finished
            ]
        , describe "Intact harbours"
            [ test "an intact harbour still docks: a boat moored alongside its pier drops anchor and moves on" <|
                \_ ->
                    case firstPier of
                        Just ( _, fp ) ->
                            let
                                alongside =
                                    onPier fp 0.4 (fp.halfWidth + Physics.boatRadius + 0.5)

                                moored =
                                    List.foldl (\_ m -> step m) (placed alongside (towardsIsland fp) 0 fresh) (List.range 1 3)

                                docked =
                                    Tuple.first (Sail.update DropAnchor moored)
                            in
                            Expect.all
                                [ \_ -> Expect.equal True (Physics.canDropAnchor moored.course.waypoints moored.progress moored.state)
                                , \_ -> Expect.within (Expect.Absolute 0.2) 0 (Vec3.distance alongside moored.state.position)
                                , \_ -> Expect.equal 1 docked.progress.currentWaypointIndex
                                , \_ -> Expect.equal Dict.empty docked.harbourDamage
                                , \_ -> Expect.equal ( fresh.progress, [] ) (Harbours.skipWrecked Dict.empty fresh.progress)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "no harbours"
            ]
        , describe "Wrecks break into their own parts"
            [ test "a sunk frigate breaks into pieces of its own model, at its size" <|
                \_ ->
                    case List.filter (\p -> p.kind == Frigate) fresh.pirates of
                        p :: _ ->
                            let
                                at =
                                    (Pirates.stateAt (fresh.simTime + frame) p).position

                                sunk =
                                    step { fresh | cannonballs = [ { id = 99, sourceSlot = 1, position = Vec3.add at (vec3 0 1.5 0), velocity = vec3 0 0 0, age = 0 } ] }

                                frigate =
                                    Wreck.Ship { name = NpcShips.frigateName, scale = Pirates.shipScale Frigate }

                                frigatePieces =
                                    List.filter
                                        (\piece ->
                                            case piece.look of
                                                Wreck.ShipPart source _ ->
                                                    source == frigate

                                                Wreck.Brick _ _ ->
                                                    False
                                        )
                                        sunk.debris
                            in
                            Expect.all
                                [ \_ -> Expect.equal [ p.id ] sunk.piratesSunk
                                , \_ -> Expect.equal Wreck.pieceCount (List.length frigatePieces)
                                , \_ -> Expect.equal (List.length sunk.debris) (List.length frigatePieces)
                                , \_ -> Expect.within (Expect.Absolute 1.0e-9) (Pirates.shipScale Frigate) (Wreck.sourceScale frigate)
                                ]
                                ()

                        [] ->
                            Expect.fail "no frigates"
            , test "a big model gives an even sample of distinct parts, capped" <|
                \_ ->
                    let
                        slots =
                            List.filterMap (Wreck.partSlot 57) (List.range 0 (Wreck.pieceCount - 1))
                    in
                    Expect.all
                        [ \_ -> Expect.equal Wreck.pieceCount (List.length slots)
                        , \_ -> Expect.equal slots (List.sortBy identity slots)
                        , \_ -> Expect.equal True (List.all (\s -> s >= 0 && s < 57) slots)
                        , \_ -> Expect.equal (List.length slots) (List.length (dedupe slots))
                        ]
                        ()
            , test "a small model breaks into each of its parts once" <|
                \_ ->
                    Expect.equal (List.range 0 6) (List.filterMap (Wreck.partSlot 7) (List.range 0 (Wreck.pieceCount - 1)))
            , test "the player's own ship breaks into its own build's parts" <|
                \_ ->
                    let
                        own =
                            Wreck.Ship { name = Boats.default.name, scale = 1 }
                    in
                    Expect.equal True
                        (List.all (\piece -> indexOf piece.look >= 0 && piece.look == Wreck.ShipPart own (indexOf piece.look)) (Wreck.breakUp own fresh.state))
            ]
        ]


dedupe : List Int -> List Int
dedupe xs =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        xs
