module PiratesTest exposing (suite)

{-| Pirate ships: where they patrol (seeded, clear of the harbours, the
start and the islands, so the course can always be finished), how they
move (round their circle at a steady speed, facing the way they go), and
what they do in a race (board a boat that comes close, sink when hit).
-}

import Expect
import Game.MapParams as MapParams
import I18n exposing (Language(..))
import Math.Vector3 as Vec3
import Pages.Sail as Sail exposing (SailModel, SailMsg(..))
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.Physics as Physics
import Sails.Pirates as Pirates exposing (Pirate)
import Sails.Types exposing (Course, Faction(..))
import Test exposing (Test, describe, test)


course : Sails.Types.Course
course =
    Archipelago.defaultCourse


seeds : List Int
seeds =
    [ 1, 2, 3, 42, 777, 12345, 99991, 2024 ]


times : List Float
times =
    List.map (\i -> toFloat i * 0.5) (List.range 0 240)


flat : Vec3.Vec3 -> ( Float, Float )
flat v =
    let
        r =
            Vec3.toRecord v
    in
    ( r.x, r.z )


dist : ( Float, Float ) -> ( Float, Float ) -> Float
dist ( ax, az ) ( bx, bz ) =
    sqrt ((ax - bx) ^ 2 + (az - bz) ^ 2)


everywhere : (( Float, Float ) -> Bool) -> Bool
everywhere ok =
    List.all
        (\seed ->
            List.all
                (\p -> List.all (\t -> ok (flat (Pirates.stateAt t p).position)) times)
                (Pirates.fleet seed course)
        )
        seeds


fresh : SailModel
fresh =
    Sail.initSail En Boats.default course True Nothing Nothing { available = False, enabled = False } 800 600


{-| Under way, with the boat moved onto a pirate's track at the time of the
next frame.
-}
alongside : Pirate -> SailModel
alongside p =
    moveOnto p (racing fresh)


moveOnto : Pirate -> SailModel -> SailModel
moveOnto p m =
    let
        s =
            m.state
    in
    { m | state = { s | position = (Pirates.stateAt (m.simTime + frame) p).position, speed = 0 } }


racing : SailModel -> SailModel
racing m =
    -- Sail out the 3 s countdown and a little more.
    List.foldl (\_ x -> step x) m (List.range 1 200)


frame : Float
frame =
    1 / 60


step : SailModel -> SailModel
step m =
    Tuple.first (Sail.update (Tick (frame * 1000)) m)


suite : Test
suite =
    describe "Pirates"
        [ describe "placement"
            [ test "a race has three pirates on three different legs" <|
                \_ ->
                    Expect.equal [ 3, 3, 3 ]
                        (List.map (\s -> Pirates.fleet s course |> List.map .id |> List.sortBy identity |> dedupe |> List.length) [ 1, 42, 2024 ])
            , test "the same seed gives the same pirates" <|
                \_ -> Expect.equal (Pirates.fleet 42 course) (Pirates.fleet 42 course)
            , test "different seeds give different fleets" <|
                \_ ->
                    seeds
                        |> List.map (\s -> Pirates.fleet s course)
                        |> dedupe
                        |> List.length
                        |> Expect.atLeast 3
            , test "on every seat's route of many maps, enough legs between harbours have room for a patrol" <|
                \_ ->
                    sweepCourses
                        |> List.filter (\c -> List.length (Pirates.patrolRoutes c) < Pirates.fleetSize)
                        |> List.map .id
                        |> Expect.equal []
            , test "a pirate never comes near a harbour's mooring circle" <|
                \_ ->
                    everywhere
                        (\pos -> List.all (\w -> dist pos (flat w.position) >= w.radius + Pirates.harbourClearance - 0.01) course.waypoints)
                        |> Expect.equal True
            , test "a pirate never comes near the start" <|
                \_ ->
                    everywhere (\pos -> dist pos (flat course.startPosition) >= Physics.harbourRadius + Pirates.harbourClearance - 0.01)
                        |> Expect.equal True
            , test "a pirate never sails onto an island" <|
                \_ ->
                    everywhere (\pos -> List.all (\i -> dist pos ( i.x, i.z ) > i.radius) course.islands)
                        |> Expect.equal True
            , test "each patrol is centred on the leg it guards, so it crosses the route" <|
                \_ ->
                    let
                        harbours =
                            List.map (.position >> flat) course.waypoints

                        mids =
                            List.map2 (\( ax, az ) ( bx, bz ) -> ( (ax + bx) / 2, (az + bz) / 2 )) harbours (List.drop 1 harbours)
                    in
                    Pirates.patrolRoutes course
                        |> List.all (\p -> List.member ( p.centreX, p.centreZ ) mids)
                        |> Expect.equal True
            ]
        , describe "movement"
            [ test "a pirate keeps to its circle" <|
                \_ ->
                    Pirates.fleet 7 course
                        |> List.all (\p -> List.all (\t -> abs (dist (flat (Pirates.stateAt t p).position) ( p.centreX, p.centreZ ) - p.radius) < 0.001) times)
                        |> Expect.equal True
            , test "a pirate sails at its steady speed" <|
                \_ ->
                    Pirates.fleet 7 course
                        |> List.all
                            (\p ->
                                let
                                    a =
                                        Pirates.stateAt 3 p

                                    b =
                                        Pirates.stateAt 3.01 p
                                in
                                abs (dist (flat a.position) (flat b.position) / 0.01 - a.speed) < 0.01 && a.speed > 3 && a.speed < 7
                            )
                        |> Expect.equal True
            , test "a pirate faces the way it sails" <|
                \_ ->
                    Pirates.fleet 7 course
                        |> List.all
                            (\p ->
                                let
                                    a =
                                        Pirates.stateAt 5 p

                                    ( ax, az ) =
                                        flat a.position

                                    ( bx, bz ) =
                                        flat (Pirates.stateAt 5.01 p).position

                                    moving =
                                        atan2 (bx - ax) (bz - az) * 180 / pi
                                in
                                abs (Physics.angleDiff moving a.heading) < 1
                            )
                        |> Expect.equal True
            , test "a pirate is back where it started after a lap" <|
                \_ ->
                    Pirates.fleet 7 course
                        |> List.all (\p -> dist (flat (Pirates.stateAt 2 p).position) (flat (Pirates.stateAt (2 + p.lapSeconds) p).position) < 0.001)
                        |> Expect.equal True
            ]
        , describe "in a race"
            [ test "the race's pirates are the fleet of its seed" <|
                \_ -> Expect.equal Pirates.fleetSize (List.length fresh.pirates)
            , test "a pirate alongside boards the boat: one hit's damage, then a pause" <|
                \_ ->
                    case fresh.pirates of
                        p :: _ ->
                            let
                                start =
                                    alongside p

                                boarded =
                                    step start

                                again =
                                    step (moveOnto p boarded)
                            in
                            Expect.equal ( True, True, False )
                                ( boarded.hull.health < start.hull.health
                                , boarded.pirateCooldown > 0
                                , again.hull.health < boarded.hull.health - 1
                                )

                        [] ->
                            Expect.fail "no pirates"
            , test "a cannon ball that hits a pirate sinks it and counts as a hit" <|
                \_ ->
                    case fresh.pirates of
                        p :: _ ->
                            let
                                m =
                                    racing fresh

                                at =
                                    (Pirates.stateAt (m.simTime + frame) p).position

                                -- A ball just above the pirate, about to come down on it.
                                ball =
                                    { id = 99, sourceSlot = 1, position = Vec3.add at (Vec3.vec3 0 1.5 0), velocity = Vec3.vec3 0 0 0, age = 0 }

                                hit =
                                    step { m | cannonballs = [ ball ] }
                            in
                            Expect.equal ( [ p.id ], m.progress.targetsHit + 1 ) ( hit.piratesSunk, hit.progress.targetsHit )

                        [] ->
                            Expect.fail "no pirates"
            , test "sailing again brings sunk pirates back" <|
                \_ ->
                    Sail.update RestartCourse { fresh | piratesSunk = [ 0, 1 ] }
                        |> Tuple.first
                        |> .piratesSunk
                        |> Expect.equal []
            ]
        , describe "factions"
            [ test "every race has ships from all three factions" <|
                \_ ->
                    seeds
                        |> List.all
                            (\s ->
                                let
                                    factions =
                                        Pirates.fleet s course |> List.map .faction
                                in
                                List.member Pirates factions
                                    && List.member BlueCoats factions
                                    && List.member RedCoats factions
                            )
                        |> Expect.equal True
            , test "each faction ship has its distinctive sail tint" <|
                \_ ->
                    let
                        fleet =
                            Pirates.fleet 42 course

                        tintOf f =
                            fleet
                                |> List.filter (\p -> p.faction == f)
                                |> List.head
                                |> Maybe.map Pirates.sailTint
                    in
                    Expect.equal
                        ( Just (Vec3.vec3 0.08 0.08 0.1)
                        , Just (Vec3.vec3 0.12 0.38 0.85)
                        , Just (Vec3.vec3 0.82 0.16 0.12)
                        )
                        ( tintOf Pirates, tintOf BlueCoats, tintOf RedCoats )
            , test "BlueCoats patrol in Cutters while Pirates and RedCoats sail Schooners" <|
                \_ ->
                    let
                        fleet =
                            Pirates.fleet 42 course

                        shipOf f =
                            fleet
                                |> List.filter (\p -> p.faction == f)
                                |> List.head
                                |> Maybe.map Pirates.shipNameFor
                    in
                    Expect.equal
                        ( Just "Schooner", Just "Cutter", Just "Schooner" )
                        ( shipOf Pirates, shipOf BlueCoats, shipOf RedCoats )
            , test "each faction is assigned the patrol near its island" <|
                \_ ->
                    let
                        fleet =
                            Pirates.fleet 42 course

                        distTo ( ix, iz ) p =
                            sqrt ((p.centreX - ix) ^ 2 + (p.centreZ - iz) ^ 2)

                        home f =
                            Physics.factionTown course f
                                |> Maybe.map (\t -> ( t.island.x, t.island.z ))
                                |> Maybe.withDefault ( 0, 0 )

                        pirateShip =
                            fleet |> List.filter (\p -> p.faction == Pirates) |> List.head

                        blueCoatsShip =
                            fleet |> List.filter (\p -> p.faction == BlueCoats) |> List.head

                        redCoatsShip =
                            fleet |> List.filter (\p -> p.faction == RedCoats) |> List.head
                    in
                    case ( pirateShip, blueCoatsShip, redCoatsShip ) of
                        ( Just p, Just b, Just r ) ->
                            Expect.equal ( True, True, True )
                                ( distTo (home Pirates) p <= distTo (home BlueCoats) p && distTo (home Pirates) p <= distTo (home RedCoats) p
                                , distTo (home BlueCoats) b <= distTo (home Pirates) b && distTo (home BlueCoats) b <= distTo (home RedCoats) b
                                , distTo (home RedCoats) r <= distTo (home Pirates) r && distTo (home RedCoats) r <= distTo (home BlueCoats) r
                                )

                        _ ->
                            Expect.fail "Missing faction patrol ships"
            ]
        ]


{-| Every seat's course of 25 maps of each biome.
-}
sweepCourses : List Course
sweepCourses =
    MapParams.allBiomes
        |> List.concatMap (\b -> List.map (\seed -> MapParams.resolve { biome = MapParams.Exactly b, season = MapParams.Any, weather = MapParams.Any, seed = seed }) (List.range 1 25))
        |> List.concatMap (Archipelago.generate >> Archipelago.seatCourses)


dedupe : List a -> List a
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
