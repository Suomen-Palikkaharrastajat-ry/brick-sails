module CannonChoiceTest exposing (suite)

{-| Choosing and ranging a built cannon's shot, shared by a tap and the F
key: of the candidate cannons the one nearest the foe fires (ties going to
the one nearest the tap), and with the foe in that cannon's arc the ball is
fired at it with the power that fits its distance, so it comes down on the
foe rather than flying over it.
-}

import Expect
import I18n exposing (Language(..))
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Pages.Sail as Sail exposing (SailModel, SailMsg(..))
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.CameraRig as CameraRig
import Sails.CannonPick as CannonPick
import Sails.Physics as Physics
import Sails.Pirates as Pirates
import Sails.ShipMesh exposing (Muzzle)
import Sails.Types exposing (Cannonball, Faction(..), SailingState)
import Test exposing (Test, describe, test)


stateAt : Float -> SailingState
stateAt heading =
    let
        s =
            Physics.initSailingState Archipelago.defaultCourse
    in
    { s | heading = heading, speed = 4 }


starboardGun : Muzzle
starboardGun =
    { position = vec3 1.2 1.1 0.4, side = 1, direction = vec3 1 0 0 }


portGun : Muzzle
portGun =
    { position = vec3 -1.2 1.1 -0.4, side = -1, direction = vec3 -1 0 0 }


{-| A second starboard gun further aft, the same distance out.
-}
aftStarboardGun : Muzzle
aftStarboardGun =
    { position = vec3 1.2 1.1 -0.4, side = 1, direction = vec3 1 0 0 }


{-| A world point `across` m to starboard (negative: to port) and `along` m
ahead of the ship, at the waterline.
-}
offShip : SailingState -> Float -> Float -> Vec3
offShip state across along =
    let
        rad =
            degrees state.heading

        p =
            Vec3.toRecord state.position
    in
    vec3 (p.x + across * cos rad + along * sin rad) 0 (p.z - across * sin rad + along * cos rad)


{-| Where (horizontally) a ball comes down to the height `y`, flying with no
drag under gravity as `Physics.stepProjectiles` moves it.
-}
landingAt : Float -> Cannonball -> Vec3
landingAt y ball =
    let
        p =
            Vec3.toRecord ball.position

        v =
            Vec3.toRecord ball.velocity

        g =
            9.81

        -- p.y + v.y t - g t^2 / 2 = y, the later root
        t =
            (v.y + sqrt (v.y * v.y + 2 * g * (p.y - y))) / g
    in
    vec3 (p.x + v.x * t) y (p.z + v.z * t)


flat : Vec3 -> Vec3
flat v =
    let
        r =
            Vec3.toRecord v
    in
    vec3 r.x 0 r.z


fireAt : Muzzle -> Maybe Vec3 -> SailingState -> Maybe Cannonball
fireAt gun foe state =
    case Physics.fireCannonAt gun foe 0 state 1 of
        ( [ ball ], _, _ ) ->
            Just ball

        _ ->
            Nothing


{-| A pirate that keeps still at `at` (a circle of no radius).
-}
pirateAt : Vec3 -> Pirates.Pirate
pirateAt at =
    { id = 99
    , centreX = Vec3.getX at
    , centreZ = Vec3.getZ at
    , radius = 0
    , lapSeconds = 60
    , phase = 0
    , direction = 1
    , faction = Pirates
    , kind = Pirates.Sloop
    }


step : SailModel -> SailModel
step m =
    Tuple.first (Sail.update (Tick (1000 / 60)) m)


{-| A voyage under way with a port and a starboard gun and, with a `foe`
offset (across, along), one still pirate there and no other.
-}
racing : Maybe ( Float, Float ) -> SailModel
racing foe =
    let
        m =
            List.foldl (\_ x -> step x) (Sail.initSail En Boats.default Archipelago.defaultCourse True Nothing Nothing { available = False, enabled = False } 800 600) (List.range 1 200)
    in
    { m
        | cannons = [ portGun, starboardGun ]
        , cannonCooldown = 0
        , pirates =
            case foe of
                Just ( across, along ) ->
                    [ pirateAt (offShip m.state across along) ]

                Nothing ->
                    []
        , piratesSunk = []
    }


newBall : SailModel -> SailModel -> Maybe Cannonball
newBall before after =
    if List.length after.cannonballs == List.length before.cannonballs + 1 then
        List.head (List.reverse after.cannonballs)

    else
        Nothing


tapGun : Muzzle -> SailModel -> Maybe SailModel
tapGun gun m =
    CannonPick.projectToScreen (CameraRig.followView m.follow) m.viewportWidth m.viewportHeight (Physics.muzzleWorld m.state gun)
        |> Maybe.map (\s -> Tuple.first (Sail.update (PointerFire { x = s.x, y = s.y, pointerType = "touch" }) m))


suite : Test
suite =
    describe "Choosing and ranging a cannon shot"
        [ describe "selectCannon"
            [ test "of several cannons near a tap, the one nearest the foe fires, even if another is nearer the tap" <|
                \_ ->
                    let
                        state =
                            stateAt 30
                    in
                    CannonPick.selectCannon state
                        (Just (offShip state 15 0))
                        [ ( portGun, 2 ), ( starboardGun, 30 ) ]
                        |> Expect.equal (Just starboardGun)
            , test "a foe to port picks the port gun" <|
                \_ ->
                    let
                        state =
                            stateAt 30
                    in
                    CannonPick.selectCannon state
                        (Just (offShip state -15 0))
                        [ ( starboardGun, 2 ), ( portGun, 30 ) ]
                        |> Expect.equal (Just portGun)
            , test "two cannons as near the foe as each other: the one nearer the tap fires" <|
                \_ ->
                    let
                        state =
                            stateAt 30

                        foe =
                            Just (offShip state 15 0)
                    in
                    ( CannonPick.selectCannon state foe [ ( starboardGun, 20 ), ( aftStarboardGun, 5 ) ]
                    , CannonPick.selectCannon state foe [ ( starboardGun, 5 ), ( aftStarboardGun, 20 ) ]
                    )
                        |> Expect.equal ( Just aftStarboardGun, Just starboardGun )
            , test "with no foe the cannon nearest the tap fires" <|
                \_ ->
                    CannonPick.selectCannon (stateAt 30) Nothing [ ( starboardGun, 12 ), ( portGun, 3 ) ]
                        |> Expect.equal (Just portGun)
            , test "no candidates, no cannon" <|
                \_ ->
                    CannonPick.selectCannon (stateAt 30) (Just (offShip (stateAt 30) 15 0)) []
                        |> Expect.equal Nothing
            , test "the foe is the aim point nearest the ship" <|
                \_ ->
                    let
                        state =
                            stateAt 0
                    in
                    CannonPick.nearestFoe state [ offShip state 30 0, offShip state -10 0, offShip state 0 50 ]
                        |> Expect.equal (Just (offShip state -10 0))
            ]
        , describe "range"
            [ test "the launch speed brings the ball down at the target's distance" <|
                \_ ->
                    List.map
                        (\d ->
                            let
                                vh =
                                    Physics.cannonLaunchSpeed d 0.5

                                t =
                                    d / vh

                                -- height gained at the target at the fixed 0.25 rise over run
                                dy =
                                    0.25 * vh * t - 0.5 * 9.81 * t * t
                            in
                            abs (dy + 0.5) < 1.0e-6
                        )
                        [ 8, 12, 18, 24 ]
                        |> Expect.equal [ True, True, True, True ]
            , test "power grows with distance and is clamped to the cannon's full power" <|
                \_ ->
                    let
                        speeds =
                            List.map (\d -> Physics.cannonLaunchSpeed d 0.5) [ 8, 16, 24, 60, 200 ]
                    in
                    Expect.equal ( True, [ Physics.cannonPower, Physics.cannonPower ] )
                        ( List.take 3 speeds == List.sortBy identity (List.take 3 speeds) && List.all (\s -> s < Physics.cannonPower) (List.take 3 speeds)
                        , List.drop 3 speeds
                        )
            , test "full power just reaches the cannon's reach" <|
                \_ ->
                    Physics.cannonLaunchSpeed (Physics.cannonReach 0.5) 0.5
                        |> Expect.within (Expect.Absolute 1.0e-6) Physics.cannonPower
            , test "a foe in the arc and in reach is hit, not overshot" <|
                \_ ->
                    let
                        state =
                            stateAt 30
                    in
                    List.map
                        (\( across, along ) ->
                            let
                                foe =
                                    offShip state across along
                            in
                            case fireAt starboardGun (Just foe) state of
                                Just ball ->
                                    Vec3.distance (flat (landingAt 0.6 ball)) (flat foe) < 1.0e-3

                                Nothing ->
                                    False
                        )
                        [ ( 6, 0 ), ( 12, 3 ), ( 20, -4 ) ]
                        |> Expect.equal [ True, True, True ]
            , test "the ranged shot is shorter than a full-power shot along the barrel" <|
                \_ ->
                    let
                        state =
                            stateAt 30

                        foe =
                            offShip state 10 0

                        dist ball =
                            Vec3.distance (flat (landingAt 0.6 ball)) (flat (Physics.muzzleWorld state starboardGun))
                    in
                    case ( fireAt starboardGun (Just foe) state, fireAt starboardGun Nothing state ) of
                        ( Just ranged, Just full ) ->
                            Expect.lessThan (dist full - 5) (dist ranged)

                        _ ->
                            Expect.fail "no ball"
            , test "a foe beyond reach gets a full-power shot at it that falls short" <|
                \_ ->
                    let
                        state =
                            stateAt 30

                        foe =
                            offShip state 33 0
                    in
                    case fireAt starboardGun (Just foe) state of
                        Just ball ->
                            let
                                v =
                                    Vec3.toRecord ball.velocity
                            in
                            Expect.equal ( True, True )
                                ( abs (sqrt (v.x * v.x + v.z * v.z) - Physics.cannonPower) < 1.0e-6
                                , Vec3.distance (flat (landingAt 0.6 ball)) (flat foe) > 3
                                )

                        Nothing ->
                            Expect.fail "no ball"
            , test "a foe outside the cannon's arc leaves the shot along the barrel" <|
                \_ ->
                    let
                        state =
                            stateAt 30
                    in
                    case ( fireAt starboardGun (Just (offShip state 0 15)) state, fireAt starboardGun Nothing state ) of
                        ( Just aimed, Just plain ) ->
                            Expect.equal plain aimed

                        _ ->
                            Expect.fail "no ball"
            , test "every shot leaves the chosen cannon's muzzle" <|
                \_ ->
                    let
                        state =
                            stateAt 30
                    in
                    fireAt starboardGun (Just (offShip state 12 0)) state
                        |> Maybe.map (\b -> Vec3.distance b.position (Physics.muzzleWorld state starboardGun) < 1.0e-6)
                        |> Expect.equal (Just True)
            ]
        , describe "the F key and a tap share one path"
            [ test "F fires the cannon nearest the foe from its muzzle, ranged onto the foe, with no turn" <|
                \_ ->
                    List.map
                        (\( across, gun ) ->
                            let
                                m =
                                    racing (Just ( across, 0 ))

                                fired =
                                    Tuple.first (Sail.update FireCannons m)
                            in
                            case newBall m fired of
                                Just ball ->
                                    let
                                        foe =
                                            CannonPick.nearestFoe m.state (List.map (\p -> (Pirates.stateAt m.simTime p).position) m.pirates)
                                    in
                                    ( Vec3.distance ball.position (Physics.muzzleWorld m.state gun) < 1.0e-6
                                    , Maybe.map (\f -> Vec3.distance (flat (landingAt (Vec3.getY f + 0.6) ball)) (flat f) < 1.0) foe == Just True
                                    , fired.combatTurnTarget == Nothing
                                    )

                                Nothing ->
                                    ( False, False, False )
                        )
                        [ ( 14, starboardGun ), ( -14, portGun ) ]
                        |> Expect.equal [ ( True, True, True ), ( True, True, True ) ]
            , test "F fires the same ball a tap on the chosen cannon fires" <|
                \_ ->
                    let
                        m =
                            racing (Just ( 14, 0 ))
                    in
                    Expect.equal
                        (newBall m (Tuple.first (Sail.update FireCannons m)))
                        (tapGun starboardGun m |> Maybe.andThen (newBall m))
            , test "with no foe a tap still fires the tapped cannon along its barrel" <|
                \_ ->
                    let
                        m =
                            racing Nothing
                    in
                    tapGun portGun m
                        |> Maybe.andThen (newBall m)
                        |> Maybe.map (\b -> ( Vec3.distance b.position (Physics.muzzleWorld m.state portGun) < 1.0e-6, b.velocity == Physics.cannonVelocity m.state portGun ))
                        |> Expect.equal (Just ( True, True ))
            ]
        ]
