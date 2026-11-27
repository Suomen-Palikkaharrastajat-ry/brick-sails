module CannonAimTest exposing (suite)

{-| A built cannon fires along its own barrel: a 32828 turned to face the
bow fires ahead along the heading, one facing out to port fires to port and
one facing the stern fires astern. A tap on a cannon sets no turn target, and
the camera's shot bearing is the ball's bearing, so the view turns to look
along the shot while it flies and then swings back to the heading.
-}

import Expect
import I18n exposing (Language(..))
import LDraw.Document as Document
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Pages.Sail as Sail exposing (SailModel, SailMsg(..))
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.CameraRig as CameraRig
import Sails.CannonPick as CannonPick
import Sails.Physics as Physics
import Sails.ShipMesh as ShipMesh exposing (Muzzle)
import Sails.Types exposing (Cannonball, SailingState)
import Test exposing (Test, describe, test)


{-| One 32828 at an LDraw position with an LDraw rotation (nine numbers,
row by row). The bow is LDraw -Z.
-}
cannonAt : String -> String -> Muzzle
cannonAt position rotation =
    let
        doc =
            Document.parse ("1 71 " ++ position ++ " " ++ rotation ++ " 32828.dat\n")

        toBoat =
            Mat4.makeScale3 ShipMesh.studScale ShipMesh.studScale ShipMesh.studScale
    in
    case ShipMesh.cannonMuzzles toBoat (Document.flattenedPlacements doc) of
        m :: _ ->
            m

        [] ->
            { position = vec3 0 0 0, side = 0, direction = vec3 0 0 0 }


{-| Bar (local +x) turned to LDraw -Z: towards the bow.
-}
bowGun : Muzzle
bowGun =
    cannonAt "0 -8 -100" "0 0 1 0 1 0 -1 0 0"


{-| Bar turned to LDraw +Z: towards the stern.
-}
sternGun : Muzzle
sternGun =
    cannonAt "0 -8 100" "0 0 -1 0 1 0 1 0 0"


{-| Bar turned to LDraw -x: out to port.
-}
portGun : Muzzle
portGun =
    cannonAt "-40 -8 0" "-1 0 0 0 1 0 0 0 -1"


stateAt : Float -> SailingState
stateAt heading =
    let
        s =
            Physics.initSailingState Archipelago.defaultCourse
    in
    { s | heading = heading, speed = 4 }


near : Float -> Vec3 -> Vec3 -> Bool
near tol a b =
    Vec3.distance a b < tol


{-| Difference of two bearings in degrees, -180 to 180.
-}
angleOff : Float -> Float -> Float
angleOff a b =
    a - b - 360.0 * toFloat (floor ((a - b + 180.0) / 360.0))


{-| The ball's velocity relative to the ship, levelled to a unit vector.
-}
flatDir : SailingState -> Vec3 -> Vec3
flatDir state v =
    let
        r =
            Vec3.toRecord (Vec3.sub v (Physics.shipVelocity state))
    in
    Vec3.normalize (vec3 r.x 0 r.z)


fire : Muzzle -> SailingState -> Maybe Cannonball
fire gun state =
    case Physics.fireFromMuzzle gun 0 state 1 of
        ( [ ball ], _, _ ) ->
            Just ball

        _ ->
            Nothing


frame : Float
frame =
    1 / 60


step : SailModel -> SailModel
step m =
    Tuple.first (Sail.update (Tick (frame * 1000)) m)


steps : Int -> SailModel -> SailModel
steps n m =
    List.foldl (\_ x -> step x) m (List.range 1 n)


{-| A voyage under way (the race starts at once), with one built cannon and
a pending broadside turn that a cannon shot must clear.
-}
racingWith : Muzzle -> SailModel
racingWith gun =
    let
        m =
            steps 200 (Sail.initSail En Boats.default Archipelago.defaultCourse True Nothing Nothing { available = False, enabled = False } 800 600)
    in
    { m | cannons = [ gun ], cannonCooldown = 0, combatTurnTarget = Just (m.state.heading + 90) }


{-| Tap the drawn cannon of `m` (where the renderer's view puts it).
-}
tapGun : Muzzle -> SailModel -> Maybe SailModel
tapGun gun m =
    CannonPick.projectToScreen (CameraRig.followView m.follow) m.viewportWidth m.viewportHeight (Physics.muzzleWorld m.state gun)
        |> Maybe.map (\s -> Tuple.first (Sail.update (PointerFire { x = s.x, y = s.y, pointerType = "mouse" }) m))


{-| Where the camera looks: its yaw plus the shot bias.
-}
viewBearing : SailModel -> Float
viewBearing m =
    m.follow.yaw + m.follow.broadsideBias


suite : Test
suite =
    describe "Cannons fire along their barrels"
        [ test "a 32828 on the bow faces forward in boat space" <|
            \_ ->
                Expect.equal True (near 1.0e-6 bowGun.direction (vec3 0 0 1))
        , test "a bow cannon's shot points along the hull's heading" <|
            \_ ->
                let
                    state =
                        stateAt 30
                in
                case fire bowGun state of
                    Just ball ->
                        Expect.equal ( True, True )
                            ( near 1.0e-6 (flatDir state ball.velocity) (vec3 (sin (degrees 30)) 0 (cos (degrees 30)))
                            , abs (angleOff (Physics.shotBearing state ball) 30) < 1.0e-6
                            )

                    Nothing ->
                        Expect.fail "no ball"
        , test "the shot leaves at 22 m/s level with a 5.5 m/s lob and the ship's way on" <|
            \_ ->
                let
                    state =
                        stateAt 30
                in
                case fire bowGun state of
                    Just ball ->
                        let
                            rel =
                                Vec3.toRecord (Vec3.sub ball.velocity (Physics.shipVelocity state))
                        in
                        Expect.equal ( True, True )
                            ( abs (sqrt (rel.x * rel.x + rel.z * rel.z) - 22) < 1.0e-6
                            , abs (rel.y - 5.5) < 1.0e-6
                            )

                    Nothing ->
                        Expect.fail "no ball"
        , test "a port cannon fires to port along its barrel" <|
            \_ ->
                let
                    state =
                        stateAt 30

                    rad =
                        degrees 30
                in
                case fire portGun state of
                    Just ball ->
                        Expect.equal ( -1, True, True )
                            ( portGun.side
                            , near 1.0e-6 portGun.direction (vec3 -1 0 0)
                            , near 1.0e-6 (flatDir state ball.velocity) (vec3 (0 - cos rad) 0 (sin rad))
                            )

                    Nothing ->
                        Expect.fail "no ball"
        , test "a stern cannon fires aft" <|
            \_ ->
                let
                    state =
                        stateAt 30
                in
                case fire sternGun state of
                    Just ball ->
                        Expect.equal ( True, True )
                            ( near 1.0e-6 (flatDir state ball.velocity) (vec3 (0 - sin (degrees 30)) 0 (0 - cos (degrees 30)))
                            , abs (angleOff (Physics.shotBearing state ball) 210) < 1.0e-6
                            )

                    Nothing ->
                        Expect.fail "no ball"
        , test "a cannon shot holds the heading" <|
            \_ ->
                let
                    state =
                        stateAt 30
                in
                case Physics.fireFromMuzzle bowGun 0 state 1 of
                    ( _, _, held ) ->
                        Expect.within (Expect.Absolute 1.0e-9) 30 held
        , test "a tap on a bow cannon fires ahead, sets no turn target and points the camera along the shot" <|
            \_ ->
                let
                    m =
                        racingWith bowGun
                in
                case tapGun bowGun m of
                    Just fired ->
                        case List.reverse fired.cannonballs of
                            ball :: _ ->
                                let
                                    bearing =
                                        Physics.shotBearing m.state ball

                                    flying =
                                        steps 60 fired
                                in
                                Expect.equal ( ( List.length fired.cannonballs - List.length m.cannonballs, fired.combatTurnTarget ), ( True, True, True ) )
                                    ( ( 1, Nothing )
                                    , ( abs (angleOff bearing m.state.heading) < 1.0e-6
                                      , abs (angleOff fired.follow.shootBearing bearing) < 1.0e-6
                                      , abs (angleOff (viewBearing flying) bearing) < 8
                                      )
                                    )

                            [] ->
                                Expect.fail "no ball"

                    Nothing ->
                        Expect.fail "the cannon is not on screen"
        , test "a tap on a stern cannon turns the camera to look aft, then back to the heading" <|
            \_ ->
                let
                    m =
                        racingWith sternGun
                in
                case tapGun sternGun m of
                    Just fired ->
                        let
                            flying =
                                steps 60 fired

                            after =
                                steps 360 fired
                        in
                        Expect.equal ( Nothing, True, True )
                            ( fired.combatTurnTarget
                            , abs (angleOff (viewBearing flying) fired.follow.shootBearing) < 15
                            , abs after.follow.broadsideBias < 5
                            )

                    Nothing ->
                        Expect.fail "the cannon is not on screen"
        , test "the cannon-shot bias looks along the shot ahead and astern and frames a side shot as a broadside" <|
            \_ ->
                List.map CameraRig.cannonShotBias [ 0, 20, -20, 90, -90, 180, -180 ]
                    |> Expect.equal [ 0, 20, -20, 65, -65, 180, -180 ]
        , test "a broadside still keeps its 40 to 65 degree bias" <|
            \_ ->
                let
                    boat =
                        vec3 0 0 0

                    f =
                        List.foldl (\_ x -> CameraRig.stepFollow frame 1.5 boat 0 0 Nothing x)
                            (CameraRig.setShootFocus 0 1.8 (CameraRig.initFollow boat 0))
                            (List.range 1 90)
                in
                Expect.within (Expect.Absolute 0.5) 40 f.broadsideBias
        ]
