module CannonPickTest exposing (suite)

{-| A tap on a drawn cannon picks that cannon (its muzzle projected to the
screen with the renderer's view and projection), a tap elsewhere picks none,
and a shot from a picked cannon leaves that muzzle on its side, square to the
hull.
-}

import Expect
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Archipelago as Archipelago
import Sails.CannonPick as CannonPick
import Sails.Physics as Physics
import Sails.Types exposing (SailingState)
import Test exposing (Test, describe, test)


stateAt : Float -> SailingState
stateAt heading =
    let
        s =
            Physics.initSailingState Archipelago.defaultCourse
    in
    { s | heading = heading, speed = 4 }


{-| A follow-style camera: behind and above the ship, looking ahead of it.
-}
camFor : SailingState -> { eye : Vec3, target : Vec3 }
camFor state =
    let
        rad =
            degrees state.heading

        fwd =
            vec3 (sin rad) 0 (cos rad)
    in
    { eye = Vec3.add state.position (Vec3.add (Vec3.scale -14 fwd) (vec3 0 7 0))
    , target = Vec3.add state.position (Vec3.scale 4 fwd)
    }


starboardGun : { position : Vec3, side : Float }
starboardGun =
    { position = vec3 1.2 1.1 0.4, side = 1 }


portGun : { position : Vec3, side : Float }
portGun =
    { position = vec3 -1.2 1.1 -0.4, side = -1 }


guns : List { position : Vec3, side : Float }
guns =
    [ starboardGun, portGun ]


width : Int
width =
    800


height : Int
height =
    600


screenOf : SailingState -> { position : Vec3, side : Float } -> Maybe { x : Float, y : Float }
screenOf state gun =
    CannonPick.projectToScreen (camFor state) width height (Physics.muzzleWorld state gun)


suite : Test
suite =
    describe "Tap on a cannon"
        [ test "a tap at a projected cannon picks that cannon" <|
            \_ ->
                let
                    state =
                        stateAt 30
                in
                List.map
                    (\gun ->
                        screenOf state gun
                            |> Maybe.andThen (\s -> CannonPick.pickCannon (camFor state) width height state guns { x = s.x + 10, y = s.y - 8 })
                    )
                    guns
                    |> Expect.equal [ Just starboardGun, Just portGun ]
        , test "the two cannons project to distinct points on screen" <|
            \_ ->
                let
                    state =
                        stateAt 30
                in
                case ( screenOf state starboardGun, screenOf state portGun ) of
                    ( Just a, Just b ) ->
                        sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)
                            |> Expect.greaterThan (2 * CannonPick.pickRadius)

                    _ ->
                        Expect.fail "a cannon did not project"
        , test "a tap far from every cannon picks none" <|
            \_ ->
                let
                    state =
                        stateAt 30
                in
                CannonPick.pickCannon (camFor state) width height state guns { x = 20, y = 20 }
                    |> Expect.equal Nothing
        , test "a point behind the camera does not project" <|
            \_ ->
                let
                    state =
                        stateAt 0
                in
                CannonPick.projectToScreen (camFor state) width height (Vec3.add state.position (vec3 0 7 -40))
                    |> Expect.equal Nothing
        , test "the shot from a picked cannon leaves its muzzle on its side, square to the hull" <|
            \_ ->
                let
                    state =
                        stateAt 30

                    check gun =
                        case Physics.fireFromMuzzle gun 0 state 7 of
                            ( [ ball ], _, held ) ->
                                let
                                    rel =
                                        Vec3.toRecord (Vec3.sub ball.velocity (Physics.shipVelocity state))

                                    rad =
                                        degrees state.heading

                                    -- starboard unit vector (cos h, -sin h)
                                    across =
                                        (rel.x * cos rad - rel.z * sin rad) * gun.side

                                    along =
                                        rel.x * sin rad + rel.z * cos rad
                                in
                                Vec3.distance ball.position (Physics.muzzleWorld state gun)
                                    < 1.0e-6
                                    && across
                                    > 20
                                    && abs along
                                    < 1.0e-6
                                    && held
                                    == state.heading
                                    && ball.id
                                    == 7

                            _ ->
                                False
                in
                List.map check guns
                    |> Expect.equal [ True, True ]
        ]
