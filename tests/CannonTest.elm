module CannonTest exposing (suite)

{-| The cannonball's flight line from a tap on the sea: an untargeted ball
leaves at 90 degrees to the boat's heading on the tapped side and the ship
holds that heading; a targeted ball is still lobbed at the target.
-}

import Expect
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Archipelago as Archipelago
import Sails.Physics as Physics
import Sails.Types exposing (SailingState)
import Test exposing (Test, describe, test)


stateAt : Float -> SailingState
stateAt heading =
    let
        s =
            Physics.initSailingState Archipelago.defaultCourse
    in
    { s | heading = heading }


{-| Fire with no aim points; returns (horizontal ball velocity, forward, held heading).
-}
fireOpen : Float -> Vec3 -> ( { x : Float, z : Float }, { x : Float, z : Float }, Float )
fireOpen heading aim =
    let
        ( balls, _, newHeading ) =
            Physics.fireTowards 0 (stateAt heading) aim [] 0

        v =
            balls
                |> List.head
                |> Maybe.map (.velocity >> Vec3.toRecord)
                |> Maybe.withDefault { x = 0, y = 0, z = 0 }

        len =
            sqrt (v.x * v.x + v.z * v.z)
    in
    ( { x = v.x / len, z = v.z / len }
    , { x = sin (degrees heading), z = cos (degrees heading) }
    , newHeading
    )


square : Float -> Vec3 -> Expect.Expectation
square heading aim =
    let
        ( v, fwd, newHeading ) =
            fireOpen heading aim

        a =
            Vec3.toRecord aim
    in
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute 1.0e-6) 0 (v.x * fwd.x + v.z * fwd.z)
        , \_ ->
            -- On the tapped side of the hull: the same sign of the
            -- lateral component as the tap direction.
            ((v.x * fwd.z - v.z * fwd.x) * (a.x * fwd.z - a.z * fwd.x))
                |> Expect.greaterThan 0
        , \_ -> Expect.within (Expect.Absolute 1.0e-9) heading newHeading
        ]
        ()


suite : Test
suite =
    describe "Cannon flight"
        [ test "heading 0, tap ahead and to (cos h, -sin h): ball square to the hull on that side" <|
            \_ -> square 0 (vec3 1 0 1)
        , test "heading 0, tap behind on the other side: ball square on that side" <|
            \_ -> square 0 (vec3 -1 0 -0.5)
        , test "heading 0, starboard tap (1, 0, 0): ball flies +x" <|
            \_ ->
                let
                    ( v, _, _ ) =
                        fireOpen 0 (vec3 1 0 0)
                in
                Expect.within (Expect.Absolute 1.0e-6) 1 v.x
        , test "heading 37, tap near the bow: ball still square to the hull" <|
            \_ -> square 37 (vec3 0.7 0 0.9)
        , test "heading -120, tap to the other side: ball still square to the hull" <|
            \_ -> square -120 (vec3 0.3 0 -0.8)
        , test "a target near the line of fire is still lobbed at directly and the ship turns" <|
            \_ ->
                let
                    s =
                        stateAt 0

                    p =
                        Vec3.toRecord s.position

                    target =
                        vec3 (p.x + 10) p.y (p.z + 10)

                    ( balls, _, newHeading ) =
                        Physics.fireTowards 0 s (vec3 1 0 1) [ target ] 0

                    v =
                        balls |> List.head |> Maybe.map (.velocity >> Vec3.toRecord) |> Maybe.withDefault { x = 0, y = 0, z = 0 }
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 1.0e-6) v.x v.z
                    , \_ -> Expect.greaterThan 0 v.x
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) -45 newHeading
                    ]
                    ()
        , test "an untargeted ball keeps the ship's velocity on top of the broadside" <|
            \_ ->
                let
                    s0 =
                        stateAt 30

                    s =
                        { s0 | speed = 8 }

                    shot st =
                        Physics.fireTowards 0 st (vec3 1 0 0) [] 0
                            |> (\( bs, _, _ ) -> bs)
                            |> List.head
                            |> Maybe.map (.velocity >> Vec3.toRecord)
                            |> Maybe.withDefault { x = 0, y = 0, z = 0 }

                    still =
                        shot s0

                    moving =
                        shot s

                    ship =
                        Vec3.toRecord (Physics.shipVelocity s)
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 1.0e-6) (still.x + ship.x) moving.x
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) (still.z + ship.z) moving.z
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) still.y moving.y
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) (8 * sin (degrees 30)) ship.x
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) (8 * cos (degrees 30)) ship.z
                    ]
                    ()
        , test "a targeted lob is aimed from the origin as before, whatever the ship's speed" <|
            \_ ->
                let
                    s0 =
                        stateAt 0

                    p =
                        Vec3.toRecord s0.position

                    target =
                        vec3 (p.x + 10) p.y (p.z + 10)

                    shot st =
                        Physics.fireTowards 0 st (vec3 1 0 1) [ target ] 0
                            |> (\( bs, _, _ ) -> bs)
                            |> List.map .velocity
                in
                Expect.equal (shot s0) (shot { s0 | speed = 8 })
        , test "a ship with a starboard cannon fires a starboard shot from that cannon's muzzle" <|
            \_ ->
                let
                    s =
                        stateAt 30

                    -- Built directly, not from a model: a muzzle 1.2 m to
                    -- starboard, 0.8 m up and 0.5 m aft, plus a port gun.
                    starboardGun =
                        { position = vec3 1.2 0.8 -0.5, side = 1 }

                    portGun =
                        { position = vec3 -1.2 0.8 0.3, side = -1 }

                    ( balls, _, _ ) =
                        Physics.fireTowardsFrom [ portGun, starboardGun ] 0 s (vec3 (cos (degrees 30)) 0 -(sin (degrees 30))) [] 0

                    p =
                        Vec3.toRecord s.position

                    rad =
                        degrees 30

                    expected =
                        { x = p.x + 1.2 * cos rad - 0.5 * sin rad
                        , y = p.y + 0.8
                        , z = p.z - 0.5 * cos rad - 1.2 * sin rad
                        }

                    ( start, vel ) =
                        balls
                            |> List.head
                            |> Maybe.map (\b -> ( Vec3.toRecord b.position, Vec3.toRecord b.velocity ))
                            |> Maybe.withDefault ( { x = 0, y = 0, z = 0 }, { x = 0, y = 0, z = 0 } )

                    still =
                        Physics.fireTowards 0 s (vec3 (cos rad) 0 -(sin rad)) [] 0
                            |> (\( bs, _, _ ) -> bs)
                            |> List.head
                            |> Maybe.map (.velocity >> Vec3.toRecord)
                            |> Maybe.withDefault { x = 0, y = 0, z = 0 }
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 1.0e-6) expected.x start.x
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) expected.y start.y
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) expected.z start.z
                    , \_ -> Expect.equal still vel
                    ]
                    ()
        , test "a targeted lob from a cannon is aimed from the muzzle and still lands on the target" <|
            \_ ->
                let
                    s =
                        stateAt 0

                    p =
                        Vec3.toRecord s.position

                    target =
                        vec3 (p.x + 10) p.y (p.z + 10)

                    gun =
                        { position = vec3 1 0.8 0, side = 1 }

                    ball =
                        Physics.fireTowardsFrom [ gun ] 0 s (vec3 1 0 1) [ target ] 0
                            |> (\( bs, _, _ ) -> bs)
                            |> List.head

                    o =
                        ball |> Maybe.map (.position >> Vec3.toRecord) |> Maybe.withDefault { x = 0, y = 0, z = 0 }

                    v =
                        ball |> Maybe.map (.velocity >> Vec3.toRecord) |> Maybe.withDefault { x = 0, y = 0, z = 0 }
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 1.0e-6) (p.x + 1) o.x
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) (p.y + 0.8) o.y

                    -- The horizontal line from the muzzle runs to the target.
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) ((p.x + 10 - o.x) / (p.z + 10 - o.z)) (v.x / v.z)
                    ]
                    ()
        , test "a ship with no cannons fires from the hull centre as before" <|
            \_ ->
                let
                    s =
                        stateAt 30

                    ( balls, _, _ ) =
                        Physics.fireTowardsFrom [] 0 s (vec3 1 0 0) [] 0

                    ( before, _, _ ) =
                        Physics.fireTowards 0 s (vec3 1 0 0) [] 0
                in
                Expect.all
                    [ \_ -> Expect.equal (List.map .position before) (List.map .position balls)
                    , \_ -> Expect.equal [ Vec3.add s.position (vec3 0 1 0) ] (List.map .position balls)
                    ]
                    ()
        , test "a ball splashes on the storm sea it is drawn on" <|
            \_ ->
                let
                    sea =
                        2.4

                    ball =
                        { id = 7, sourceSlot = 0, position = vec3 3 30 4, velocity = vec3 0 -400 0, age = 0 }

                    ( flying, splashes ) =
                        Physics.stepProjectiles sea 0.1 5 [ ball ]
                in
                case splashes of
                    [ splash ] ->
                        let
                            sp =
                                Vec3.toRecord splash.position
                        in
                        Expect.all
                            [ \_ -> Expect.equal [] flying
                            , \_ -> Expect.within (Expect.Absolute 1.0e-9) (Physics.seaElevation sea sp.x sp.z 5) sp.y
                            , \_ -> Expect.notWithin (Expect.Absolute 0.01) (Physics.waterElevation sp.x sp.z 5) sp.y
                            ]
                            ()

                    _ ->
                        Expect.fail "expected one splash"
        ]
