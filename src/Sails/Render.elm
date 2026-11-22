module Sails.Render exposing
    ( RenderBoat
    , renderShipyard
    , renderWorld
    )

{-| Draws the sailing world: sea, sky, blocky islands, buoys, the finish
gate, floating targets, cannonballs and the toy boats. Every prop is a small
procedural mesh built from bricks (boxes, studs and prisms).
-}

import Html exposing (Html)
import Html.Attributes as Attr
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.OceanShader as Ocean exposing (Ctx, PropVertex)
import Sails.Types
    exposing
        ( Boat
        , BoatKind(..)
        , BuoyKind(..)
        , Cannonball
        , CombatImpact
        , Course
        , SailingState
        , Target
        , Waypoint
        )
import WebGL



-- PUBLIC


{-| A boat to draw: its look, its state, and an optional sail colour
override (used to tell multiplayer boats apart).
-}
type alias RenderBoat =
    { boat : Boat
    , state : SailingState
    , sailTint : Maybe Vec3
    }


{-| Render the full race scene.
-}
renderWorld :
    { eye : Vec3, target : Vec3 }
    ->
        { course : Course
        , nextIndex : Int
        , boats : List RenderBoat
        , guide : Maybe { from : Vec3, to : Vec3 }
        , targets : List Target
        , balls : List Cannonball
        , impacts : List CombatImpact
        , time : Float
        }
    -> Int
    -> Int
    -> Html msg
renderWorld cam scene width height =
    let
        ctx =
            Ocean.makeCtx cam scene.time width height

        entities =
            [ Ocean.skyEntity ctx
            , Ocean.oceanEntity ctx
            ]
                ++ islandEntities ctx scene.course
                ++ List.concatMap (buoyEntities ctx scene.time scene.nextIndex scene.course.waypoints) scene.course.waypoints
                ++ List.concatMap (targetEntities ctx scene.time) scene.targets
                ++ List.concatMap (boatEntities ctx) scene.boats
                ++ guideEntities ctx scene.guide
                ++ List.map (ballEntity ctx) scene.balls
                ++ List.map (impactEntity ctx) scene.impacts
    in
    canvas width height entities


{-| Shipyard preview: one boat on the water, seen from the side.
-}
renderShipyard : Boat -> Float -> Int -> Int -> Html msg
renderShipyard boat time width height =
    let
        aspect =
            toFloat (max 1 width) / toFloat (max 1 height)

        -- Pull back on narrow portrait screens so the whole boat fits.
        dist =
            if aspect < 1 then
                13 / max 0.45 aspect

            else
                12

        angle =
            time * 0.35

        cam =
            { eye = vec3 (sin angle * dist) (dist * 0.5) (cos angle * dist)
            , target = vec3 0 1.6 0
            }

        ctx =
            Ocean.makeCtx cam time width height

        state =
            { position = vec3 0 0 0
            , heading = 0
            , speed = 0
            , steer = 0
            , sailPower = 1
            , sailAngle = 25
            , heel = 0
            , pitch = 2 * sin (time * 1.7)
            , bumped = False
            }
    in
    canvas width
        height
        ([ Ocean.skyEntity ctx, Ocean.oceanEntity ctx ]
            ++ boatEntities ctx { boat = boat, state = state, sailTint = Nothing }
        )


canvas : Int -> Int -> List WebGL.Entity -> Html msg
canvas width height entities =
    WebGL.toHtmlWith
        [ WebGL.alpha False
        , WebGL.antialias
        , WebGL.depth 1
        , WebGL.clearColor 0.75 0.88 0.97 1
        ]
        [ Attr.width width
        , Attr.height height
        , Attr.class "w-full h-full block"
        ]
        entities



-- ENTITIES


white : Vec3
white =
    vec3 1 1 1


boatEntities : Ctx -> RenderBoat -> List WebGL.Entity
boatEntities ctx rb =
    let
        s =
            rb.state

        p =
            Vec3.toRecord s.position

        base =
            Mat4.makeTranslate3 p.x p.y p.z
                |> rotY (degrees s.heading)
                |> rotZ (degrees -s.heel)
                |> rotX (degrees -s.pitch)

        sailColor =
            Maybe.withDefault rb.boat.sailColor rb.sailTint

        sail mastZ size =
            ocean ctx
                (base
                    |> Mat4.translate3 0 0 mastZ
                    |> rotY (degrees s.sailAngle)
                    |> Mat4.scale3 size size size
                )
                sailColor
                sailMesh
    in
    ocean ctx base white (hullMesh rb.boat.kind)
        :: (case rb.boat.kind of
                Schooner ->
                    [ sail 0.8 1.0, sail -0.8 0.85 ]

                _ ->
                    [ sail 0.2 1.0 ]
           )


ocean : Ctx -> Mat4 -> Vec3 -> WebGL.Mesh PropVertex -> WebGL.Entity
ocean =
    Ocean.propEntity


buoyEntities : Ctx -> Float -> Int -> List Waypoint -> Waypoint -> List WebGL.Entity
buoyEntities ctx time nextIndex all wp =
    let
        p =
            Vec3.toRecord wp.position

        isNext =
            wp.index == nextIndex

        passed =
            wp.index < nextIndex

        bob =
            0.2 * sin (time * 2 + toFloat wp.index)

        at =
            Mat4.makeTranslate3 p.x bob p.z

        color =
            if passed then
                vec3 0.55 0.58 0.62

            else
                case wp.kind of
                    PortBuoy ->
                        vec3 0.9 0.15 0.12

                    StarboardBuoy ->
                        vec3 0.1 0.72 0.25

                    FinishGate ->
                        vec3 1 0.8 0.1

        pulse =
            1 + 0.06 * sin (time * 5)

        ring =
            if isNext then
                [ ocean ctx (Mat4.makeTranslate3 p.x 0.35 p.z |> Mat4.scale3 (wp.radius * pulse) 1 (wp.radius * pulse)) (vec3 1 0.92 0.3) ringMesh
                , ocean ctx (at |> rotY (time * 1.5)) (vec3 1 0.85 0.15) beaconMesh
                ]

            else
                []
    in
    case wp.kind of
        FinishGate ->
            let
                approach =
                    all
                        |> List.filter (\w -> w.index == wp.index - 1)
                        |> List.head
                        |> Maybe.map
                            (\prev ->
                                let
                                    a =
                                        Vec3.toRecord prev.position
                                in
                                atan2 (p.x - a.x) (p.z - a.z)
                            )
                        |> Maybe.withDefault 0
            in
            ocean ctx (Mat4.makeTranslate3 p.x 0 p.z |> rotY approach |> Mat4.scale3 wp.radius 1 1) white gateMesh
                :: ring

        _ ->
            ocean ctx at color buoyMesh :: ring


targetEntities : Ctx -> Float -> Target -> List WebGL.Entity
targetEntities ctx time t =
    if t.sunk && t.sinkAge > 1.6 then
        []

    else
        let
            p =
                Vec3.toRecord t.position

            sink =
                if t.sunk then
                    t.sinkAge * 1.6

                else
                    0
        in
        [ ocean ctx
            (Mat4.makeTranslate3 p.x (p.y - sink) p.z
                |> rotY (time * 0.4 + toFloat t.id)
                |> rotZ (sink * 0.5)
            )
            white
            targetMesh
        ]


islandEntities : Ctx -> Course -> List WebGL.Entity
islandEntities ctx course =
    List.indexedMap
        (\i isl ->
            ocean ctx
                (Mat4.makeTranslate3 isl.x -0.6 isl.z |> Mat4.scale3 isl.radius isl.height isl.radius)
                white
                islandMesh
                :: (if i == 0 then
                        [ ocean ctx (Mat4.makeTranslate3 isl.x (isl.height - 0.6) isl.z) white lighthouseMesh ]

                    else
                        []
                   )
        )
        course.islands
        |> List.concat


guideEntities : Ctx -> Maybe { from : Vec3, to : Vec3 } -> List WebGL.Entity
guideEntities ctx guide =
    case guide of
        Just g ->
            let
                a =
                    Vec3.toRecord g.from

                b =
                    Vec3.toRecord g.to

                dx =
                    b.x - a.x

                dz =
                    b.z - a.z

                d =
                    sqrt (dx * dx + dz * dz)
            in
            if d > 14 then
                [ ocean ctx
                    (Mat4.makeTranslate3 (a.x + dx / d * 6.5) 0.45 (a.z + dz / d * 6.5) |> rotY (atan2 dx dz))
                    (vec3 1 0.88 0.2)
                    arrowMesh
                ]

            else
                []

        Nothing ->
            []


ballEntity : Ctx -> Cannonball -> WebGL.Entity
ballEntity ctx ball =
    let
        p =
            Vec3.toRecord ball.position
    in
    ocean ctx (Mat4.makeTranslate3 p.x p.y p.z) white ballMesh


impactEntity : Ctx -> CombatImpact -> WebGL.Entity
impactEntity ctx imp =
    let
        p =
            Vec3.toRecord imp.position

        t =
            clamp 0 1 (imp.age / 0.7)

        h =
            max 0.1 (sin (t * pi)) * 3

        r =
            0.6 + t * 2.2
    in
    ocean ctx
        (Mat4.makeTranslate3 p.x p.y p.z |> Mat4.scale3 r h r)
        (if imp.isHit then
            vec3 1 0.6 0.15

         else
            white
        )
        splashMesh



-- MATRIX HELPERS


rotX : Float -> Mat4 -> Mat4
rotX a m =
    Mat4.rotate a (vec3 1 0 0) m


rotY : Float -> Mat4 -> Mat4
rotY a m =
    Mat4.rotate a (vec3 0 1 0) m


rotZ : Float -> Mat4 -> Mat4
rotZ a m =
    Mat4.rotate a (vec3 0 0 1) m



-- MESH BUILDING


type alias Tri =
    ( PropVertex, PropVertex, PropVertex )


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List Tri
face a b c d col =
    let
        n =
            Vec3.normalize (Vec3.cross (Vec3.sub b a) (Vec3.sub c a))

        v p =
            { position = p, normal = n, color = col }
    in
    [ ( v a, v b, v c ), ( v a, v c, v d ) ]


{-| Axis-aligned box from (x0, y0, z0) to (x1, y1, z1).
-}
box : ( Float, Float, Float ) -> ( Float, Float, Float ) -> Vec3 -> List Tri
box ( x0, y0, z0 ) ( x1, y1, z1 ) col =
    let
        p =
            vec3
    in
    List.concat
        [ face (p x0 y1 z0) (p x0 y1 z1) (p x1 y1 z1) (p x1 y1 z0) col
        , face (p x0 y0 z0) (p x1 y0 z0) (p x1 y0 z1) (p x0 y0 z1) col
        , face (p x0 y0 z1) (p x1 y0 z1) (p x1 y1 z1) (p x0 y1 z1) col
        , face (p x1 y0 z0) (p x0 y0 z0) (p x0 y1 z0) (p x1 y1 z0) col
        , face (p x1 y0 z1) (p x1 y0 z0) (p x1 y1 z0) (p x1 y1 z1) col
        , face (p x0 y0 z0) (p x0 y0 z1) (p x0 y1 z1) (p x0 y1 z0) col
        ]


{-| Upright n-sided prism centred on (cx, cz).
-}
prism : Int -> Float -> ( Float, Float ) -> ( Float, Float ) -> Vec3 -> List Tri
prism n r ( cx, cz ) ( y0, y1 ) col =
    let
        pt i y =
            let
                a =
                    2 * pi * toFloat i / toFloat n
            in
            vec3 (cx + r * sin a) y (cz + r * cos a)

        side i =
            face (pt i y0) (pt (i + 1) y0) (pt (i + 1) y1) (pt i y1) col

        cap i =
            let
                up =
                    { position = vec3 cx y1 cz, normal = vec3 0 1 0, color = col }

                down =
                    { position = vec3 cx y0 cz, normal = vec3 0 -1 0, color = col }

                at y nrm j =
                    { position = pt j y, normal = nrm, color = col }
            in
            [ ( up, at y1 (vec3 0 1 0) i, at y1 (vec3 0 1 0) (i + 1) )
            , ( down, at y0 (vec3 0 -1 0) (i + 1), at y0 (vec3 0 -1 0) i )
            ]
    in
    List.range 0 (n - 1) |> List.concatMap (\i -> side i ++ cap i)


{-| A brick stud on top of a surface at height y.
-}
stud : Float -> Float -> Float -> Vec3 -> List Tri
stud x y z col =
    prism 8 0.15 ( x, z ) ( y, y + 0.1 ) col


{-| Thin triangular panel in the YZ plane (a sail), given three (y, z) corners.
-}
panel : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float -> Vec3 -> List Tri
panel ( ay, az ) ( by, bz ) ( cy, cz ) t col =
    let
        a x =
            vec3 x ay az

        b x =
            vec3 x by bz

        c x =
            vec3 x cy cz

        tri p q r =
            let
                n =
                    Vec3.normalize (Vec3.cross (Vec3.sub q p) (Vec3.sub r p))

                v pos =
                    { position = pos, normal = n, color = col }
            in
            ( v p, v q, v r )
    in
    [ tri (a t) (b t) (c t)
    , tri (a -t) (c -t) (b -t)
    ]
        ++ face (a -t) (b -t) (b t) (a t) col
        ++ face (b -t) (c -t) (c t) (b t) col
        ++ face (c -t) (a -t) (a t) (c t) col


transformTris : Mat4 -> List Tri -> List Tri
transformTris m =
    let
        tv v =
            { v | position = Mat4.transform m v.position, normal = Vec3.normalize (Vec3.sub (Mat4.transform m (Vec3.add v.position v.normal)) (Mat4.transform m v.position)) }
    in
    List.map (\( a, b, c ) -> ( tv a, tv b, tv c ))



-- MESHES


deckColor : Vec3
deckColor =
    vec3 0.86 0.74 0.52


darkWood : Vec3
darkWood =
    vec3 0.35 0.22 0.12


black : Vec3
black =
    vec3 0.12 0.12 0.14


hullMesh : BoatKind -> WebGL.Mesh PropVertex
hullMesh kind =
    case kind of
        Sloop ->
            sloopMesh

        Cutter ->
            cutterMesh

        Schooner ->
            schoonerMesh

        Catamaran ->
            catamaranMesh


monoHull : Vec3 -> Float -> List Tri
monoHull col len =
    let
        stern =
            -len / 2

        bowStart =
            len / 2 - 1.0

        bowTip =
            len / 2

        p =
            vec3

        bow =
            ( { position = p -0.8 0.5 bowStart, normal = vec3 0 1 0, color = deckColor }
            , { position = p 0.8 0.5 bowStart, normal = vec3 0 1 0, color = deckColor }
            , { position = p 0 0.5 bowTip, normal = vec3 0 1 0, color = deckColor }
            )
                :: face (p -0.8 -0.3 bowStart) (p 0 -0.3 bowTip) (p 0 0.5 bowTip) (p -0.8 0.5 bowStart) col
                ++ face (p 0.8 -0.3 bowStart) (p 0 -0.3 bowTip) (p 0 0.5 bowTip) (p 0.8 0.5 bowStart) col
    in
    box ( -0.8, -0.3, stern ) ( 0.8, 0.5, bowStart ) col
        ++ bow
        ++ box ( -0.7, 0.5, stern + 0.1 ) ( 0.7, 0.58, bowStart ) deckColor
        ++ box ( -0.55, 0.58, stern + 0.15 ) ( 0.55, 1.0, stern + 0.95 ) col
        ++ List.concatMap (\( x, z ) -> stud x 1.0 z col) [ ( -0.25, stern + 0.35 ), ( 0.25, stern + 0.35 ), ( -0.25, stern + 0.75 ), ( 0.25, stern + 0.75 ) ]
        ++ box ( 0.8, 0.15, -0.25 ) ( 1.15, 0.4, 0.25 ) black
        ++ box ( -1.15, 0.15, -0.25 ) ( -0.8, 0.4, 0.25 ) black


mast : Float -> Float -> List Tri
mast z h =
    box ( -0.08, 0.5, z - 0.08 ) ( 0.08, h, z + 0.08 ) darkWood
        ++ box ( 0, h - 0.45, z - 0.02 ) ( 0.02, h - 0.1, z + 0.55 ) (vec3 0.9 0.15 0.15)


sloopMesh : WebGL.Mesh PropVertex
sloopMesh =
    WebGL.triangles
        (monoHull (vec3 0.55 0.33 0.16) 4.2
            ++ mast 0.2 4.8
            ++ panel ( 4.3, 0.3 ) ( 1.0, 0.3 ) ( 1.0, 1.9 ) 0.03 (vec3 0.97 0.96 0.9)
        )


cutterMesh : WebGL.Mesh PropVertex
cutterMesh =
    WebGL.triangles
        (monoHull (vec3 0.12 0.35 0.7) 3.8
            ++ mast 0.2 4.2
            ++ panel ( 3.8, 0.3 ) ( 1.0, 0.3 ) ( 1.0, 1.7 ) 0.03 (vec3 0.98 0.85 0.3)
        )


schoonerMesh : WebGL.Mesh PropVertex
schoonerMesh =
    WebGL.triangles
        (monoHull (vec3 0.22 0.22 0.25) 4.8
            ++ mast 0.8 5.2
            ++ mast -0.8 4.4
        )


catamaranMesh : WebGL.Mesh PropVertex
catamaranMesh =
    let
        col =
            vec3 0.95 0.95 0.95

        pontoon x =
            box ( x - 0.32, -0.3, -2.1 ) ( x + 0.32, 0.35, 1.7 ) col
                ++ box ( x - 0.2, -0.3, 1.7 ) ( x + 0.2, 0.3, 2.2 ) col
    in
    WebGL.triangles
        (pontoon -1.0
            ++ pontoon 1.0
            ++ box ( -1.0, 0.35, -1.4 ) ( 1.0, 0.5, 0.9 ) (vec3 0.2 0.7 0.85)
            ++ List.concatMap (\( x, z ) -> stud x 0.5 z (vec3 0.2 0.7 0.85)) [ ( -0.4, -1.0 ), ( 0.4, -1.0 ), ( -0.4, -0.5 ), ( 0.4, -0.5 ) ]
            ++ mast 0.2 5.0
        )


{-| Main sail around a mast at the origin; the boom points aft (-Z).
Painted white so the boat's sail colour comes through the tint.
-}
sailMesh : WebGL.Mesh PropVertex
sailMesh =
    WebGL.triangles
        (panel ( 4.4, -0.1 ) ( 0.9, -0.1 ) ( 0.9, -2.3 ) 0.04 white
            ++ box ( -0.05, 0.8, -2.4 ) ( 0.05, 0.9, -0.05 ) darkWood
        )


buoyMesh : WebGL.Mesh PropVertex
buoyMesh =
    WebGL.triangles
        (prism 8 1.0 ( 0, 0 ) ( -0.6, 0.8 ) white
            ++ box ( -0.4, 0.8, -0.4 ) ( 0.4, 2.6, 0.4 ) white
            ++ box ( -0.42, 1.5, -0.42 ) ( 0.42, 1.9, 0.42 ) (vec3 0.95 0.95 0.95)
            ++ stud 0 2.6 0 white
        )


beaconMesh : WebGL.Mesh PropVertex
beaconMesh =
    WebGL.triangles
        (box ( -0.08, 2.6, -0.08 ) ( 0.08, 8.5, 0.08 ) (vec3 0.9 0.9 0.9)
            ++ box ( 0, 7.0, -0.04 ) ( 1.8, 8.4, 0.04 ) white
        )


ringMesh : WebGL.Mesh PropVertex
ringMesh =
    let
        n =
            40

        pt r i =
            let
                a =
                    2 * pi * toFloat i / toFloat n
            in
            vec3 (r * sin a) 0 (r * cos a)

        seg i =
            if modBy 2 i == 0 then
                face (pt 0.9 i) (pt 0.9 (i + 1)) (pt 1.0 (i + 1)) (pt 1.0 i) white
                    |> List.map (\( a, b, c ) -> ( a, c, b ))
                    |> List.map (\( a, b, c ) -> ( { a | normal = vec3 0 1 0 }, { b | normal = vec3 0 1 0 }, { c | normal = vec3 0 1 0 } ))

            else
                []
    in
    WebGL.triangles (List.range 0 (n - 1) |> List.concatMap seg)


gateMesh : WebGL.Mesh PropVertex
gateMesh =
    let
        -- Scaled in X by the gate radius: posts sit at x = -1 and x = 1.
        post x =
            box ( x - 0.06, -0.5, -0.5 ) ( x + 0.06, 7.0, 0.5 ) white

        checker i =
            let
                x0 =
                    -1 + toFloat i * 0.2
            in
            box ( x0, 6.0, -0.15 )
                ( x0 + 0.2, 7.2, 0.15 )
                (if modBy 2 i == 0 then
                    black

                 else
                    white
                )
    in
    WebGL.triangles
        (post -1 ++ post 1 ++ List.concatMap checker (List.range 0 9))


targetMesh : WebGL.Mesh PropVertex
targetMesh =
    WebGL.triangles
        (box ( -0.8, -0.4, -0.8 ) ( 0.8, 0.5, 0.8 ) (vec3 0.6 0.4 0.2)
            ++ box ( -0.82, 0.05, -0.82 ) ( 0.82, 0.15, 0.82 ) darkWood
            ++ prism 12 0.7 ( 0, 0 ) ( 0.5, 0.62 ) (vec3 0.9 0.12 0.12)
            ++ prism 12 0.47 ( 0, 0 ) ( 0.62, 0.7 ) white
            ++ prism 12 0.22 ( 0, 0 ) ( 0.7, 0.78 ) (vec3 0.9 0.12 0.12)
        )


islandMesh : WebGL.Mesh PropVertex
islandMesh =
    -- Unit island (radius 1, height 1), scaled per island.
    WebGL.triangles
        (prism 12 1.0 ( 0, 0 ) ( -0.5, 0.18 ) (vec3 0.93 0.84 0.58)
            ++ prism 12 0.8 ( 0, 0 ) ( 0.18, 0.55 ) (vec3 0.35 0.68 0.3)
            ++ prism 12 0.5 ( 0.1, -0.05 ) ( 0.55, 1.0 ) (vec3 0.27 0.56 0.24)
            ++ prism 6 0.25 ( -0.35, 0.3 ) ( 0.55, 0.8 ) (vec3 0.55 0.55 0.58)
        )


lighthouseMesh : WebGL.Mesh PropVertex
lighthouseMesh =
    let
        band i =
            box ( -1.2, toFloat i * 1.6, -1.2 )
                ( 1.2, toFloat (i + 1) * 1.6, 1.2 )
                (if modBy 2 i == 0 then
                    vec3 0.95 0.95 0.95

                 else
                    vec3 0.85 0.15 0.15
                )
    in
    WebGL.triangles
        (List.concatMap band (List.range 0 4)
            ++ box ( -1.5, 8.0, -1.5 ) ( 1.5, 8.4, 1.5 ) black
            ++ box ( -0.9, 8.4, -0.9 ) ( 0.9, 9.8, 0.9 ) (vec3 1 0.9 0.4)
            ++ prism 4 1.3 ( 0, 0 ) ( 9.8, 10.6 ) (vec3 0.85 0.15 0.15)
        )


arrowMesh : WebGL.Mesh PropVertex
arrowMesh =
    let
        arm sign =
            box ( -0.25, 0, -1.6 ) ( 0.25, 0.25, 0 ) white
                |> transformTris (Mat4.makeRotate (sign * 0.75) (vec3 0 1 0))
    in
    WebGL.triangles
        (arm 1 ++ arm -1 ++ (arm 1 ++ arm -1 |> transformTris (Mat4.makeTranslate3 0 0 -1.4)))


ballMesh : WebGL.Mesh PropVertex
ballMesh =
    WebGL.triangles (prism 6 0.35 ( 0, 0 ) ( -0.3, 0.3 ) black)


splashMesh : WebGL.Mesh PropVertex
splashMesh =
    -- Unit splash: a short stack of shrinking rings.
    WebGL.triangles
        (prism 8 1.0 ( 0, 0 ) ( 0, 0.35 ) white
            ++ prism 8 0.65 ( 0, 0 ) ( 0.35, 0.7 ) white
            ++ prism 8 0.3 ( 0, 0 ) ( 0.7, 1.0 ) white
        )
