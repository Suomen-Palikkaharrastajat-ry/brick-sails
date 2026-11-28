module Sails.DamageMarks exposing
    ( Mark
    , Pool
    , alpha
    , colourFor
    , empty
    , entities
    , flash
    , fraction
    , hit
    , markSeconds
    , poolSize
    , step
    )

{-| Durability bars over the structures the cannons hit (`Sails.Structures`,
`Sails.Harbours`): at a glance, how many more hits a struck building or
pier takes.

A hit puts a mark in a small pool (`hit`, at most `poolSize`, the oldest
dropped first; a structure hit again refreshes its own mark). Each mark is
a bar in the world over the structure, turned to face the camera: a dark
backing the full width and a fill as long as the share of its durability
left (`fraction`), green when whole through yellow to red when nearly
down (`colourFor`). It fades in on the hit with a white flash (`flash`),
stays a moment and fades out (`alpha`) after `markSeconds`.

Each mark is one draw call of one shared mesh (`entities`): the fill's
length, its colour, the flash and the fade are uniforms, so a mark costs no
mesh building.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.OceanShader exposing (Ctx)
import WebGL
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


{-| One bar: whose it is (`key`), where it shows (over the structure), the
hits left of the structure's durability (`total`) when it was last hit and
how long ago that was (seconds).
-}
type alias Mark =
    { key : String
    , position : Vec3
    , left : Int
    , total : Int
    , age : Float
    }


{-| The bars showing, latest hit first.
-}
type alias Pool =
    List Mark


{-| No bars.
-}
empty : Pool
empty =
    []


{-| The most bars showing at once.
-}
poolSize : Int
poolSize =
    4


{-| Seconds a bar shows after the hit.
-}
markSeconds : Float
markSeconds =
    3.0


fadeInSeconds : Float
fadeInSeconds =
    0.15


fadeOutSeconds : Float
fadeOutSeconds =
    0.6


flashSeconds : Float
flashSeconds =
    0.25


{-| A hit on the structure `key` at `position`, with `left` of its `total`
hits left: its bar (anew, or refreshed) goes first in the pool.
-}
hit : String -> Vec3 -> Int -> Int -> Pool -> Pool
hit key position left total pool =
    { key = key, position = position, left = max 0 left, total = max 1 total, age = 0 }
        :: List.filter (\m -> m.key /= key) pool
        |> List.take poolSize


{-| Age the bars; those past `markSeconds` are gone.
-}
step : Float -> Pool -> Pool
step dt pool =
    List.filterMap
        (\m ->
            if m.age + dt >= markSeconds then
                Nothing

            else
                Just { m | age = m.age + dt }
        )
        pool


{-| Share (0..1) of the durability left.
-}
fraction : Mark -> Float
fraction m =
    clamp 0 1 (toFloat m.left / toFloat (max 1 m.total))


{-| How opaque a bar is: fading in on the hit, out at the end.
-}
alpha : Mark -> Float
alpha m =
    clamp 0 1 (min (m.age / fadeInSeconds) ((markSeconds - m.age) / fadeOutSeconds))


{-| How white the fill flashes (1 on the hit, gone after a quarter second).
-}
flash : Mark -> Float
flash m =
    clamp 0 1 (1 - m.age / flashSeconds)


{-| Colour of the fill for a share of durability left: green when whole,
yellow at half, red when nearly down.
-}
colourFor : Float -> Vec3
colourFor f =
    let
        green =
            vec3 0.2 0.85 0.25

        yellow =
            vec3 0.98 0.85 0.15

        red =
            vec3 0.92 0.18 0.12

        mix a b t =
            Vec3.add (Vec3.scale (1 - t) a) (Vec3.scale t b)
    in
    if f >= 0.5 then
        mix yellow green (clamp 0 1 ((f - 0.5) * 2))

    else
        mix red yellow (clamp 0 1 (f * 2))



-- DRAWING


{-| A vertex of the bar: where it is (the bar is 1 wide and 1 high,
centred on the origin), whether it is the fill (1) or the backing (0) and
whether it is on the fill's growing end (1), which the shader moves to
the share of durability left.
-}
type alias BarVertex =
    { position : Vec3
    , fill : Float
    , stretch : Float
    }


type alias BarUniforms =
    { u_model : Mat4
    , u_view : Mat4
    , u_perspective : Mat4
    , u_fraction : Float
    , u_color : Vec3
    , u_flash : Float
    , u_alpha : Float
    }


{-| The bars of the pool, one draw each, for those in view (`inView` on a
point and a radius).
-}
entities : (Vec3 -> Float -> Bool) -> Ctx -> Pool -> List WebGL.Entity
entities inView ctx pool =
    List.filterMap (entity inView ctx) pool


entity : (Vec3 -> Float -> Bool) -> Ctx -> Mark -> Maybe WebGL.Entity
entity inView ctx m =
    let
        p =
            Vec3.toRecord m.position

        e =
            Vec3.toRecord ctx.eye

        distance =
            Vec3.distance ctx.eye m.position

        -- A little wider far off, so it stays readable.
        width =
            clamp 2.2 5.5 (0.08 * distance)

        model =
            Mat4.makeTranslate3 p.x p.y p.z
                |> Mat4.rotate (atan2 (e.x - p.x) (e.z - p.z)) (vec3 0 1 0)
                |> Mat4.scale3 width (0.16 * width) 1
    in
    if inView m.position width then
        Just
            (WebGL.entityWith
                [ DepthTest.less { write = False, near = 0, far = 1 }
                , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
                ]
                barVertexShader
                barFragmentShader
                barMesh
                { u_model = model
                , u_view = ctx.view
                , u_perspective = ctx.perspective
                , u_fraction = fraction m
                , u_color = colourFor (fraction m)
                , u_flash = flash m
                , u_alpha = alpha m
                }
            )

    else
        Nothing


{-| The bar: the backing box, then the fill box a little inside it and
thicker (so it shows in front of the backing from either side), drawn in
that order.
-}
barMesh : WebGL.Mesh BarVertex
barMesh =
    let
        quad fill a b c d =
            [ ( a, b, c ), ( a, c, d ) ]
                |> List.map
                    (\( v1, v2, v3 ) ->
                        ( vertex fill v1, vertex fill v2, vertex fill v3 )
                    )

        vertex fill ( x, y, z ) =
            { position = vec3 x y z
            , fill = fill
            , stretch =
                if fill > 0.5 && x > 0 then
                    1

                else
                    0
            }

        boxOf fill ( x0, y0, z0 ) ( x1, y1, z1 ) =
            List.concat
                [ quad fill ( x0, y0, z1 ) ( x1, y0, z1 ) ( x1, y1, z1 ) ( x0, y1, z1 )
                , quad fill ( x1, y0, z0 ) ( x0, y0, z0 ) ( x0, y1, z0 ) ( x1, y1, z0 )
                , quad fill ( x0, y1, z0 ) ( x0, y1, z1 ) ( x1, y1, z1 ) ( x1, y1, z0 )
                , quad fill ( x0, y0, z0 ) ( x1, y0, z0 ) ( x1, y0, z1 ) ( x0, y0, z1 )
                , quad fill ( x1, y0, z1 ) ( x1, y0, z0 ) ( x1, y1, z0 ) ( x1, y1, z1 )
                , quad fill ( x0, y0, z0 ) ( x0, y0, z1 ) ( x0, y1, z1 ) ( x0, y1, z0 )
                ]
    in
    WebGL.triangles
        (boxOf 0 ( -0.5, -0.5, -0.03 ) ( 0.5, 0.5, 0.03 )
            ++ boxOf 1 ( -0.46, -0.3, -0.06 ) ( 0.46, 0.3, 0.06 )
        )


barVertexShader : WebGL.Shader BarVertex BarUniforms { v_fill : Float }
barVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        attribute float fill;
        attribute float stretch;
        uniform mat4 u_model;
        uniform mat4 u_view;
        uniform mat4 u_perspective;
        uniform float u_fraction;
        varying float v_fill;

        void main () {
            vec3 p = position;
            if (stretch > 0.5) {
                p.x = -0.46 + 0.92 * u_fraction;
            }
            v_fill = fill;
            gl_Position = u_perspective * u_view * u_model * vec4(p, 1.0);
        }
    |]


barFragmentShader : WebGL.Shader {} BarUniforms { v_fill : Float }
barFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 u_color;
        uniform float u_flash;
        uniform float u_alpha;
        varying float v_fill;

        void main () {
            vec3 backing = vec3(0.08, 0.09, 0.11);
            vec3 fill = mix(u_color, vec3(1.0, 1.0, 1.0), u_flash * 0.7);
            vec3 colour = mix(backing, fill, step(0.5, v_fill));
            gl_FragColor = vec4(colour, u_alpha * mix(0.8, 1.0, v_fill));
        }
    |]
