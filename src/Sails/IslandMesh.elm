module Sails.IslandMesh exposing
    ( CourseMesh
    , IslandVertex
    , build
    , forCourse
    , triangles
    )

{-| Procedural stepped 4-tier brick island shells (`Sails.IslandMesh`) shaded
with `Game.BrickLook.surfaceFragmentShader`.

Each island (`Hub`, `HarbourIsle`, `TreasureIsle`, `Skerry`) is built once per
voyage directly from its physics collision `radius`, `height`, and biome/season
`World.islandPalette` (`beach`, `shore`, `grass`, `hill`):

1.  Sloped `beach` foundation rising from below storm wave troughs across the
    shoreline to a flat studded beach terrace at `Physics.beachHeight`.
2.  Stepped `shore` cliff riser and coastal terrace halfway to the grass
    plateau, giving brick-mortar vertical detail between sand and turf.
3.  Grassy plateau (`grass`) at `Physics.grassHeight` wide enough to support
    towns, trees, rocks, barns, and hay bales cleanly.
4.  Central `hill` slope and studded summit cap at `-0.6 + 0.94 * height`
    matching `Sails.Render.landmarkPos`.

Because upward terraces have strict `(0, 1, 0)` normals (`studs = 1`) and steep
risers keep faceted outward normals, `Game.BrickLook.surfaceFragmentShader`
draws studded 16 x 16 baseplates across horizontal shelves and horizontal brick
mortar courses along the cliffs in roughly 180-270 triangles per island instead
of a fixed-size 28,024-triangle LDraw baseplate assembly.

-}

import Game.BrickLook as BrickLook
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Sails.Physics as Physics
import Sails.Types exposing (Course, Island, IslandRole(..))
import Sails.World as World exposing (IslandPalette)
import WebGL


{-| Vertex matching `Game.BrickLook.SurfaceVaryings` in island-local space
(metres around the island's centre; translated to `(isl.x, 0, isl.z)` in the
shader so the stud grid stays locked to world XZ coordinates).
-}
type alias IslandVertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    , studs : Float
    }


{-| Pre-built procedural meshes for every island in a course, cached once per
voyage so zero WebGL buffers are rebuilt per frame.
-}
type alias CourseMesh =
    List ( Island, WebGL.Mesh IslandVertex )


type alias Profile =
    { sectors : Int
    , seabedShare : Float
    , beachOuterShare : Float
    , beachInnerShare : Float
    , shoreStepShare : Float
    , shoreInnerShare : Float
    , grassOuterShare : Float
    , grassInnerShare : Float
    , hillTopShare : Float
    }


type alias TierBand =
    { outerRadius : Float
    , outerY : Float
    , innerRadius : Float
    , innerY : Float
    , color : Vec3
    }


{-| Build the cached island meshes for a whole course (`Course.islands`) using
the map's `World.islandPalette`.
-}
forCourse : Course -> CourseMesh
forCourse course =
    let
        palette =
            World.islandPalette course.map
    in
    List.map (\isl -> ( isl, build palette isl )) course.islands


{-| Compile one island's procedural stepped shell into a `WebGL.Mesh`.
-}
build : IslandPalette -> Island -> WebGL.Mesh IslandVertex
build palette isl =
    WebGL.triangles (triangles palette isl)


{-| Generate the procedural stepped 4-tier triangles for `isl` in island-local
space (`(0, y, 0)` at center), with consistent counter-clockwise outward
winding suitable for back-face culling and unit normals matching each facet.
-}
triangles : IslandPalette -> Island -> List ( IslandVertex, IslandVertex, IslandVertex )
triangles palette isl =
    let
        prof =
            profileFor isl.role

        r =
            isl.radius

        beachH =
            Physics.beachHeight isl

        grassH =
            Physics.grassHeight isl

        shoreH =
            0.5 * (beachH + grassH)

        hillH =
            -0.6 + 0.94 * isl.height

        seabedY =
            min -1.8 (beachH - 1.5)

        beachCol =
            BrickLook.colour palette.beach

        shoreCol =
            BrickLook.colour palette.shore

        grassCol =
            BrickLook.colour palette.grass

        hillCol =
            BrickLook.colour palette.hill

        bands =
            [ { outerRadius = prof.seabedShare * r, outerY = seabedY, innerRadius = prof.beachOuterShare * r, innerY = beachH, color = beachCol }
            , { outerRadius = prof.beachOuterShare * r, outerY = beachH, innerRadius = prof.beachInnerShare * r, innerY = beachH, color = beachCol }
            , { outerRadius = prof.beachInnerShare * r, outerY = beachH, innerRadius = prof.shoreStepShare * r, innerY = shoreH, color = shoreCol }
            , { outerRadius = prof.shoreStepShare * r, outerY = shoreH, innerRadius = prof.shoreInnerShare * r, innerY = shoreH, color = shoreCol }
            , { outerRadius = prof.shoreInnerShare * r, outerY = shoreH, innerRadius = prof.grassOuterShare * r, innerY = grassH, color = grassCol }
            , { outerRadius = prof.grassOuterShare * r, outerY = grassH, innerRadius = prof.grassInnerShare * r, innerY = grassH, color = grassCol }
            , { outerRadius = prof.grassInnerShare * r, outerY = grassH, innerRadius = prof.hillTopShare * r, innerY = hillH, color = hillCol }
            ]

        n =
            prof.sectors

        stepAngle =
            2 * pi / toFloat n

        rawPhase =
            abs (isl.x * 0.017 + isl.z * 0.031)

        phase =
            (rawPhase - toFloat (floor rawPhase)) * stepAngle

        angleAt k =
            phase + toFloat k * stepAngle

        dirAt k =
            let
                a =
                    angleAt (modBy n k)
            in
            ( cos a, sin a )

        stripTris band k =
            let
                ( c0, s0 ) =
                    dirAt k

                ( c1, s1 ) =
                    dirAt (k + 1)

                o0 =
                    vec3 (band.outerRadius * c0) band.outerY (band.outerRadius * s0)

                o1 =
                    vec3 (band.outerRadius * c1) band.outerY (band.outerRadius * s1)

                i0 =
                    vec3 (band.innerRadius * c0) band.innerY (band.innerRadius * s0)

                i1 =
                    vec3 (band.innerRadius * c1) band.innerY (band.innerRadius * s1)
            in
            [ flatTri o0 i0 o1 band.color 1
            , flatTri i0 i1 o1 band.color 1
            ]

        hillTopR =
            prof.hillTopShare * r

        capCenter =
            vec3 0 hillH 0

        capTri k =
            let
                ( c0, s0 ) =
                    dirAt k

                ( c1, s1 ) =
                    dirAt (k + 1)

                t0 =
                    vec3 (hillTopR * c0) hillH (hillTopR * s0)

                t1 =
                    vec3 (hillTopR * c1) hillH (hillTopR * s1)
            in
            flatTri capCenter t1 t0 hillCol 1

        sectors =
            List.range 0 (n - 1)
    in
    List.concatMap (\band -> List.concatMap (stripTris band) sectors) bands
        ++ List.map capTri sectors


profileFor : IslandRole -> Profile
profileFor role =
    case role of
        Hub ->
            -- 18 sides: cos(pi/18) = 0.9848. Hill summit >= 0.335 covers
            -- Fort/Lighthouse corners around landmarkPos (0.2R, 0.1R); grass
            -- >= 0.792 covers Hub town quay corners; beach >= 1.006 covers flags.
            { sectors = 18
            , seabedShare = 1.075
            , beachOuterShare = 1.022
            , beachInnerShare = 0.848
            , shoreStepShare = 0.832
            , shoreInnerShare = 0.816
            , grassOuterShare = 0.804
            , grassInnerShare = 0.41
            , hillTopShare = 0.342
            }

        HarbourIsle ->
            -- 18 sides: grass covers Barn (0.45R), Rock (0.55R), Houses
            -- (0.60R), Trees (0.70R), and HayBale (0.75R); beach covers flags.
            { sectors = 18
            , seabedShare = 1.075
            , beachOuterShare = 1.022
            , beachInnerShare = 0.855
            , shoreStepShare = 0.835
            , shoreInnerShare = 0.815
            , grassOuterShare = 0.795
            , grassInnerShare = 0.37
            , hillTopShare = 0.24
            }

        TreasureIsle ->
            -- 16 sides: beach terrace houses chest at 0.86R; grass holds trees at 0.70R.
            { sectors = 16
            , seabedShare = 1.08
            , beachOuterShare = 1.025
            , beachInnerShare = 0.825
            , shoreStepShare = 0.805
            , shoreInnerShare = 0.785
            , grassOuterShare = 0.765
            , grassInnerShare = 0.48
            , hillTopShare = 0.26
            }

        Skerry ->
            -- 12 sides: chunky bare sea stack tapered up to a craggy peak.
            { sectors = 12
            , seabedShare = 1.08
            , beachOuterShare = 1.036
            , beachInnerShare = 0.85
            , shoreStepShare = 0.77
            , shoreInnerShare = 0.69
            , grassOuterShare = 0.61
            , grassInnerShare = 0.4
            , hillTopShare = 0.18
            }


flatTri : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> ( IslandVertex, IslandVertex, IslandVertex )
flatTri p0 p1 p2 color studs =
    let
        raw =
            Vec3.cross (Vec3.sub p1 p0) (Vec3.sub p2 p0)

        normal =
            if Vec3.lengthSquared raw > 1.0e-12 then
                Vec3.normalize raw

            else
                vec3 0 1 0
    in
    ( { position = p0, normal = normal, color = color, studs = studs }
    , { position = p1, normal = normal, color = color, studs = studs }
    , { position = p2, normal = normal, color = color, studs = studs }
    )
