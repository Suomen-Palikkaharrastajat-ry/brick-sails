module Sails.IslandMesh exposing
    ( CourseMesh
    , FarPieces
    , IslandMeshes
    , IslandVertex
    , Placed
    , build
    , farMeshes
    , forCourse
    , isLandmark
    , proxyTriangles
    , splitFar
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

The island's dressing is planned here too, once per voyage
(`Sails.IslandDressing.plans` of the course's map, the same on every seat):
`forCourse` keeps each island's items for `Sails.Render` to draw in real
bricks when the island is near the camera, and bakes the far look of all of
them (`Sails.IslandDecor.proxyOf`: a few boxes and roof prisms per tree and
house, in their colours) together with the shell into one `far` mesh, so an
island far off costs a single draw call.

-}

import Game.BrickLook as BrickLook
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Sails.Archipelago as Archipelago
import Sails.IslandDecor as Decor exposing (Shape(..))
import Sails.IslandDressing as Dressing exposing (Item)
import Sails.Physics as Physics
import Sails.Scenery as Scenery
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
    List IslandMeshes


{-| One island's meshes: the bare `shell`, the `far` mesh (the shell with the
far proxies of its dressing baked in) and the dressing's items, with what
`Sails.Render` needs to draw them worked out once per voyage instead of on
every frame: each item's placement (`placed`), the hub's landmarks among them
(`landmarks`) and the prop models the island needs (`propNames`, each once).

`farPieces` is Nothing until a building of the island falls; from then on the
far look is drawn from it (`farMeshes`) instead of `far`.

-}
type alias IslandMeshes =
    { island : Island
    , shell : WebGL.Mesh IslandVertex
    , far : WebGL.Mesh IslandVertex
    , farPieces : Maybe FarPieces
    , items : List Item
    , placed : List Placed
    , landmarks : List Placed
    , propNames : List String
    }


{-| An item of an island's dressing placed in the world: its model matrix at
the islands' one brick scale, the centre and radius of the sphere it is culled
by, and its tint (for its colour-16 parts).
-}
type alias Placed =
    { item : Item
    , model : Mat4
    , centre : Vec3
    , radius : Float
    , tint : Vec4
    }


placeItem : Item -> Placed
placeItem it =
    let
        s =
            it.spot

        c =
            BrickLook.colour it.tint
    in
    { item = it
    , model =
        Mat4.makeTranslate3 s.x s.y s.z
            |> Mat4.rotate (degrees s.bearing) (vec3 0 1 0)
            |> Mat4.scale3 Scenery.metersPerLdu Scenery.metersPerLdu Scenery.metersPerLdu
    , centre = vec3 s.x (s.y + 0.5 * it.height) s.z
    , radius = max it.reach (0.6 * it.height)
    , tint = vec4 (Vec3.getX c) (Vec3.getY c) (Vec3.getZ c) 1
    }


{-| The hub's landmarks, seen from across the course, so drawn in real bricks
even on an island far off.
-}
isLandmark : String -> Bool
isLandmark prop =
    prop == "fort" || prop == "dec:lighthouse" || prop == "dec:cairn"


uniqueNames : List String -> List String
uniqueNames names =
    List.foldl
        (\n acc ->
            if List.member n acc then
                acc

            else
                n :: acc
        )
        []
        names


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

        plans =
            Dressing.plans (Archipelago.generate course.map)

        itemsOf isl =
            plans
                |> List.filter (\p -> p.island == isl)
                |> List.head
                |> Maybe.map .items
                |> Maybe.withDefault []
    in
    List.map
        (\isl ->
            let
                shellTris =
                    triangles palette isl

                items =
                    itemsOf isl

                placed =
                    List.map placeItem items
            in
            { island = isl
            , shell = WebGL.triangles shellTris
            , far = WebGL.triangles (shellTris ++ proxyTriangles isl items)
            , farPieces = Nothing
            , items = items
            , placed = placed
            , landmarks = List.filter (\p -> isLandmark p.item.prop) placed
            , propNames = uniqueNames (List.map .prop items)
            }
        )
        course.islands


{-| An island's far look split, once, so that buildings can fall out of it
without the island being baked again: the `base` (the shell and the far
proxies of every item that cannot fall) and one small mesh per standing
building (`pieces`, by its dressing item). A fallen building's piece is only
dropped from the list; no mesh is built.
-}
type alias FarPieces =
    { base : WebGL.Mesh IslandVertex
    , pieces : List ( Item, WebGL.Mesh IslandVertex )
    }


{-| Split an island's far look (`FarPieces`) by `canFall` over its current
items: the same triangles as `far`, the shell and the proxies of the items
that cannot fall in the `base` and one piece per item that can (small items
have no proxy, so no piece). Baked once per island per voyage, at its first
fall.
-}
splitFar : IslandPalette -> (Item -> Bool) -> IslandMeshes -> FarPieces
splitFar palette canFall entry =
    let
        isl =
            entry.island

        ( falling, fixed ) =
            List.partition canFall entry.items
    in
    { base = WebGL.triangles (triangles palette isl ++ proxyTriangles isl fixed)
    , pieces =
        falling
            |> List.filter (\it -> not it.small)
            |> List.map (\it -> ( it, WebGL.triangles (proxyTriangles isl [ it ]) ))
    }


{-| The meshes of an island's far look: its baked `far` mesh, or once a
building has fallen the `base` and the standing buildings' `pieces`.
-}
farMeshes : IslandMeshes -> List (WebGL.Mesh IslandVertex)
farMeshes entry =
    case entry.farPieces of
        Nothing ->
            [ entry.far ]

        Just split ->
            split.base :: List.map Tuple.second split.pieces


{-| The far proxies of an island's items (small props have none), in the
island-local space of `triangles`, facing outwards.
-}
proxyTriangles : Island -> List Item -> List ( IslandVertex, IslandVertex, IslandVertex )
proxyTriangles isl items =
    items
        |> List.filter (\it -> not it.small)
        |> List.concatMap
            (\it ->
                let
                    m =
                        Mat4.makeTranslate3 (it.spot.x - isl.x) it.spot.y (it.spot.z - isl.z)
                            |> Mat4.rotate (degrees it.spot.bearing) (vec3 0 1 0)

                    paint c =
                        if c == 16 then
                            BrickLook.colour it.tint

                        else
                            BrickLook.colour c

                    place ( a, b, c ) colour =
                        flatTri (Mat4.transform m a) (Mat4.transform m b) (Mat4.transform m c) colour 0
                in
                List.concatMap
                    (\shape ->
                        let
                            ( tris, colour ) =
                                shapeTriangles shape
                        in
                        List.map (\t -> place t (paint colour)) tris
                    )
                    (Decor.proxyOf it.prop)
            )


{-| A proxy shape's triangles, wound counter-clockwise seen from outside, and
its LDraw colour. Boxes have no bottom.
-}
shapeTriangles : Shape -> ( List ( Vec3, Vec3, Vec3 ), Int )
shapeTriangles shape =
    case shape of
        Box b ->
            let
                c =
                    vec3 b.x b.y b.z

                face n hn u hu v hv =
                    let
                        o =
                            Vec3.add c (Vec3.scale hn n)

                        at su sv =
                            Vec3.add o (Vec3.add (Vec3.scale (su * hu) u) (Vec3.scale (sv * hv) v))
                    in
                    [ ( at -1 -1, at 1 -1, at 1 1 ), ( at -1 -1, at 1 1, at -1 1 ) ]

                ex =
                    vec3 1 0 0

                ey =
                    vec3 0 1 0

                ez =
                    vec3 0 0 1
            in
            ( face ex b.hx ey b.hy ez b.hz
                ++ face (Vec3.negate ex) b.hx ez b.hz ey b.hy
                ++ face ey b.hy ez b.hz ex b.hx
                ++ face ez b.hz ex b.hx ey b.hy
                ++ face (Vec3.negate ez) b.hz ey b.hy ex b.hx
            , b.colour
            )

        Roof r ->
            let
                p x y z =
                    vec3 (r.x + x) (r.y + y) (r.z + z)

                fl =
                    p -r.hx 0 -r.hz

                fr =
                    p r.hx 0 -r.hz

                bl =
                    p -r.hx 0 r.hz

                br =
                    p r.hx 0 r.hz

                tl =
                    p -r.hx r.h 0

                tr =
                    p r.hx r.h 0
            in
            ( [ ( fl, tl, tr )
              , ( fl, tr, fr ) -- slope towards -Z
              , ( br, tr, tl )
              , ( br, tl, bl ) -- slope towards +Z
              , ( fl, bl, tl ) -- gable end at -X
              , ( fr, tr, br ) -- gable end at +X
              ]
            , r.colour
            )


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
