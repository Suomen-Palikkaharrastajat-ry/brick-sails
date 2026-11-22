module Sails.ShipMesh exposing
    ( PartProgress
    , ShipMesh
    , build
    , buildScenery
    , partProgress
    , sailByColour
    , sailByPartName
    )

{-| Turn a brick model into the geometry the game draws.

This is the same pipeline the brick building apps use: parts come from the
catalog through `LDraw.Resolve` (one pre-bundled MPD per part, so a part and
all its sub-parts and primitives arrive in one request), `LDraw.Geometry`
flattens the model into triangles and edges, and `Render.Scene.buildScene`
uploads them once. `Main` keeps the result per boat, so nothing is parsed or
flattened again while the game runs.

Sail bricks are kept in a scene of their own, so the sails can be lowered
and raised, and are marked in the vertex `inherit` channel (unused by baked
geometry otherwise) so `Sails.Render` can paint them in a player colour.

-}

import Dict
import Game.PartLoading as PartLoading
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Mesh exposing (Vertex)
import Render.Scene as Scene exposing (Scene)


{-| A boat's geometry, ready to draw.

  - `scene`: triangles and edges of the hull, rig and everything but the
    sails, in Y-up LDraw units, bow towards +Z.
  - `sails`: the sail bricks, in the same units, drawn separately so they
    can be lowered (squashed down to `sailFoot`, their lowest point) and
    take a player colour.
  - `toBoat`: maps the scene into boat space in metres: centred on the hull,
    bow along +Z, waterline at y = 0.
  - `lengthMeters`, `heightMeters`: size on the water, for cameras.

-}
type alias ShipMesh =
    { scene : Scene
    , sails : Scene
    , sailFoot : Float
    , toBoat : Mat4
    , triangleCount : Int
    , lengthMeters : Float
    , heightMeters : Float
    }


{-| Hull length every boat is scaled to, in metres. The buoy course, the
cameras and the cannons are tuned for boats of this size.
-}
targetLength : Float
targetLength =
    4.6


{-| How far the hull sits in the water, as a share of its length, at most
`maxDraftLdu` (two thirds of a brick) so a one-brick hull still shows.
-}
draftShare : Float
draftShare =
    0.06


maxDraftLdu : Float
maxDraftLdu =
    16


{-| Sail rule for the built-in boats: bricks in the boat's sail colour.
-}
sailByColour : Int -> Placement -> Bool
sailByColour code p =
    p.color == code


{-| Sail rule for loaded builds: parts whose name says sail, flag or cloth.
-}
sailByPartName : (String -> Bool) -> Placement -> Bool
sailByPartName isSail p =
    isSail p.part


{-| Part files still to fetch for a boat, and how many are on their way.
A boat is ready to build when nothing is `needed` and nothing is `inFlight`.
-}
type alias PartProgress =
    PartLoading.PartProgress


{-| Walk every part the boat references, starting from its flattened
placements, and report which ones the cache does not have yet.
-}
partProgress : Document -> PartCache -> PartProgress
partProgress doc cache =
    PartLoading.progress (PartLoading.flattenedParts doc) doc cache


{-| Flatten a model whose parts are all in the cache into a `ShipMesh`.
`isSail` picks the placements that take a player colour.
-}
build : (Placement -> Bool) -> Document -> PartCache -> ShipMesh
build isSail doc rawCache =
    let
        cache =
            List.foldl (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc) rawCache (Document.subfiles doc)

        placements =
            Document.flattenedPlacements doc

        ( sails, body ) =
            List.partition isSail placements

        flatten group =
            Geometry.flattenWith Geometry.worldOptions (List.map toRef group) cache 15 Mat4.identity

        bodyGeom =
            flatten body

        sailGeom =
            flatten sails

        markSail ( a, b, c ) =
            ( { a | inherit = 1 }, { b | inherit = 1 }, { c | inherit = 1 } )

        sailTriangles =
            List.map markSail sailGeom.triangles

        triangles =
            bodyGeom.triangles ++ sailTriangles

        bounds =
            boundsOf triangles

        size =
            Vec3.sub bounds.max bounds.min |> Vec3.toRecord

        -- A build that is longer across than fore and aft is turned so its
        -- long side runs along the heading.
        turned =
            size.x > size.z * 1.15

        lengthLdu =
            max 1
                (if turned then
                    size.x

                 else
                    size.z
                )

        scale =
            min (targetLength / lengthLdu) (9 / max 1 size.y)

        centre =
            Vec3.scale 0.5 (Vec3.add bounds.min bounds.max) |> Vec3.toRecord

        lengthMeters =
            lengthLdu * scale

        draftLdu =
            min maxDraftLdu (lengthLdu * draftShare)

        toBoat =
            Mat4.makeScale3 scale scale scale
                |> (\m ->
                        if turned then
                            Mat4.rotate (pi / 2) (vec3 0 1 0) m

                        else
                            m
                   )
                |> Mat4.translate3 -centre.x -(Vec3.getY bounds.min + draftLdu) -centre.z
    in
    { scene =
        Scene.buildScene
            { triangles = bodyGeom.triangles
            , lines = bodyGeom.lines
            , conditionalLines = []
            }
            False
    , sails =
        Scene.buildScene
            { triangles = sailTriangles
            , lines = sailGeom.lines
            , conditionalLines = []
            }
            False
    , sailFoot = Vec3.getY (boundsOf sailTriangles).min
    , toBoat = toBoat
    , triangleCount = List.length triangles
    , lengthMeters = lengthMeters
    , heightMeters = size.y * scale
    }


{-| Flatten a scenery model (see `Sails.Scenery`) whose parts are all in the
cache, at a fixed scale in metres per LDraw unit. Its origin stays where
the model has it, on the ground; it has no sails.
-}
buildScenery : Float -> Document -> PartCache -> ShipMesh
buildScenery metersPerLdu doc rawCache =
    let
        cache =
            List.foldl (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc) rawCache (Document.subfiles doc)

        geom =
            Geometry.flattenWith Geometry.worldOptions (List.map toRef (Document.flattenedPlacements doc)) cache 15 Mat4.identity

        size =
            boundsOf geom.triangles |> (\b -> Vec3.sub b.max b.min) |> Vec3.toRecord

        empty =
            Scene.buildScene { triangles = [], lines = [], conditionalLines = [] } False
    in
    { scene =
        Scene.buildScene
            { triangles = geom.triangles
            , lines = geom.lines
            , conditionalLines = []
            }
            False
    , sails = empty
    , sailFoot = 0
    , toBoat = Mat4.makeScale3 metersPerLdu metersPerLdu metersPerLdu
    , triangleCount = List.length geom.triangles
    , lengthMeters = max size.x size.z * metersPerLdu
    , heightMeters = size.y * metersPerLdu
    }


toRef : Placement -> LDrawLine
toRef p =
    SubFileRef { color = p.color, transform = p.matrix, file = p.part }


boundsOf : List ( Vertex, Vertex, Vertex ) -> { min : Vec3, max : Vec3 }
boundsOf tris =
    let
        grow v acc =
            let
                p =
                    Vec3.toRecord v.position
            in
            { minX = min acc.minX p.x
            , minY = min acc.minY p.y
            , minZ = min acc.minZ p.z
            , maxX = max acc.maxX p.x
            , maxY = max acc.maxY p.y
            , maxZ = max acc.maxZ p.z
            }

        b =
            List.foldl (\( v1, v2, v3 ) acc -> acc |> grow v1 |> grow v2 |> grow v3)
                { minX = 1.0e9, minY = 1.0e9, minZ = 1.0e9, maxX = -1.0e9, maxY = -1.0e9, maxZ = -1.0e9 }
                tris
    in
    if b.minX > b.maxX then
        { min = vec3 0 0 0, max = vec3 0 0 0 }

    else
        { min = vec3 b.minX b.minY b.minZ, max = vec3 b.maxX b.maxY b.maxZ }
