module Sails.ShipMesh exposing
    ( PartProgress
    , ShipMesh
    , build
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

Sail bricks are marked in the vertex `inherit` channel (unused by baked
geometry otherwise) so `Sails.Render` can paint them in a player colour
without a second copy of the mesh.

-}

import Dict
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Geometry as Geometry
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Mesh exposing (Vertex)
import Render.Scene as Scene exposing (Scene)
import Set


{-| A boat's geometry, ready to draw.

  - `scene`: triangles and edges in Y-up LDraw units, bow towards +Z.
  - `toBoat`: maps the scene into boat space in metres: centred on the hull,
    bow along +Z, waterline at y = 0.
  - `lengthMeters`, `heightMeters`: size on the water, for cameras.

-}
type alias ShipMesh =
    { scene : Scene
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


{-| Part files still to fetch for a model, and how many are on their way.
A model is ready to build when both are empty.
-}
type alias PartProgress =
    { needed : List String
    , waiting : Int
    , total : Int
    }


{-| Walk every part the model references, through sub-models and already
loaded part files, and report which ones the cache does not have yet.
-}
partProgress : Document -> PartCache -> PartProgress
partProgress doc cache =
    let
        subfiles =
            Dict.fromList (Document.subfiles doc)

        start =
            List.map .part (Document.flattenedPlacements doc)

        step frontier visited acc =
            case frontier of
                [] ->
                    { acc | needed = List.reverse acc.needed }

                raw :: rest ->
                    let
                        name =
                            normalize raw
                    in
                    if String.isEmpty name || Set.member name visited then
                        step rest visited acc

                    else
                        let
                            seen =
                                Set.insert name visited
                        in
                        case Dict.get name subfiles of
                            Just lines ->
                                step (Resolve.pendingParts lines Dict.empty ++ rest) seen acc

                            Nothing ->
                                case Dict.get name cache of
                                    Nothing ->
                                        step rest seen { acc | needed = name :: acc.needed, total = acc.total + 1 }

                                    Just Loading ->
                                        step rest seen { acc | waiting = acc.waiting + 1, total = acc.total + 1 }

                                    Just (Failed _) ->
                                        step rest seen { acc | total = acc.total + 1 }

                                    Just (Loaded lines) ->
                                        step (Resolve.pendingParts lines Dict.empty ++ rest) seen { acc | total = acc.total + 1 }
    in
    step start Set.empty { needed = [], waiting = 0, total = 0 }


normalize : String -> String
normalize raw =
    String.toLower (String.replace "\\" "/" (String.trim raw))


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

        triangles =
            bodyGeom.triangles ++ List.map markSail sailGeom.triangles

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
            { triangles = triangles
            , lines = bodyGeom.lines ++ sailGeom.lines
            , conditionalLines = []
            }
            False
    , toBoat = toBoat
    , triangleCount = List.length triangles
    , lengthMeters = lengthMeters
    , heightMeters = size.y * scale
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
