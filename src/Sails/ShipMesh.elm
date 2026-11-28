module Sails.ShipMesh exposing
    ( Muzzle
    , PartProgress
    , ShipMesh
    , ShipPart
    , build
    , buildScenery
    , cannonMuzzles
    , partProgress
    , sailByColour
    , sailHoist
    , studScale
    , withParts
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

import Array exposing (Array)
import Dict
import Game.BrickProps as BrickProps
import Game.PartLoading as PartLoading
import LDraw.Connectivity as Connectivity exposing (ConnectionType(..))
import LDraw.Document as Document exposing (Document, Entry(..), Placement)
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Mesh exposing (Vertex)
import Render.Scene as Scene exposing (Scene)
import Sails.Wreck as Wreck
import Set exposing (Set)


{-| A boat's geometry, ready to draw.

  - `scene`: triangles and edges of the hull, rig and everything but the
    sails, in Y-up LDraw units, bow towards +Z.
  - `sails`: the sail bricks, in the same units, drawn separately so they
    can be lowered (see `sailHoist`) and take a player colour.
  - `sailFoot`: where the sails are attached and come down to when lowered,
    the deck or mast foot under them (see `sailFootOf`); `sailLow` and
    `sailTop` are the sails' own lowest and highest points. All Y-up LDU.
  - `toBoat`: maps the scene into boat space in metres: centred on the hull,
    bow along +Z, waterline at y = 0.
  - `lengthMeters`, `heightMeters`: size on the water, for cameras.
  - `deckStuds`: the free studs on top of the hull, in boat space (metres),
    where an anchor chain can be hooked on.
  - `cannons`: the muzzles of the cannons built on the boat, in boat space
    (metres); empty for a boat with no cannons (see `cannonMuzzles`).

-}
type alias ShipMesh =
    { scene : Scene
    , sails : Scene
    , sailFoot : Float
    , sailLow : Float
    , sailTop : Float
    , sailHangs : Bool
    , toBoat : Mat4
    , linear : Mat4
    , parts : Array ShipPart
    , triangleCount : Int
    , lengthMeters : Float
    , heightMeters : Float
    , deckStuds : List Vec3
    , cannons : List Muzzle
    }


{-| A cannon's muzzle in boat space (metres, bow along +Z), the side it
stands on: +1 starboard (boat-space x > 0, which `Sails.Render`'s heading
turn puts at (cos h, -sin h), the game's starboard), -1 port, and the
`direction` its barrel points (a unit vector in boat space), the way a shot
from it leaves (see `Sails.Physics.fireFromMuzzle`).
-}
type alias Muzzle =
    { position : Vec3
    , side : Float
    , direction : Vec3
    }


{-| One brick (or other part) of a boat, for when the boat breaks up
(`Sails.Wreck`): its own geometry in the boat's brick units (LDU, as in
`scene`) and its centre, the point its pieces turn about. A `ShipMesh`'s
`parts` holds only the parts a wreck can show, in piece order
(`Sails.Wreck.shownParts`), not every part of the model.
-}
type alias ShipPart =
    { scene : Scene
    , pivot : Vec3
    }


{-| Unified stud scale for all boats, ships and vehicles in the game, in metres per LDU.
At 0.035 m/LDU, 1 stud (20 LDU) is 0.70 m, and every ship—player builds,
pirate schooners, navy cutters, and ghost ships—shares this exact scale so
all bricks and studs match 1:1.
-}
studScale : Float
studScale =
    0.035


{-| Hull length baseline for a standard 6-stud boat, in metres.
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
            studScale

        centre =
            Vec3.scale 0.5 (Vec3.add bounds.min bounds.max) |> Vec3.toRecord

        lengthMeters =
            lengthLdu * scale

        draftLdu =
            min maxDraftLdu (lengthLdu * draftShare)

        linear =
            Mat4.makeScale3 scale scale scale
                |> (\m ->
                        if turned then
                            Mat4.rotate (pi / 2) (vec3 0 1 0) m

                        else
                            m
                   )

        toBoat =
            linear
                |> Mat4.translate3 -centre.x -(Vec3.getY bounds.min + draftLdu - 24) -centre.z

        -- Only the parts a wreck can show (`Sails.Wreck.shownParts`).
        parts =
            Wreck.shownParts placements
                |> List.map (onePart isSail doc cache)
                |> Array.fromList

        docSafe =
            docIsBfcSafe doc
    in
    { scene =
        Scene.buildScene
            { triangles = bodyGeom.triangles
            , lines = bodyGeom.lines
            , conditionalLines = []
            }
            (docSafe && BrickProps.arePartsBfcCertified cache (List.map .part body))
    , sails =
        Scene.buildScene
            { triangles = sailTriangles
            , lines = sailGeom.lines
            , conditionalLines = []
            }
            (docSafe && BrickProps.arePartsBfcCertified cache (List.map .part sails))
    , sailFoot = sailFootOf bodyGeom.triangles sailTriangles
    , sailLow = Vec3.getY (boundsOf sailTriangles).min
    , sailTop = Vec3.getY (boundsOf sailTriangles).max
    , sailHangs = sailsHangFromAbove (List.map (pieceBounds cache) body) sailTriangles
    , toBoat = toBoat
    , triangleCount = List.length triangles
    , lengthMeters = lengthMeters
    , heightMeters = size.y * scale
    , deckStuds = deckStudPoints toBoat placements body
    , cannons = cannonMuzzles toBoat placements
    , linear = linear
    , parts = parts
    }


{-| The height (Y-up LDU) the sails are lowered to: where they are attached,
the top of the hull, deck or mast foot under them. That is the highest point
of the hull's triangles that lie wholly below the lowest sail point and
reach under the sails' footprint (x and z). A flag hung at a mast top, or
square sails hanging from yards, then come down to the deck rather than
folding up where they hang. With nothing under the sails it is their own
lowest point.
-}
sailFootOf : List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex ) -> Float
sailFootOf body sails =
    let
        box =
            boundsOf sails

        lowest =
            Vec3.getY box.min

        under tri =
            let
                b =
                    boundsOf [ tri ]
            in
            (Vec3.getY b.max <= lowest + 0.5)
                && (Vec3.getX b.max >= Vec3.getX box.min)
                && (Vec3.getX b.min <= Vec3.getX box.max)
                && (Vec3.getZ b.max >= Vec3.getZ box.min)
                && (Vec3.getZ b.min <= Vec3.getZ box.max)

        -- The top of what the sails stand on: the highest surface directly
        -- under them. A sail with nothing under it (floating) still folds down
        -- onto the deck, the highest surface below its lowest point.
        tops =
            List.map (\tri -> Vec3.getY (boundsOf [ tri ]).max) body

        directlyUnder =
            body
                |> List.filter under
                |> List.map (\tri -> Vec3.getY (boundsOf [ tri ]).max)
                |> List.maximum
    in
    case directlyUnder of
        Just top ->
            min lowest top

        Nothing ->
            tops
                |> List.filter (\top -> top <= lowest + 0.5)
                |> List.maximum
                |> Maybe.map (min lowest)
                |> Maybe.withDefault lowest


{-| How the sails scene is drawn at a sail setting (`SailingState.sail`: 0
lowered, 1 full; more is drawn as full), in the scene's own units (apply
before `toBoat`). Lowering brings the sails down to where they are attached,
`sailFoot`: the lowest sail point comes down to it (all the way when
lowered) and the top of the sails to 12 % of its height above it, so a flag
at a mast top or sails hanging from yards come down to the deck, and sails
standing on the deck fold down onto it. The sails are never stretched.
-}
sailHoist : ShipMesh -> Float -> Mat4
sailHoist mesh sail =
    let
        k =
            0.12 + 0.88 * clamp 0 1 sail
    in
    if mesh.sailHangs then
        -- Something above holds the sails: they roll up towards it. The top
        -- stays where the sails hang and the cloth gathers up to it.
        Mat4.makeTranslate3 0 mesh.sailTop 0
            |> Mat4.scale3 1 k 1
            |> Mat4.translate3 0 -mesh.sailTop 0

    else
        -- Nothing above: the sails roll down, the foot stays on whatever
        -- they stand on, and the top comes down to it.
        Mat4.makeTranslate3 0 mesh.sailFoot 0
            |> Mat4.scale3 1 k 1
            |> Mat4.translate3 0 -mesh.sailFoot 0


{-| Whether the sails hang from a yard: a hull or rig piece (one placed part,
its whole bounds from `pieceBounds`, not one of its triangles) wider than one
stud (20 LDU) along x or z, whose bottom lies within the sails' height and
over their footprint. A mast is one stud wide, so a flag at a mast top does
not hang from one, and sails on the deck with nothing above roll down.
-}
sailsHangFromAbove : List { min : Vec3, max : Vec3 } -> List ( Vertex, Vertex, Vertex ) -> Bool
sailsHangFromAbove pieces sails =
    let
        box =
            boundsOf sails

        yard b =
            (Vec3.getY b.min >= Vec3.getY box.min + 0.5)
                && (Vec3.getY b.min <= Vec3.getY box.max + 0.5)
                && ((Vec3.getX b.max - Vec3.getX b.min >= 20.5) || (Vec3.getZ b.max - Vec3.getZ b.min >= 20.5))
                && (Vec3.getX b.max >= Vec3.getX box.min)
                && (Vec3.getX b.min <= Vec3.getX box.max)
                && (Vec3.getZ b.max >= Vec3.getZ box.min)
                && (Vec3.getZ b.min <= Vec3.getZ box.max)
    in
    List.any yard pieces


{-| The bounds of one placed part as a whole piece, in the Y-up scene (LDraw
Y and Z negated, as `LDraw.Geometry.worldOptions` flips them): the part's
catalog bounding boxes (`Connectivity.knownCatalogPart`, the real brick or
plate size) through its placement when the part is known, otherwise the
bounds of its own flattened geometry.
-}
pieceBounds : PartCache -> Placement -> { min : Vec3, max : Vec3 }
pieceBounds cache p =
    let
        toScene v =
            let
                q =
                    Vec3.toRecord (Mat4.transform p.matrix v)
            in
            vec3 q.x -q.y -q.z

        corners b =
            let
                lo =
                    Vec3.toRecord b.min

                hi =
                    Vec3.toRecord b.max
            in
            List.concatMap
                (\x -> List.concatMap (\y -> List.map (\z -> toScene (vec3 x y z)) [ lo.z, hi.z ]) [ lo.y, hi.y ])
                [ lo.x, hi.x ]

        catalogPoints =
            Connectivity.knownCatalogPart p.part
                |> Maybe.map (.boundingBoxes >> List.concatMap corners)
                |> Maybe.withDefault []
    in
    case catalogPoints of
        first :: rest ->
            { min = List.foldl (\v acc -> vec3 (min (Vec3.getX v) (Vec3.getX acc)) (min (Vec3.getY v) (Vec3.getY acc)) (min (Vec3.getZ v) (Vec3.getZ acc))) first rest
            , max = List.foldl (\v acc -> vec3 (max (Vec3.getX v) (Vec3.getX acc)) (max (Vec3.getY v) (Vec3.getY acc)) (max (Vec3.getZ v) (Vec3.getZ acc))) first rest
            }

        [] ->
            boundsOf (Geometry.flattenWith Geometry.worldOptions [ toRef p ] cache 15 Mat4.identity).triangles


markSail : ( Vertex, Vertex, Vertex ) -> ( Vertex, Vertex, Vertex )
markSail ( a, b, c ) =
    ( { a | inherit = 1 }, { b | inherit = 1 }, { c | inherit = 1 } )


{-| One placed part of a model on its own, for its wreck (`ShipPart`).
-}
onePart : (Placement -> Bool) -> Document -> PartCache -> Placement -> ShipPart
onePart isSail doc cache p =
    let
        geom =
            Geometry.flattenWith Geometry.worldOptions [ toRef p ] cache 15 Mat4.identity

        tris =
            if isSail p then
                List.map markSail geom.triangles

            else
                geom.triangles

        box =
            boundsOf tris

        partBfc =
            docIsBfcSafe doc && BrickProps.arePartsBfcCertified cache [ p.part ]
    in
    { scene = Scene.buildScene { triangles = tris, lines = geom.lines, conditionalLines = [] } partBfc
    , pivot = Vec3.scale 0.5 (Vec3.add box.min box.max)
    }


{-| The free studs on top of the hull, in boat space (metres). Candidates are
the upward studs of the hull's own bricks (sails are left out); a stud is not
free when an anti-stud of any brick, sail or not, sits on it. Connectors are
in LDraw space, flipped to the Y-up scene (see `LDraw.Geometry.worldOptions`)
and then mapped by `toBoat`.
-}
deckStudPoints : Mat4 -> List Placement -> List Placement -> List Vec3
deckStudPoints toBoat allPlacements hullPlacements =
    let
        worldConnectors kind placement =
            case Connectivity.knownCatalogPart placement.part of
                Just conn ->
                    conn.connections
                        |> List.filter (\c -> c.connType == kind)
                        |> List.map (Connectivity.transformPoint placement.matrix 0)

                Nothing ->
                    []

        -- LDraw is Y-down; the scene negates Y and Z.
        sceneFlip =
            Mat4.makeScale3 1 -1 -1

        toBoatPoint p =
            Mat4.transform toBoat (Mat4.transform sceneFlip p)

        key v =
            let
                q =
                    Vec3.toRecord v
            in
            ( round (q.x * 100), round (q.y * 100), round (q.z * 100) )

        covered : Set ( Int, Int, Int )
        covered =
            allPlacements
                |> List.concatMap (worldConnectors AntiStud)
                |> List.map (.worldPosition >> key)
                |> Set.fromList
    in
    hullPlacements
        |> List.concatMap (worldConnectors Stud)
        |> List.filter (\s -> Vec3.getY s.worldNormal < -0.7 && not (Set.member (key s.worldPosition) covered))
        |> List.map (.worldPosition >> toBoatPoint)


{-| The muzzles of the boat's cannons, in boat space (metres). A cannon is a
part the shipyard counts as one (`Sails.Ingest.classifyPart`: the name has
32828, cannon or shooting in it). On 32828 (plate 1 x 1 round with bar, the
shipyard's cannon) the muzzle is the open end of the bar, at (30, 2, 0) in
the part's own LDraw units; any other cannon part fires from its origin. The
point goes through the part's placement, the LDraw-to-scene flip and
`toBoat`, as the deck studs do (see `deckStudPoints`).

The barrel direction of a 32828 is the bar's axis, the part's own +x: the
muzzle point minus the point one LDraw unit back along +x, both taken through
the same placement, flip and `toBoat`, then made a unit vector. So a cannon
turned to face the bow points along +Z (forward), one facing the stern along
-Z, and one facing out to port along -x. Any other cannon part (whose barrel
axis is not known) points square out on its side, (side, 0, 0), as before.

-}
cannonMuzzles : Mat4 -> List Placement -> List Muzzle
cannonMuzzles toBoat placements =
    let
        isCannon name =
            let
                n =
                    String.toLower name
            in
            String.contains "32828" n || String.contains "cannon" n || String.contains "shooting" n

        localMuzzle name =
            if String.contains "32828" (String.toLower name) then
                vec3 30 2 0

            else
                vec3 0 0 0

        toBoatPoint placement local =
            Mat4.transform placement.matrix local
                |> Mat4.transform (Mat4.makeScale3 1 -1 -1)
                |> Mat4.transform toBoat

        muzzle placement =
            let
                local =
                    localMuzzle placement.part

                boatPoint =
                    toBoatPoint placement local

                side =
                    if Vec3.getX boatPoint >= 0 then
                        1

                    else
                        -1

                barrel =
                    Vec3.sub boatPoint (toBoatPoint placement (Vec3.sub local (vec3 1 0 0)))

                direction =
                    if String.contains "32828" (String.toLower placement.part) && Vec3.length barrel > 1.0e-9 then
                        Vec3.normalize barrel

                    else
                        vec3 side 0 0
            in
            { position = boatPoint
            , side = side
            , direction = direction
            }
    in
    placements
        |> List.filter (.part >> isCannon)
        |> List.map muzzle


{-| Flatten a scenery model (see `Sails.Scenery`) whose parts are all in the
cache, at a fixed scale in metres per LDraw unit. Its origin stays where
the model has it, on the ground; it has no sails.
-}
buildScenery : Float -> Document -> PartCache -> ShipMesh
buildScenery metersPerLdu doc rawCache =
    let
        cache =
            List.foldl (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc) rawCache (Document.subfiles doc)

        placements =
            Document.flattenedPlacements doc

        geom =
            Geometry.flattenWith Geometry.worldOptions (List.map toRef placements) cache 15 Mat4.identity

        size =
            boundsOf geom.triangles |> (\b -> Vec3.sub b.max b.min) |> Vec3.toRecord

        empty =
            Scene.buildScene { triangles = [], lines = [], conditionalLines = [] } False

        bfcCertified =
            docIsBfcSafe doc && BrickProps.arePartsBfcCertified cache (List.map .part placements)
    in
    { scene =
        Scene.buildScene
            { triangles = geom.triangles
            , lines = geom.lines
            , conditionalLines = []
            }
            bfcCertified
    , sails = empty
    , sailFoot = 0
    , sailLow = 0
    , sailTop = 0
    , sailHangs = False
    , toBoat = Mat4.makeScale3 metersPerLdu metersPerLdu metersPerLdu
    , triangleCount = List.length geom.triangles
    , lengthMeters = max size.x size.z * metersPerLdu
    , heightMeters = size.y * metersPerLdu
    , deckStuds = []
    , cannons = []
    , linear = Mat4.makeScale3 metersPerLdu metersPerLdu metersPerLdu
    , parts = Array.empty
    }


{-| A piece of scenery with its own parts too, for when it is smashed (a
harbour's pier, see `Sails.Harbours`), the way a boat breaks up
(`Sails.Wreck`). `buildScenery` leaves them out: most scenery never breaks.
-}
withParts : Document -> PartCache -> ShipMesh -> ShipMesh
withParts doc rawCache mesh =
    let
        cache =
            List.foldl (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc) rawCache (Document.subfiles doc)
    in
    { mesh | parts = Array.fromList (List.map (onePart (\_ -> False) doc cache) (Wreck.shownParts (Document.flattenedPlacements doc))) }


docIsBfcSafe : Document -> Bool
docIsBfcSafe doc =
    let
        entrySafe entry =
            case entry of
                PlacementEntry _ ->
                    True

                Raw text ->
                    not
                        (String.contains "BFC CERTIFY CW" text
                            || String.contains "BFC NOCERTIFY" text
                            || String.contains "BFC NOCLIP" text
                            || String.contains "BFC INVERTNEXT" text
                        )
    in
    List.all (\sub -> List.all entrySafe sub.entries) doc.submodels


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
