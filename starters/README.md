# Starter hulls

Six alternative boat hulls a player can start a Brick Sails build from. Each
hull has a bow, a flat plate deck with studs to build on, and a keel. There is
no mast or sail: the player adds those. The ship editor (`Pages.Build`,
"Build a ship on a hull" in the shipyard) starts every ship from one of them.

| File | Name | Parts | Bounds in LDU (x / y / z) | Character |
|---|---|---|---|---|
| `sails/narrow-racer.mpd` | Narrow Racing Hull | 6 | -20..20 / -12..72 / -70..50 (40 x 84 x 120) | Micro 2 x 6 stud racing sloop with inverted slope bow, flat plate deck and fin keel |
| `sails/wide-sturdy.mpd` | Wide Sturdy Hull | 8 | -40..40 / -12..72 / -70..50 (80 x 84 x 120) | Micro 4 x 6 stud beamy galleon with twin bow slopes, broad deck and central keel |
| `sails/long-cruiser.mpd` | Long Cruiser Hull | 8 | -20..20 / -12..72 / -80..80 (40 x 84 x 160) | Sleek 2 x 8 stud micro clipper with wing-plate bow cutwater, long deck and keel |
| `sails/round-bow.mpd` | Round Bow Hull | 7 | -20..20 / -12..72 / -60..60 (40 x 84 x 120) | Micro 2 x 6 stud round-nosed cog with round brick stem, flush deck and keel |
| `sails/sloop-hull.mpd` | Sloop Hull | 39 | -52..52 / -94..24 / -160..80 (104 x 118 x 240) | Hull of the NPC sloop (`src/Sails/NpcShips.elm`): 4 x 9 studs, raked bow, bowsprit, gunport band, raised quarterdeck with helm |
| `sails/frigate-hull.mpd` | Frigate Hull | 49 | -52..52 / -84..24 / -180..100 (104 x 108 x 280) | Hull of the NPC frigate: 4 x 11 studs, raked bow, bowsprit, forecastle, waist and quarterdeck, gunport band |

Bounds are from the real catalog geometry, in LDraw axes like
`Sails.Boats`: -Y is up, the bow points to -Z, the hull bricks' tops are at
y = 0, the deck plates on top of them at y = -8 and the keel hangs below y = 24.

The sloop and frigate hulls are the hulls of the computer's ships: their hull
submodels (`npc-sloop-hull.ldr`, `npc-frigate-hull.ldr`) are the same, part
for part, as in `src/Sails/NpcShips.elm`, so the NPC ship is the hull plus the
masts, sails, flags and cannons the player can add. They are bigger than the
small hulls (39 and 49 parts) and have no keel below y = 24.

## Layout of a file

Each `.mpd` holds two LDraw files:

1. The main model (`<name>.ldr`): header comments (name, description, part
   count, bounds) and a single type-1 line that places the hull.
2. The hull submodel (`<name>-base.ldr`): every part of the hull.

Because the whole hull is one submodel reference, the editors select and move
it as one piece, and it must be ungrouped before a part can come off. The game
flattens the submodel (`LDraw.Document.flattenedPlacements`), so the loader and
`Sails.ShipMesh` see every part. The `0 Name:` line of the main model is the
title the game shows ("Your build: Round Bow Hull").

## Build rules they meet

- Micro-scale starter base budget: 6 to 8 parts (< 10 parts each), leaving maximum part budget for player customization.
- `Sails.Ingest.validateShip` accepts each one: the hull part count is above the 2 parts `Sails.Analyze` needs to float. With no sail yet the analysis gives only the "No sail detected" advisory.
- Parts whose origin is lower than y = 10 (LDraw y > 10) count as keel (`RudderKeel`) in `Sails.Ingest`; every hull has one.

## Checks

- `tests/StarterHullsTest.elm` (run by `make test`) parses each file and
  checks: one grouped submodel; the header part count; at most 80 parts;
  `validateShip` accepts it, with only the no-sail advisory; it has a keel
  below the hull and plates on top; `Studio.Health` finds no misaligned or
  overlapping part; every part is joined to the rest of the hull by studs.
- `src/Sails/StarterBases.elm` embeds the files for the ship editor and the
  tests. Regenerate it after changing a file: `node starters/embed-starters.mjs`.
- Each hull was also loaded into the game with a `#model=` link and the real
  catalog parts: it appears as "Your ship" with its title, ready to sail.

## In the ship editor

The editor (`src/Pages/Build.elm`, rules in `src/Sails/Editor.elm`) opens a
hull as it is: it stays one grouped submodel. The player adds parts from a
fixed list of 43 parts in 8 colours (hull bricks, plates and slopes, deck
tiles, the antenna mast, the flag sail, a cannon); a flag added while a mast
is selected hangs on the mast. The hull can be moved and turned as a whole
but never ungrouped, deleted, recoloured or copied (every edit that would
split it is dropped). "Use ship" loads the build like a build link, as "Your
ship", and puts its `#b/...` link in the URL; "Edit ship" opens it again.
`tests/EditorTest.elm` and `tests/BuildPageTest.elm` check this.

## Regenerating

`starters/tools/build_starters.py` is the script that wrote these files (and
the Brick Racer car bases). It measures real part bounds from the catalog's
MPD bundles (`https://catalog.palikkaharrastajat.fi/bundles/`, cached in the
system temp directory) and needs Python 3 with numpy:

```bash
python3 starters/tools/build_starters.py /tmp/racer-bases starters/sails
node starters/embed-starters.mjs
```

The hull cards in the ship editor's hulls drawer show a picture of each hull,
`public/base-previews/<name>.png`, as Brick Racer's bases drawer does. Redraw
them after changing a hull:

```bash
node starters/tools/render_base_previews.mjs
```

It flattens the hull submodel with the shared thumbnail pipeline's
`flattenPart` (`vendor/brick-studio/packages/brick-frame-viewport/runtime/thumbnail-pipeline.js`,
so `make vendor` first), fetches the parts and LDConfig colours from the
catalog, and draws each hull once with WebGL in headless Chromium, so it needs
the `playwright` package and its Chromium.
