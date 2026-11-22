# Starter hulls

Four alternative boat hulls a player can start a Brick Sails build from. Each
hull has a bow, a flat plate deck with studs to build on, and a keel. There is
no mast or sail: the player adds those. They are the samples a later ship
editor's fixed parts list will be drawn from.

| File | Name | Parts | Bounds in LDU (x / y / z) | Character |
|---|---|---|---|---|
| `sails/narrow-racer.mpd` | Narrow Racing Hull | 26 | -40..40 / -20..96 / -160..160 (80 x 116 x 320) | 4 wide, wing-plate (`51739`) pointed bow, deep fin keel |
| `sails/wide-sturdy.mpd` | Wide Sturdy Hull | 39 | -60..60 / -20..72 / -140..140 (120 x 92 x 280) | 6 wide, three courses deep, blunt bow |
| `sails/long-cruiser.mpd` | Long Cruiser Hull | 38 | -40..40 / -20..72 / -280..280 (80 x 92 x 560) | 4 x 28 studs, pointed bow, full-length 2-wide keel |
| `sails/round-bow.mpd` | Round Bow Hull | 43 | -60..60 / -20..72 / -160..120 (120 x 92 x 280) | 6 wide, round bricks, round plates and curved inverted slopes at the bow |

Bounds are from the real catalog geometry, in LDraw axes like
`Sails.Boats`: -Y is up, the bow points to -Z, the hull bricks' tops are at
y = 0, the deck plates on top of them at y = -8, gunwale plates at y = -16 and
the keel hangs below y = 24.

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

- Well under `Sails.Ingest.maxShipParts` (600): 26 to 43 parts.
- `Sails.Ingest.validateShip` accepts each one: the hull part count is far
  above the 2 parts `Sails.Analyze` needs to float. With no sail yet the
  analysis gives only the "No sail detected" advisory.
- Parts whose origin is lower than y = 10 (LDraw y > 10) count as keel (`RudderKeel`) in `Sails.Ingest`; every hull has one.

## Checks

- `tests/StarterHullsTest.elm` (run by `make test`) parses each file and
  checks: one grouped submodel; the header part count; at most 80 parts;
  `validateShip` accepts it, with only the no-sail advisory; it has a keel
  below the hull and plates on top; `Studio.Health` finds no misaligned or
  overlapping part; every part is joined to the rest of the hull by studs.
- `tests/StarterBases.elm` embeds the files for the test. Regenerate it after
  changing a file: `node starters/embed-starters.mjs`.
- Each hull was also loaded into the game with a `#model=` link and the real
  catalog parts: it appears as "Your ship" with its title, ready to sail.

## Regenerating

`starters/tools/build_starters.py` is the script that wrote these files (and
the Brick Racer car bases). It measures real part bounds from the catalog's
MPD bundles (`https://catalog.palikkaharrastajat.fi/bundles/`, cached in the
system temp directory) and needs Python 3 with numpy:

```bash
python3 starters/tools/build_starters.py /tmp/racer-bases starters/sails
node starters/embed-starters.mjs
```
