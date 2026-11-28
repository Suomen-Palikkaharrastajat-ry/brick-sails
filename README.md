# Vaarallisilla vesillä (Brick Sails)

Vaarallisilla vesillä (Brick Sails) is a toy boat sea adventure by Suomen Palikkaharrastajat ry: ride the trade winds in a little brick-built ship past reefs and palm coves, hunt for treasure and fight the pirate fleet.

It runs on the shared Brick Frame game layer of `brick-studio` (git submodule
`vendor/brick-studio`), which also brings in the family branding from
`master-builder` (`vendor/brick-studio/vendor/master-builder`). The icons,
social image, web manifest and Outfit font in `public/` are the family set
from `master-builder/public`, as in Brick Play.

## Game Goals & Objectives

### 1. Primary Voyage Objective
- **Course Navigation & Docking**: Navigate through the archipelago's waypoint sequence in order. Each waypoint is a sheltered harbour marked by beacons.
- **Mooring**: To check in at each waypoint, steer into the harbour radius, slow down, and moor (or drop anchor with the anchor button / Space).
- **Final Harbour**: Complete all legs of the route and dock at the final harbour to finish the voyage, stopping the timer and recording your time on the leaderboard.
- **Multiplayer Crew Voyage**: In an online multiplayer room, all players participate as one unified crew (`Sails.Crew`). The voyage timer starts together, and the voyage completes when the entire crew has safely moored at the final harbour. Alone, you are a crew of one: your run completes when your ship moors at the final harbour, and it still counts as a personal record.

### 2. Secondary & Auxiliary Goals
- **Naval Combat & Pirate Ships**: A fleet of six patrol ships roams the archipelago, two for each faction, in three sizes: big frigates, middling corvettes and small sloops (`Sails.Pirates`). Protect your ship by out-maneuvering them and engaging with broadside cannon fire (tap/click the sea or press Space); each ship sunk wins gold coins.
- **Regalia & Standards (War of the Three Ensigns)**: Recover faction standards (Pirates, BlueCoats, RedCoats) bobbing at sea after wrecks or dropped in combat by sailing over them.
- **Ghost Ship**: Test your seamanship against a phantom runner (spectral cyan ghost vessel) recreating previous best routes and times.
- **Treasure Hunting**: Sail near secluded island beaches and coves to discover pirate chests and gather flying gold coins.
- **Seamanship & Survival**: Manage your angle to the wind across points of sail (Running, Reaching, Close-Hauled, and In Irons). Avoid shallow coral reefs, pounding rough seas, and skerry rocks—taking too much damage will wreck your vessel and end the run.

---

## Hint Arrows & Navigation Indicators

Brick Sails provides a dual navigational indicator system: in-world 3D water surface arrows and 2D on-screen HUD badges.

### 1. In-World 3D Water Surface Arrows
3D chevrons with custom brick icons float and bob directly on the ocean surface in concentric orbits around the player's boat:
- **Emerald Green Chevron (Radius ~2.1 m)**: Port gateway beacon icon pointing toward the **next Harbour Waypoint** you must dock at.
- **Radiant Gold Star Chevron (Radius ~2.3 m, within 350 m)**: 8-point compass star icon pointing to the nearest **Lost Faction Standard** bobbing at sea.
- **Crimson Red Crosshair Chevron (Radius ~2.5 m, within 180 m)**: Bullseye crosshair icon pointing to the nearest hostile **Pirate Ship**.
- **Spectral Cyan Twin-Sail Chevron (Radius ~2.7 m, within 400 m)**: Phantom sail icon pointing toward the **Ghost Ship**.

### 2. 2D HUD Goal Badges
Orbiting pill badges appear around the ship in screen space to provide continuous bearing and distance readouts:
- **Harbour Goal** (Emerald green badge, Anchor icon): Displays heading and distance (in meters) to the next harbour.
- **Standard Goal** (Amber/gold badge, Flag icon): Displays heading and distance to the nearest faction standard afloat.
- **Pirate Goal** (Rose/crimson badge, Crosshair icon): Displays heading and distance to the nearest hostile pirate ship.

The badges fade out (over 0.4 s) while you are shooting and come back once you have sailed for 3 s without a shot, or when you drop anchor (`Sails.HUD.hintVisible`).

### 3. Course Correction & Wind Indicators
- **Back to Course Arrow**: If wind, currents, or combat push your boat far off the designated course channel, a high-visibility yellow pill badge (`Back to course`) appears at the top of the HUD with an arrow pointing directly toward the route fairway.
- **Wind Compass Arrow**: Points in the direction the wind is blowing relative to your ship and camera, helping you trim sails up or down to avoid getting caught "in irons" (dead into the wind).
- **Contextual Action Hints**: On-screen prompts indicate when to drop anchor in a harbour, when cannons are ready to fire ("Tap to shoot" / Space), and available steering controls.

## Quick Start

Using [devenv](https://devenv.sh):

```bash
# Enter devenv shell
devenv shell

# Initialize git submodules and symlinks
make vendor

# Run checks and test suites
make test

# Start local dev server (http://localhost:5177)
make dev

# Production build
make build
```

## Licensing & Trademarks

- Software is open source under the MIT License (see [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md)).
- LEGO® is a registered trademark of the LEGO Group of companies, which does not sponsor, authorize, or endorse this project.
- LDraw™ is a trademark owned and licensed by the Estate of James Jessiman.
