# Brick Sails (Palikkapurjehdus)

Brick Sails is a toy boat buoy race by Suomen Palikkaharrastajat ry: steer a little brick-built boat through the buoys, round the island and fire at floating targets.

It runs on the shared Brick Frame game layer of `brick-studio` (git submodule
`vendor/brick-studio`), which also brings in the family branding from
`master-builder` (`vendor/brick-studio/vendor/master-builder`). The icons,
social image, web manifest and Outfit font in `public/` are the family set
from `master-builder/public`, as in Brick Play.

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
