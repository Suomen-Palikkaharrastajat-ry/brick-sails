# Brick Sails (brick-sails). Only the name and ports are this game's own:
# every other target comes from the shared Makefile.game of the brick-studio
# submodule, included at the end once `make vendor` has checked it out.
GAME_NAME := sails
WATCH_PORT ?= 5177
PREVIEW_PORT ?= 4177

GAME_MAKEFILE := vendor/brick-studio/packages/brick-frame-game/Makefile.game

.DEFAULT_GOAL := help

.PHONY: help
help: ## Show available targets
	@grep -hE '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; !seen[$$1]++ {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
	@[ -f $(GAME_MAKEFILE) ] || echo "Run 'make vendor' first to get the shared targets."

# ── Vendor / symlinks ────────────────────────────────────────────────────────
# Kept here, not in Makefile.game: it is what checks that file out.

.PHONY: vendor
vendor: ## Init and update all git submodules and configure symlinks
	@[ -z "$$CI" ] || git config --global url."https://github.com/".insteadOf "git@github.com:"
	@if [ -d .git ]; then \
		if [ -z "$$CI" ] && [ -d ../brick-studio/.git ] && [ -d ../master-builder/.git ]; then \
			git -c submodule."vendor/brick-studio".url="$(abspath ../brick-studio)" \
			    -c protocol.file.allow=always submodule update --init && \
			git -C vendor/brick-studio \
			    -c submodule."vendor/master-builder".url="$(abspath ../master-builder)" \
			    -c protocol.file.allow=always submodule update --init; \
		else \
			git submodule update --init --recursive; \
		fi; \
	fi
	mkdir -p vendor review/src
	ln -sfn brick-studio/packages vendor/packages
	ln -sfn brick-studio/vendor/master-builder vendor/master-builder
	ln -sfn vendor/brick-studio/vendor/master-builder/packages packages
	ln -sfn ../../vendor/brick-studio/vendor/master-builder/review/src/LlmAgent review/src/LlmAgent

-include $(GAME_MAKEFILE)

.PHONY: watch all review review-fix fmt
watch: dev ## Alias for dev (start Vite dev server on WATCH_PORT)
all: clean build ## Clean and rebuild everything
review: elm-review ## Alias for elm-review
review-fix: elm-review-fix ## Alias for elm-review-fix
fmt: format ## Alias for format
