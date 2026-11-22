GAME_NAME ?= sails
WATCH_PORT ?= 5177
PREVIEW_PORT ?= 4177

-include vendor/packages/brick-frame-game/Makefile.game

ifeq ($(wildcard vendor/packages/brick-frame-game/Makefile.game),)
.PHONY: vendor
vendor:
	@[ -z "$$CI" ] || git config --global url."https://github.com/".insteadOf "git@github.com:"
	@if [ -d .git ]; then git submodule update --init --recursive; fi
	mkdir -p vendor review/src
	ln -sfn brick-studio/packages vendor/packages
	ln -sfn brick-studio/vendor/master-builder vendor/master-builder
	ln -sfn vendor/brick-studio/vendor/master-builder/packages packages
	ln -sfn ../../vendor/brick-studio/vendor/master-builder/review/src/LlmAgent review/src/LlmAgent
endif
