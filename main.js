import './main.css'
import { bootGame } from './vendor/packages/brick-frame-game/runtime/game-boot.js'
import { createAudioEngine } from './audio.js'
import { Elm } from './src/Main.elm'

// A build link in the page URL (`#b/...` or `#model=...`) is loaded as the
// player's own ship; without one the four built-in boats are offered.
bootGame({
  elm: Elm.Main,
  env: import.meta.env,
  storagePrefix: 'sails',
  createAudioEngine,
  playerPrefix: 'Sailor ',
  tiltSteer: { storageKey: 'sails.steerMode' },
})
