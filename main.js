import './main.css'
import { bootGame } from './vendor/packages/brick-frame-game/runtime/game-boot.js'
import { createAudioEngine } from './audio.js'
import { Elm } from './src/Main.elm'

// Phones play in landscape. On the first touch, ask for fullscreen and lock the
// screen to landscape where the browser allows it (fullscreen and a user gesture
// are needed on some browsers; iOS has no lock). Where the lock is not possible,
// the rotate hint in index.html and main.css covers the portrait phone.
if (window.matchMedia?.('(pointer: coarse)').matches) {
  window.addEventListener(
    'pointerdown',
    async () => {
      try {
        if (document.fullscreenEnabled && !document.fullscreenElement) {
          await document.documentElement.requestFullscreen()
        }
      } catch {
        // Fullscreen refused: the orientation lock may still work.
      }
      try {
        await screen.orientation?.lock?.('landscape')
      } catch {
        // Not supported or refused: the rotate hint takes over.
      }
    },
    { once: true, capture: true, passive: true },
  )
}

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
