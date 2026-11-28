import './main.css'
import { bootGame } from './vendor/packages/brick-frame-game/runtime/game-boot.js'
import { readLanguage } from './vendor/packages/brick-frame-game/runtime/language.js'
import { initThumbnails } from './vendor/packages/brick-frame-viewport/runtime/thumbnail-pipeline.js'
import { createAudioEngine } from './audio.js'
import { Elm } from './src/Main.elm'

// The page language follows the game's (Finnish unless a ?lang= link or a
// saved choice says English), so the rotate hint in index.html shows only the
// player's language. The game's FI / EN switch saves its choice through
// `storeLanguage`; the page follows it there too.
document.documentElement.lang = readLanguage()

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
const { ports } = bootGame({
  elm: Elm.Main,
  env: import.meta.env,
  storagePrefix: 'sails',
  createAudioEngine,
  playerPrefix: 'Sailor ',
  // Phones always steer by tilt (there is no toggle); the on-screen buttons
  // are only the fallback when the sensor is missing or permission is
  // refused. The iOS permission prompt runs on the first tap anywhere.
  tiltSteer: { storageKey: 'sails.steerMode', forceOnPhones: true },
})

ports?.storeLanguage?.subscribe((lang) => {
  document.documentElement.lang = lang
})
// Part pictures for the ship editor's dock (`Pages.Build`) and pictures of
// whole hulls and saved builds, from the same catalog the game loads its parts
// from.
initThumbnails(ports, import.meta.env.VITE_LDRAW_BASE || 'https://catalog.palikkaharrastajat.fi/raw')

// The ship editor's build history (`Game.BuildHistory`): saved on every change,
// handed back to Elm at start-up. Storage may be blocked (private mode, full
// quota); the game then simply has no saved builds.
const BUILD_HISTORY_KEY = 'palikkakehys.sails.buildHistory'
ports?.saveBuildHistory?.subscribe((json) => {
  try {
    window.localStorage.setItem(BUILD_HISTORY_KEY, String(json ?? ''))
  } catch {
    // Not saved; the build stays in memory.
  }
})
let savedBuildHistory = ''
try {
  savedBuildHistory = window.localStorage.getItem(BUILD_HISTORY_KEY) || ''
} catch {
  savedBuildHistory = ''
}
// After the app's first subscriptions are in place.
setTimeout(() => ports?.buildHistoryLoaded?.send(savedBuildHistory), 0)
