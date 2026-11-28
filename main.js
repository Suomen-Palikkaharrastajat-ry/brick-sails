import './main.css'
import { zipSync, strToU8 } from 'fflate'
import { bootGame } from './vendor/packages/brick-frame-game/runtime/game-boot.js'
import { readLanguage } from './vendor/packages/brick-frame-game/runtime/language.js'
import { initGameThumbnails } from './thumbnails.js'
import { createAudioEngine } from './audio.js'
import { initVoyageFlag } from './voyage-flag.js'
import { initWebglRelease } from './webgl-release.js'
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
const { ports, qrScanner } = bootGame({
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

// A voyage in progress is flagged in sessionStorage (`Ports.voyageInProgress`),
// so a load after the browser killed the tab mid-voyage is logged once.
initVoyageFlag(ports)

// Elm's WebGL never frees the context of a canvas it drops (the shipyard
// preview when a voyage starts, the sail view when it ends): release them,
// so repeated voyages do not pile up live contexts (webgl-release.js).
initWebglRelease()

// The sail canvas handles a lost WebGL context itself (`Pages.Sail`: the
// voyage waits, a toast says the graphics are being restored, and a restored
// context gets a fresh canvas). These only log it, for crash reports. The
// events do not bubble, hence the capture phase.
window.addEventListener('webglcontextlost', () => console.warn('[brick-sails] WebGL context lost'), true)
window.addEventListener('webglcontextrestored', () => console.warn('[brick-sails] WebGL context restored'), true)

ports?.storeLanguage?.subscribe((lang) => {
  document.documentElement.lang = lang
})
// Part pictures for the ship editor's dock (`Pages.Build`) and pictures of
// whole hulls and saved builds, from the same catalog the game loads its parts
// from. The hull pictures do not wait behind the dock's part pictures
// (see thumbnails.js).
initGameThumbnails(ports,import.meta.env.VITE_LDRAW_BASE || 'https://catalog.palikkaharrastajat.fi/raw')

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

// The ship editor's paste button: read a shared build (a link or LDraw text)
// from the clipboard. The browser may refuse (no permission, no API, an
// insecure page); the editor then says so and the text field still works.
ports?.readClipboard?.subscribe(async () => {
  if (!navigator.clipboard?.readText) {
    ports?.clipboardReadFailed?.send('unsupported')
    return
  }
  try {
    const text = await navigator.clipboard.readText()
    ports?.clipboardRead?.send(String(text ?? ''))
  } catch (err) {
    ports?.clipboardReadFailed?.send(err?.name === 'NotAllowedError' ? 'denied' : 'failed')
  }
})

// The share dialog's BrickLink Studio download: a .io archive holding the
// build as model.ldr (the same archive Brick Play downloads).
ports?.downloadStudioIo?.subscribe(({ filename, ldrText }) => {
  const zipData = zipSync({ 'model.ldr': strToU8(String(ldrText ?? '')) })
  const blob = new Blob([zipData], { type: 'application/octet-stream' })
  const url = URL.createObjectURL(blob)
  const link = document.createElement('a')
  link.href = url
  link.download = filename || 'ship.io'
  document.body.appendChild(link)
  link.click()
  link.remove()
  setTimeout(() => URL.revokeObjectURL(url), 0)
})

// Escape closes the camera's QR scanner (the shared runtime's overlay has its
// own close button and backdrop, but no key). It is caught before the editor's
// own Escape, which would otherwise also close the dialog under the scanner.
window.addEventListener(
  'keydown',
  (event) => {
    if (event.key !== 'Escape') return
    const scanner = document.getElementById('brick-qr-scanner-modal')
    if (scanner && scanner.style.display !== 'none') {
      event.stopImmediatePropagation()
      event.preventDefault()
      qrScanner?.close()
    }
  },
  true,
)
