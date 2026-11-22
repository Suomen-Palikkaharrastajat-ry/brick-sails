import './main.css'
import { initBricksRuntime } from './vendor/packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { createAudioEngine } from './audio.js'
import { setupTilt } from './tilt.js'
import { createMultiplayerManager } from './vendor/packages/brick-frame-game/runtime/multiplayer.js'
import { createQrCameraScanner } from './vendor/packages/brick-frame-game/runtime/qr-camera-scanner.js'
import {
  readLanguage,
  readAudioMuted,
  readLeaderboard,
  decodeHashToByteArray,
  setupGameRuntime,
} from './vendor/packages/brick-frame-game/runtime/game-runtime.js'
import { Elm } from './src/Main.elm'

const ldrawBase = import.meta.env.VITE_LDRAW_BASE || 'https://catalog.palikkaharrastajat.fi/raw'
const indicesBase = import.meta.env.VITE_LDRAW_INDICES_BASE || 'https://catalog.palikkaharrastajat.fi/indices'

const coarsePointer =
  typeof window.matchMedia === 'function' && window.matchMedia('(pointer: coarse)').matches

// Default to user's example ship if no hash is present in URL
const defaultShipHash =
  '#b/FYixDYMwEADvwR-lI0XK7JBXXjaO6ChooaKHKRiHkg1YD1PcnXT9IDzUU7I1uJmrW8pr-LlH9X9sF81tzBZmPkDDs5jp-waBrQBdVdKMt9mlruCUMdRwCFrmiws'
const initialHash = window.location.hash || defaultShipHash
const savedAudioMuted = readAudioMuted('sails')
const audioEngine = createAudioEngine(savedAudioMuted)

const runtime = initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'viewer',
  ldrawBase,
  indicesBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE || '/raw',
  defaultModel: '',
  loadModelInJs: false,
  extraFlags: {
    coarsePointer,
    language: readLanguage(),
    savedAudioMuted,
    savedLeaderboardJson: readLeaderboard('sails'),
    baseUrl: window.location.origin + window.location.pathname,
    predecodedInitialBytes: decodeHashToByteArray(initialHash),
  },
  initialHash,
  syncUrlHash: true,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})

const ports = runtime.app?.ports
const multiplayer = createMultiplayerManager(ports)
const qrScanner = createQrCameraScanner(ports, {
  onScanned: (hash) => {
    const bytes = decodeHashToByteArray(hash)
    if (bytes) {
      ports?.binaryHashDecoded?.send(bytes)
      window.location.hash = hash
    }
  },
})

setupGameRuntime(ports, {
  storagePrefix: 'sails',
  audioEngine,
  multiplayer,
  qrScanner,
  playerPrefix: 'Sailor ',
})

setupTilt(ports)
