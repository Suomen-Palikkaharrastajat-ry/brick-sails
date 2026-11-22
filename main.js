import './main.css'
import { initBricksRuntime } from './vendor/packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { decodeHash, isBinaryHash } from './vendor/packages/brick-frame-viewport/runtime/url-hash-codec.js'
import { createAudioEngine } from './audio.js'
import { Elm } from './src/Main.elm'

const ldrawBase = import.meta.env.VITE_LDRAW_BASE || 'https://catalog.palikkaharrastajat.fi/raw'
const indicesBase = import.meta.env.VITE_LDRAW_INDICES_BASE || 'https://catalog.palikkaharrastajat.fi/indices'

const LANG_KEY = 'palikkakehys.language'
const AUDIO_MUTED_KEY = 'palikkakehys.sails.audioMuted'
const LEADERBOARD_KEY = 'palikkakehys.sails.leaderboard'

function readLanguage() {
  try {
    const hashParams = new URLSearchParams(window.location.hash.replace(/^#/, ''))
    const searchParams = new URLSearchParams(window.location.search)
    const lang = searchParams.get('lang') || hashParams.get('lang')
    if (lang) {
      return lang.toLowerCase().startsWith('fi') ? 'fi' : 'en'
    }
    const stored = window.localStorage.getItem(LANG_KEY)
    if (stored) {
      return stored.toLowerCase().startsWith('fi') ? 'fi' : 'en'
    }
    return 'en'
  } catch {
    return 'en'
  }
}

function readAudioMuted() {
  try {
    return window.localStorage.getItem(AUDIO_MUTED_KEY) === '1'
  } catch {
    return false
  }
}

function readLeaderboard() {
  try {
    return window.localStorage.getItem(LEADERBOARD_KEY) || '[]'
  } catch {
    return '[]'
  }
}

function extractBinaryHashFragment(rawInput) {
  const str = String(rawInput ?? '').trim()
  const hashIdx = str.indexOf('#')
  const candidate = hashIdx >= 0 ? str.slice(hashIdx) : str

  const parts = candidate.replace(/^#/, '').split('&')
  for (const part of parts) {
    if (part.startsWith('b/')) {
      return `#${part}`
    }
  }
  if (isBinaryHash(candidate)) {
    return candidate.startsWith('#') ? candidate : `#${candidate}`
  }
  return null
}

function decodeHashToByteArray(rawHash) {
  const binaryFrag = extractBinaryHashFragment(rawHash)
  if (!binaryFrag) return null
  try {
    const bytes = decodeHash(binaryFrag)
    return bytes ? Array.from(bytes) : null
  } catch {
    return null
  }
}

const coarsePointer =
  typeof window.matchMedia === 'function' && window.matchMedia('(pointer: coarse)').matches

// Default to user's example ship if no hash is present in URL
const defaultShipHash = '#b/FYixDYMwEADvwR-lI0XK7JBXXjaO6ChooaKHKRiHkg1YD1PcnXT9IDzUU7I1uJmrW8pr-LlH9X9sF81tzBZmPkDDs5jp-waBrQBdVdKMt9mlruCUMdRwCFrmiws'
const initialHash = window.location.hash || defaultShipHash
const savedAudioMuted = readAudioMuted()
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
    savedLeaderboardJson: readLeaderboard(),
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

const unlockAudioOnce = () => {
  audioEngine.unlock()
}
window.addEventListener('pointerdown', unlockAudioOnce, { passive: true })
window.addEventListener('keydown', unlockAudioOnce, { passive: true })
window.addEventListener('touchstart', unlockAudioOnce, { passive: true })

ports?.updateAudioState?.subscribe((state) => {
  audioEngine.updateContinuous(state)
})

ports?.playSfx?.subscribe((event) => {
  audioEngine.playSfx(event)
})

ports?.setAudioMuted?.subscribe((nextMuted) => {
  audioEngine.setMuted(nextMuted)
  try {
    window.localStorage.setItem(AUDIO_MUTED_KEY, nextMuted ? '1' : '0')
  } catch {
    // Ignore storage errors
  }
})

ports?.storeLanguage?.subscribe((lang) => {
  try {
    window.localStorage.setItem(LANG_KEY, lang)
  } catch {
    // Ignore storage errors
  }
})

ports?.saveLeaderboard?.subscribe((jsonStr) => {
  try {
    window.localStorage.setItem(LEADERBOARD_KEY, String(jsonStr ?? '[]'))
  } catch {
    // Ignore storage errors
  }
})

ports?.decodeBinaryHash?.subscribe((rawInput) => {
  const bytes = decodeHashToByteArray(rawInput)
  ports?.binaryHashDecoded?.send(bytes)
})

ports?.copyShareUrl?.subscribe((hashOrUrl) => {
  const full = String(hashOrUrl ?? '').startsWith('#')
    ? new URL(hashOrUrl, window.location.href).href
    : String(hashOrUrl ?? '')
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(full).catch(() => {})
  }
})

function getScreenTiltDegrees(betaDeg, gammaDeg) {
  const degToRad = Math.PI / 180
  const radToDeg = 180 / Math.PI
  const b = betaDeg * degToRad
  const g = gammaDeg * degToRad

  const ux = -Math.cos(b) * Math.sin(g)
  const uy = Math.sin(b)
  const uz = Math.cos(b) * Math.cos(g)

  const legacyAngle = typeof window.orientation === 'number' ? window.orientation : null
  const modernAngle =
    typeof window.screen !== 'undefined' &&
    window.screen.orientation &&
    typeof window.screen.orientation.angle === 'number'
      ? window.screen.orientation.angle
      : null
  const mediaLandscape =
    typeof window.matchMedia === 'function' && window.matchMedia('(orientation: landscape)').matches

  const isLandscape =
    mediaLandscape ||
    window.innerWidth > window.innerHeight ||
    legacyAngle === 90 ||
    legacyAngle === -90 ||
    modernAngle === 90 ||
    modernAngle === 270

  let negURight
  let uTop
  if (isLandscape) {
    let topIsPosX
    if (modernAngle === 90) {
      topIsPosX = true
    } else if (modernAngle === 270) {
      topIsPosX = false
    } else if (legacyAngle === 90) {
      topIsPosX = true
    } else if (legacyAngle === -90) {
      topIsPosX = false
    } else {
      topIsPosX = ux > 0.05
    }

    if (topIsPosX) {
      uTop = ux
      negURight = uy
    } else {
      uTop = -ux
      negURight = -uy
    }
  } else {
    uTop = uy
    negURight = -ux
  }

  const screenRollRad = Math.atan2(negURight, Math.max(0.01, uTop))
  return screenRollRad * radToDeg
}

function handleOrientation(evt) {
  if (typeof evt.beta !== 'number' || typeof evt.gamma !== 'number') return
  const tiltDeg = getScreenTiltDegrees(evt.beta, evt.gamma)
  const clamped = Math.max(-45, Math.min(45, tiltDeg))
  const normalized = clamped / 45.0
  ports?.tiltOrientationReceived?.send(normalized)
}

ports?.requestTiltPermission?.subscribe(() => {
  if (
    typeof DeviceOrientationEvent !== 'undefined' &&
    typeof DeviceOrientationEvent.requestPermission === 'function'
  ) {
    DeviceOrientationEvent.requestPermission()
      .then((perm) => {
        if (perm === 'granted') {
          window.addEventListener('deviceorientation', handleOrientation, true)
        }
      })
      .catch(() => {})
  } else if (typeof window !== 'undefined' && 'ondeviceorientation' in window) {
    window.addEventListener('deviceorientation', handleOrientation, true)
  }
})
