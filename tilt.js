// Tilt steering for Brick Sails.
//
// Reads DeviceOrientationEvent, turns the phone's roll (relative to the
// screen, so it works in portrait and landscape) into a steering value
// -1 .. 1 and sends it to Elm through the `tiltSteer` port. Elm stays pure:
// it only receives numbers and a status record.
//
// - Neutral point: the angle at race start (`calibrateTilt` port) is
//   "straight ahead". Rotating the screen re-calibrates.
// - Dead zone of a few degrees, full helm at about 22 degrees.
// - On by default on phones (`(pointer: coarse)`), off elsewhere.
// - On/off toggle: any element with `data-tilt-toggle`. The choice is saved
//   in localStorage and overrides the default. On iOS the permission prompt must run inside a tap,
//   so the toggle and the start button (`data-tilt-start`) are handled here,
//   in the click itself, not through an Elm command.
// - If permission is denied or no sensor exists, tilt quietly switches off
//   and the on-screen buttons keep working.

import { getScreenTiltDegrees } from './vendor/packages/brick-frame-game/runtime/game-runtime.js'

const STORAGE_KEY = 'sails.steerMode'
const DEAD_ZONE_DEG = 3
const FULL_LOCK_DEG = 22

// Phones (touch-primary screens) start with tilt on; other devices start with
// buttons. A choice saved with the toggle always wins over this default.
export function defaultTiltOn(win = typeof window !== 'undefined' ? window : undefined) {
  try {
    return typeof win?.matchMedia === 'function' && win.matchMedia('(pointer: coarse)').matches
  } catch {
    return false
  }
}

export function resolveMode(saved, phoneDefault) {
  if (saved === 'tilt') return true
  if (saved === 'touch') return false
  return phoneDefault
}

function readMode() {
  let saved = null
  try {
    saved = window.localStorage.getItem(STORAGE_KEY)
  } catch {}
  return resolveMode(saved, defaultTiltOn())
}

function saveMode(tilt) {
  try {
    window.localStorage.setItem(STORAGE_KEY, tilt ? 'tilt' : 'touch')
  } catch {}
}

export function steerFromAngle(angleDeg, neutralDeg) {
  let d = angleDeg - neutralDeg
  if (d > 180) d -= 360
  if (d < -180) d += 360
  const mag = Math.abs(d)
  if (mag <= DEAD_ZONE_DEG) return 0
  const v = Math.min(1, (mag - DEAD_ZONE_DEG) / (FULL_LOCK_DEG - DEAD_ZONE_DEG))
  return Math.sign(d) * v
}

export function setupTilt(ports) {
  const hasApi = typeof window !== 'undefined' && 'DeviceOrientationEvent' in window
  const hasTouch =
    (typeof navigator !== 'undefined' && navigator.maxTouchPoints > 0) ||
    (typeof window.matchMedia === 'function' && window.matchMedia('(pointer: coarse)').matches)
  const available = hasApi && hasTouch
  const needsPermission =
    hasApi && typeof window.DeviceOrientationEvent.requestPermission === 'function'

  let enabled = available && readMode()
  let permitted = !needsPermission
  let listening = false
  let lastAngle = null
  let neutral = null
  let lastSent = null

  function sendStatus() {
    ports?.tiltStatus?.send({ available, enabled })
  }

  function send(v) {
    // Skip repeats so Elm is not woken 60 times a second for nothing.
    if (lastSent !== null && Math.abs(v - lastSent) < 0.01) return
    lastSent = v
    ports?.tiltSteer?.send(v)
  }

  function onOrientation(event) {
    if (typeof event.gamma !== 'number' && typeof event.beta !== 'number') return
    const angle = getScreenTiltDegrees(event.beta ?? 0, event.gamma ?? 0)
    lastAngle = angle
    if (neutral === null) neutral = angle
    if (enabled) send(steerFromAngle(angle, neutral))
  }

  function listen() {
    if (listening) return
    listening = true
    window.addEventListener('deviceorientation', onOrientation, true)
  }

  function turnOff() {
    enabled = false
    saveMode(false)
    send(0)
    sendStatus()
  }

  // Must be called synchronously from a tap.
  function enable() {
    if (!available) return
    if (permitted) {
      enabled = true
      saveMode(true)
      neutral = null
      listen()
      sendStatus()
      return
    }
    let request
    try {
      request = window.DeviceOrientationEvent.requestPermission()
    } catch {
      turnOff()
      return
    }
    Promise.resolve(request)
      .then((result) => {
        if (result === 'granted') {
          permitted = true
          enabled = true
          saveMode(true)
          neutral = null
          listen()
          sendStatus()
        } else {
          turnOff()
        }
      })
      .catch(turnOff)
  }

  document.addEventListener(
    'click',
    (event) => {
      const target = event.target instanceof Element ? event.target : null
      if (!target) return
      if (target.closest('[data-tilt-toggle]')) {
        if (enabled) {
          turnOff()
        } else {
          enable()
        }
      } else if (target.closest('[data-tilt-start]') && enabled && !listening) {
        enable()
      }
    },
    true,
  )

  ports?.calibrateTilt?.subscribe(() => {
    neutral = lastAngle
    lastSent = null
    if (enabled) send(0)
  })

  const recalibrate = () => {
    neutral = null
  }
  window.addEventListener('orientationchange', recalibrate)
  window.screen?.orientation?.addEventListener?.('change', recalibrate)

  // Without an iOS-style prompt, tilt (saved or default) can start listening
  // now. With one, the first tap on the start button asks for permission.
  if (enabled && permitted) listen()
  sendStatus()
  setTimeout(sendStatus, 0)
}
