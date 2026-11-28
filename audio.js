/**
 * Procedural Web Audio synthesizer for Brick Sails: no sound files, only
 * small synth voices, tuned to a soft toy-boat-by-the-sea feel (wooden
 * knocks, plastic brick clatter, plunks and bubbles, ship's bells, a toy
 * horn) rather than realistic war sounds.
 *
 * Built on the shared `BaseAudioEngine` (context, master gain, mute, unlock):
 *
 * - Ambience (per frame from `updateAudioState`): wind, water wash and a
 *   faint hull creak, plus a far-off gull now and then.
 * - Music (`audio-music.js`): an endless, gently varied 6/8 shanty lilt at a
 *   low level while the voyage is on, faded out when it is not.
 * - One-shot sounds (`playSfx`, see `SOUNDS`): the events of `Sails.Sound`.
 *   The shared `Game.Audio` events (`impact`, `checkpoint`, ...) still work.
 * - UI clicks: a soft wooden tick for any button pressed on the page.
 *
 * Long voyages must not pile up audio nodes (mobile browsers crash when they
 * do), so every sound is a voice of a bounded `VoicePool`
 * (`audio-voices.js`): at most `SFX_LIMITS.maxVoices` effects and
 * `MUSIC_LIMITS.maxVoices` music notes sound at once, and every node a voice
 * made is disconnected when it ends (or is stolen). Noise comes from two
 * buffers made once, never per sound.
 *
 * Nothing is created before the first user gesture (the shared runtime calls
 * `unlock()` on it); the context is suspended while the page is hidden.
 */

import {
  BaseAudioEngine,
  createNoiseLoop,
  createOscillatorLoop,
} from './vendor/packages/brick-frame-game/runtime/audio-engine.js'
import { VoicePool } from './audio-voices.js'
import { BAR_SECONDS, EIGHTH, composeBar, midiToHz } from './audio-music.js'

/** Level of the effects bus and of the music bus (under the master gain). */
export const SFX_LEVEL = 0.9
export const MUSIC_LEVEL = 0.3

/** Seconds of music scheduled ahead of the audio clock. */
export const MUSIC_LOOKAHEAD = 0.35

/** Seconds of silence after which the music starts a fresh tune. */
const MUSIC_RESTART_AFTER = 6

export const SFX_LIMITS = {
  maxVoices: 14,
  perKind: {
    cannon: 3,
    enemyCannon: 2,
    splash: 3,
    hitShip: 3,
    hitDock: 2,
    hitBuilding: 2,
    sunk: 2,
    collapse: 2,
    hullHit: 2,
    wrecked: 1,
    bump: 2,
    coins: 2,
    debrisCollected: 2,
    bell: 2,
    finish: 1,
    horn: 1,
    anchorDrop: 1,
    anchorWeigh: 1,
    thunder: 1,
    ghostAppear: 1,
    ghostBanish: 1,
    click: 2,
    gull: 1,
  },
  minInterval: {
    cannon: 0.06,
    enemyCannon: 0.15,
    splash: 0.07,
    hitShip: 0.05,
    hitDock: 0.05,
    hitBuilding: 0.05,
    hullHit: 0.08,
    bump: 0.2,
    coins: 0.08,
    debrisCollected: 0.1,
    click: 0.05,
    thunder: 0.5,
    gull: 4,
  },
}

export const MUSIC_LIMITS = { maxVoices: 10 }

/**
 * Old names (`Game.Audio` and earlier builds) for the sounds of `SOUNDS`.
 * `impact` maps by its strength in `resolveSound`.
 */
const ALIASES = {
  checkpoint: 'coins',
  treasure: 'coins',
  coin: 'coins',
  destroy: 'collapse',
}

/**
 * The one-shot sound a `playSfx` event asks for, as `{ kind, strength,
 * count }`, or `null` for an unknown one. Pure; exported for the tests.
 */
export function resolveSound(event) {
  const type = typeof event === 'string' ? event : event?.type
  if (typeof type !== 'string') return null
  const rawStrength = typeof event === 'object' && typeof event?.strength === 'number' ? event.strength : 1
  const strength = Math.max(0.1, Math.min(1.5, rawStrength))
  const rawCount = typeof event === 'object' && typeof event?.count === 'number' ? event.count : 1
  const count = Math.max(1, Math.min(100, Math.round(rawCount)))
  let kind = ALIASES[type] ?? type
  if (type === 'impact') kind = strength >= 0.95 ? 'hitShip' : 'hullHit'
  if (!(kind in SOUNDS)) return null
  return { kind, strength, count }
}

const vary = (x, pct = 0.06) => x * (1 + (Math.random() * 2 - 1) * pct)

function makeWhiteNoise(ctx, seconds = 1) {
  const len = Math.max(1, Math.floor(ctx.sampleRate * seconds))
  const buf = ctx.createBuffer(1, len, ctx.sampleRate)
  const data = buf.getChannelData(0)
  for (let i = 0; i < len; i++) data[i] = Math.random() * 2 - 1
  return buf
}

/* ── Voice building blocks ─────────────────────────────────────────────────
 * `v` is a voice under construction (see `startVoice`): `v.node(n)` keeps a
 * node for disconnecting, `v.source(s)` also waits for it to end.
 */

function envGain(v, t, peak, attack, decay) {
  const g = v.node(v.ctx.createGain())
  const p = Math.max(0.0002, peak)
  g.gain.setValueAtTime(0.0001, t)
  g.gain.exponentialRampToValueAtTime(p, t + Math.max(0.001, attack))
  g.gain.exponentialRampToValueAtTime(0.0001, t + Math.max(0.001, attack) + Math.max(0.01, decay))
  return g
}

/** One oscillator with a pluck envelope and an optional pitch glide. */
function tone(v, { type = 'sine', f, f2, t, attack = 0.004, decay = 0.2, peak = 0.2, glide, dest = v.out }) {
  const osc = v.source(v.ctx.createOscillator())
  osc.type = type
  osc.frequency.setValueAtTime(f, t)
  if (f2) osc.frequency.exponentialRampToValueAtTime(f2, t + (glide ?? attack + decay))
  const g = envGain(v, t, peak, attack, decay)
  osc.connect(g)
  g.connect(dest)
  osc.start(t)
  osc.stop(t + attack + decay + 0.03)
  return osc
}

/** A slice of a shared noise buffer through a filter and an envelope. */
function noise(v, { buffer, t, dur, type = 'lowpass', f, f2, Q, peak = 0.2, attack = 0.003, dest = v.out }) {
  const buf = buffer ?? v.white
  const src = v.source(v.ctx.createBufferSource())
  src.buffer = buf
  const filter = v.node(v.ctx.createBiquadFilter())
  filter.type = type
  filter.frequency.setValueAtTime(f, t)
  if (f2) filter.frequency.exponentialRampToValueAtTime(f2, t + dur)
  if (typeof Q === 'number') filter.Q.value = Q
  const g = envGain(v, t, peak, attack, Math.max(0.01, dur - attack))
  src.connect(filter)
  filter.connect(g)
  g.connect(dest)
  const room = Math.max(0, (buf.duration ?? 1) - dur - 0.05)
  src.start(t, Math.random() * room)
  src.stop(t + dur + 0.03)
  return src
}

/**
 * A run of struck notes on ONE oscillator and ONE gain: each note re-pitches
 * the oscillator and re-strikes the envelope (cut short where the next one
 * comes). A brick rattle or an arpeggio then costs two nodes, not two per
 * note. `notes`: `{ at, f, p, decay }`, absolute times.
 */
function strikes(v, { type = 'sine', notes, dest = v.out }) {
  if (notes.length === 0) return
  const list = notes.slice().sort((x, y) => x.at - y.at)
  const osc = v.source(v.ctx.createOscillator())
  osc.type = type
  const g = v.node(v.ctx.createGain())
  const t0 = list[0].at
  g.gain.setValueAtTime(0.0001, t0)
  let free = t0
  list.forEach((n, i) => {
    const at = Math.max(n.at, free)
    const nextAt = i + 1 < list.length ? Math.max(list[i + 1].at, at + 0.01) : Infinity
    const decay = Math.max(0.005, Math.min(n.decay, nextAt - at - 0.004))
    osc.frequency.setValueAtTime(n.f, at)
    g.gain.setValueAtTime(0.0001, at)
    g.gain.exponentialRampToValueAtTime(Math.max(0.0002, n.p), at + 0.002)
    g.gain.exponentialRampToValueAtTime(0.0001, at + 0.002 + decay)
    free = at + 0.003 + decay
  })
  osc.connect(g)
  g.connect(dest)
  osc.start(t0)
  osc.stop(free + 0.02)
}

/** Tiny bright ticks: plastic bricks knocking and tumbling. */
function clatter(v, { t, count, spread, lo, hi, peak, falling = false, type = 'triangle' }) {
  const notes = []
  for (let i = 0; i < count; i++) {
    const at = t + (falling ? (spread * i) / Math.max(1, count - 1) : Math.random() * spread)
    const span = falling ? 1 - i / Math.max(1, count) : Math.random()
    notes.push({ at, f: vary(lo + (hi - lo) * span, 0.08), decay: 0.03 + Math.random() * 0.03, p: peak * (0.6 + 0.4 * Math.random()) })
  }
  strikes(v, { type, notes })
}

/**
 * A run of music-box / marimba notes, `[delay, midi, peak, decay]` after
 * `t`: a sine with a faint fourth-octave shimmer.
 */
function chimes(v, t, run) {
  strikes(v, { notes: run.map(([at, midi, p, decay]) => ({ at: t + at, f: midiToHz(midi), p, decay })) })
  strikes(v, { notes: run.map(([at, midi, p, decay]) => ({ at: t + at, f: midiToHz(midi) * 4, p: p * 0.12, decay: decay * 0.25 })) })
}

/** A hollow wooden knock (toy hull, pier plank, woodblock). */
function woodKnock(v, t, f, peak) {
  tone(v, { type: 'triangle', f, f2: f * 0.8, t, attack: 0.002, decay: 0.09, peak })
  noise(v, { t, dur: 0.04, type: 'bandpass', f: f * 3, Q: 4, peak: peak * 0.5 })
}

/** A soft low thump. */
function thud(v, t, f, peak, decay = 0.22) {
  tone(v, { type: 'sine', f, f2: f * 0.45, t, attack: 0.003, decay, peak, glide: decay * 0.8 })
}

/** One music-box / marimba note. */
function chime(v, t, midi, peak, decay = 0.4) {
  chimes(v, t, [[0, midi, peak, decay]])
}

function plunk(v, t, s) {
  tone(v, { type: 'sine', f: vary(620, 0.1), f2: 180, t, attack: 0.003, decay: 0.12, peak: 0.32 * s, glide: 0.1 })
  noise(v, { t, dur: 0.42, type: 'bandpass', f: 1800, f2: 800, Q: 0.9, peak: 0.24 * s, attack: 0.015 })
  tone(v, { type: 'sine', f: vary(1000, 0.1), f2: 1600, t: t + 0.13, attack: 0.002, decay: 0.06, peak: 0.08 * s })
}

function cannonBoom(v, t, s, far) {
  thud(v, t, vary(150), 0.55 * s, 0.28)
  noise(v, { buffer: v.pink, t, dur: 0.34, type: 'lowpass', f: far ? 650 : 1500, f2: 220, peak: 0.55 * s, attack: 0.004 })
  if (!far) tone(v, { type: 'triangle', f: vary(520), f2: 260, t, attack: 0.002, decay: 0.05, peak: 0.16 * s })
}

function crumble(v, t, s) {
  noise(v, { buffer: v.pink, t, dur: 0.7, type: 'lowpass', f: 300, f2: 110, peak: 0.34 * s, attack: 0.02 })
  clatter(v, { t: t + 0.02, count: 6, spread: 0.45, lo: 1200, hi: 2700, peak: 0.12 * s, falling: true })
  thud(v, t, 90, 0.42 * s, 0.4)
}

function shipsBell(v, t, peak) {
  const base = vary(740, 0.02)
  const partials = [
    [1, 1, 1.6],
    [2.0, 0.45, 1.1],
    [2.76, 0.35, 0.8],
    [5.4, 0.15, 0.4],
  ]
  // Ship's bells strike in pairs, ding-ding: each partial is struck twice
  // on one oscillator.
  const second = 0.32
  for (const [ratio, amp, decay] of partials) {
    const osc = v.source(v.ctx.createOscillator())
    osc.type = 'sine'
    osc.frequency.setValueAtTime(base * ratio, t)
    const g = v.node(v.ctx.createGain())
    const p1 = Math.max(0.0002, peak * amp)
    const p2 = Math.max(0.0002, peak * amp * 0.6)
    g.gain.setValueAtTime(0.0001, t)
    g.gain.exponentialRampToValueAtTime(p1, t + 0.003)
    g.gain.exponentialRampToValueAtTime(Math.max(0.0001, p1 * Math.exp(-4 * second / decay)), t + second)
    g.gain.exponentialRampToValueAtTime(p2, t + second + 0.003)
    g.gain.exponentialRampToValueAtTime(0.0001, t + second + decay)
    osc.connect(g)
    g.connect(v.out)
    osc.start(t)
    osc.stop(t + second + decay + 0.03)
  }
}

/**
 * Every one-shot sound: `[seconds long, priority, play(v, t, sound)]`.
 * Priority decides which voices may be stolen when the pool is full.
 */
export const SOUNDS = {
  // The player's own broadside: a round toy "pomf" with a little pop.
  cannon: [0.42, 2, (v, t, { strength }) => cannonBoom(v, t, Math.min(1, strength), false)],
  // A pirate or patrol ship firing: the same, quieter and muffled by distance.
  enemyCannon: [0.4, 1, (v, t) => cannonBoom(v, t, 0.32, true)],
  // A ball dropping in the sea: a plunk, a fizz and one bubble.
  splash: [0.6, 1, (v, t, { strength }) => plunk(v, t, Math.min(1, 0.4 + strength * 0.6))],
  // A ball striking a ship: two wooden knocks and a few bricks rattling.
  hitShip: [0.32, 2, (v, t) => {
    woodKnock(v, t, vary(330), 0.42)
    woodKnock(v, t + 0.045, vary(250), 0.28)
    clatter(v, { t: t + 0.01, count: 3, spread: 0.09, lo: 1800, hi: 3200, peak: 0.12 })
  }],
  // A ball striking a harbour pier: a deeper plank knock and a thump.
  hitDock: [0.36, 2, (v, t) => {
    woodKnock(v, t, vary(190), 0.45)
    thud(v, t, 110, 0.32, 0.2)
    clatter(v, { t: t + 0.02, count: 2, spread: 0.08, lo: 1500, hi: 2600, peak: 0.1 })
  }],
  // A ball striking a building: a thump and a spray of bricks.
  hitBuilding: [0.36, 2, (v, t) => {
    thud(v, t, 140, 0.34, 0.18)
    clatter(v, { t, count: 4, spread: 0.14, lo: 2000, hi: 3800, peak: 0.13 })
    noise(v, { t, dur: 0.18, type: 'lowpass', f: 900, peak: 0.14 })
  }],
  // A pirate ship sinks: bricks tumbling down, a falling "womp", bubbles.
  sunk: [1.0, 3, (v, t) => {
    clatter(v, { t, count: 6, spread: 0.5, lo: 1400, hi: 3000, peak: 0.13, falling: true })
    tone(v, { type: 'triangle', f: 360, f2: 110, t, attack: 0.01, decay: 0.7, peak: 0.22 })
    noise(v, { buffer: v.pink, t, dur: 0.6, type: 'lowpass', f: 500, f2: 160, peak: 0.2, attack: 0.01 })
    for (const [at, f] of [
      [0.35, 300],
      [0.5, 380],
      [0.66, 470],
    ]) {
      tone(v, { type: 'sine', f: vary(f), f2: f * 1.7, t: t + at, attack: 0.003, decay: 0.07, peak: 0.1 })
    }
  }],
  // A pier or building falls down.
  collapse: [0.85, 3, (v, t) => crumble(v, t, 1)],
  // The player's own hull takes a knock.
  hullHit: [0.36, 2, (v, t, { strength }) => {
    const s = Math.min(1, strength)
    thud(v, t, 120, 0.45 * s, 0.25)
    woodKnock(v, t, vary(240), 0.3 * s)
    clatter(v, { t: t + 0.02, count: 2, spread: 0.1, lo: 1600, hi: 2800, peak: 0.1 * s })
  }],
  // The player's ship breaks up: a crumble and a sad little "wah-wah-wah".
  wrecked: [1.7, 3, (v, t) => {
    crumble(v, t, 1)
    chimes(v, t, [
      [0.4, 67, 0.2, 0.26],
      [0.68, 64, 0.2, 0.26],
      [0.96, 60, 0.2, 0.7],
    ])
  }],
  // Scraping a rock or reef: a soft grind and a bump.
  bump: [0.36, 2, (v, t) => {
    noise(v, { buffer: v.pink, t, dur: 0.3, type: 'lowpass', f: 650, f2: 200, peak: 0.24, attack: 0.01 })
    thud(v, t, 95, 0.3, 0.2)
  }],
  // Coins earned: the bright two-note "pling" (three for a big haul).
  coins: [0.6, 3, (v, t, { count }) => {
    const notes = count >= 20 ? [83, 88, 91] : [83, 88]
    chimes(v, t, notes.map((midi, i) => [i * 0.075, midi, 0.2, i === notes.length - 1 ? 0.4 : 0.15]))
  }],
  // Floating debris fished out to patch the hull: a quiet wooden tock and
  // a little rising tick, short and soft.
  debrisCollected: [0.22, 1, (v, t) => {
    woodKnock(v, t, vary(420), 0.14)
    tone(v, { type: 'sine', f: vary(1250, 0.04), f2: 1650, t: t + 0.05, attack: 0.002, decay: 0.08, peak: 0.06, glide: 0.06 })
  }],
  // A buoy or harbour passed: a ship's bell, ding-ding.
  bell: [2.0, 3, (v, t) => shipsBell(v, t, 0.22)],
  // The voyage is done: a little marimba fanfare and the bell.
  finish: [2.2, 3, (v, t) => {
    chimes(v, t, [72, 76, 79, 84].map((midi, i) => [i * 0.11, midi, 0.22, i === 3 ? 0.9 : 0.25]))
    shipsBell(v, t + 0.33, 0.12)
  }],
  // Race start: a toy steamer's horn, toot-tooot.
  horn: [1.4, 3, (v, t) => {
    for (const [at, len] of [
      [0, 0.22],
      [0.32, 0.8],
    ]) {
      const filter = v.node(v.ctx.createBiquadFilter())
      filter.type = 'lowpass'
      filter.frequency.value = 560
      filter.Q.value = 0.7
      const g = v.node(v.ctx.createGain())
      const s = t + at
      g.gain.setValueAtTime(0.0001, s)
      g.gain.exponentialRampToValueAtTime(0.2, s + 0.06)
      g.gain.setValueAtTime(0.2, s + len - 0.08)
      g.gain.exponentialRampToValueAtTime(0.0001, s + len + 0.12)
      filter.connect(g)
      g.connect(v.out)
      for (const f of [146.8, 147.9]) {
        const osc = v.source(v.ctx.createOscillator())
        osc.type = 'sawtooth'
        osc.frequency.setValueAtTime(f, s)
        osc.connect(filter)
        osc.start(s)
        osc.stop(s + len + 0.15)
      }
    }
  }],
  // Anchor dropped: a chain rattle and a plunk.
  anchorDrop: [1.0, 2, (v, t) => {
    clatter(v, { t, count: 7, spread: 0.32, lo: 2500, hi: 4200, peak: 0.08, type: 'square' })
    plunk(v, t + 0.36, 0.7)
  }],
  // Anchor weighed: a ratchet clicking up.
  anchorWeigh: [0.5, 2, (v, t) => {
    strikes(v, {
      type: 'triangle',
      notes: [0, 1, 2, 3, 4, 5].map((i) => ({ at: t + i * 0.06, f: 1800 + i * 160, p: 0.1, decay: 0.03 })),
    })
    woodKnock(v, t + 0.38, 300, 0.2)
  }],
  // Lightning: a soft far rumble with a faint crack.
  thunder: [1.5, 2, (v, t) => {
    noise(v, { buffer: v.pink, t, dur: 1.4, type: 'lowpass', f: 190, f2: 90, peak: 0.45, attack: 0.05 })
    noise(v, { t, dur: 0.08, type: 'highpass', f: 2000, peak: 0.08 })
  }],
  // The ghost ship appears: two slow wavering sines, a low hum.
  ghostAppear: [1.7, 2, (v, t) => {
    tone(v, { type: 'sine', f: 440, f2: 392, t, attack: 0.3, decay: 1.2, peak: 0.1 })
    tone(v, { type: 'sine', f: 446, f2: 396, t, attack: 0.3, decay: 1.2, peak: 0.08 })
    tone(v, { type: 'sine', f: 110, t, attack: 0.2, decay: 1.3, peak: 0.08 })
  }],
  // The ghost ship banished: a rising music-box sparkle.
  ghostBanish: [0.9, 3, (v, t) => {
    chimes(v, t, [84, 88, 91, 96].map((midi, i) => [i * 0.06, midi, 0.14, 0.3]))
  }],
  // A button pressed: a soft woodblock tick.
  click: [0.08, 1, (v, t) => {
    tone(v, { type: 'sine', f: vary(1500, 0.03), f2: 1100, t, attack: 0.001, decay: 0.03, peak: 0.1 })
    noise(v, { t, dur: 0.02, type: 'bandpass', f: 3000, Q: 5, peak: 0.05 })
  }],
  // A far-off gull, now and then while sailing.
  gull: [0.6, 0, (v, t) => {
    const g = vary(1, 0.08)
    tone(v, { type: 'sine', f: 1300 * g, f2: 1900 * g, t, attack: 0.03, decay: 0.18, peak: 0.04, glide: 0.08 })
    tone(v, { type: 'sine', f: 1800 * g, f2: 1250 * g, t: t + 0.22, attack: 0.02, decay: 0.22, peak: 0.035 })
  }],
}

export function createAudioEngine(initialMuted = false) {
  const engine = new BaseAudioEngine(initialMuted)

  const sfxPool = new VoicePool(SFX_LIMITS)
  const musicPool = new VoicePool(MUSIC_LIMITS)
  engine.sfxPool = sfxPool
  engine.musicPool = musicPool

  let gestured = false
  let sfxBus = null
  let musicBus = null
  let white = null
  let wind = null
  let wash = null
  let creak = null
  const music = { playing: false, bar: 0, nextBarAt: 0, seed: 1, stoppedAt: -Infinity }
  let nextGullAt = Infinity

  engine.initContinuousLoops = function (ctx, noiseBuffer, masterGain) {
    white = makeWhiteNoise(ctx, 1)

    // Effects go through a gentle compressor, so a busy battle stays soft.
    const comp = ctx.createDynamicsCompressor()
    comp.threshold.value = -18
    comp.knee.value = 12
    comp.ratio.value = 4
    comp.attack.value = 0.005
    comp.release.value = 0.2
    comp.connect(masterGain)
    sfxBus = ctx.createGain()
    sfxBus.gain.value = SFX_LEVEL
    sfxBus.connect(comp)

    // Music: its own bus, a little darkened, silent until the voyage is on.
    const musicTone = ctx.createBiquadFilter()
    musicTone.type = 'lowpass'
    musicTone.frequency.value = 3200
    musicTone.connect(masterGain)
    musicBus = ctx.createGain()
    musicBus.gain.value = 0
    musicBus.connect(musicTone)

    wind = createNoiseLoop(ctx, noiseBuffer, masterGain, { type: 'bandpass', frequency: 350, Q: 0.9 })
    wash = createNoiseLoop(ctx, noiseBuffer, masterGain, { type: 'lowpass', frequency: 180 })
    creak = createOscillatorLoop(ctx, masterGain, { type: 'triangle', frequency: 55 })
  }

  // Only a real user gesture (the shared runtime's listeners) may make the
  // context; per-frame telemetry before that must not.
  const baseUnlock = engine.unlock.bind(engine)
  engine.unlock = function () {
    gestured = true
    baseUnlock()
  }

  function ready() {
    if (!gestured) return null
    if (typeof document !== 'undefined' && document.hidden) return null
    return engine.ensureContext()
  }

  /**
   * Start a voice in `pool`: `build(v, t)` makes its nodes through
   * `v.node` / `v.source` into `v.out`. Every node is disconnected when the
   * last source ends, when the voice is stolen and stopped, or when the pool
   * prunes it after its planned end. Returns false when it did not play.
   */
  function startVoice(pool, kind, t, length, priority, bus, build, level = 1) {
    const ctx = engine.ctx
    const nodes = []
    const sources = []
    let done = false
    let out = null
    const disconnectAll = () => {
      if (done) return
      done = true
      for (const n of nodes) {
        try {
          n.disconnect()
        } catch {
          // Already disconnected.
        }
      }
      nodes.length = 0
      sources.length = 0
    }
    const id = pool.request(kind, ctx.currentTime, {
      endsAt: t + length,
      priority,
      onEvict: (stolen) => {
        if (!stolen || done) return disconnectAll()
        const now = ctx.currentTime
        try {
          out.gain.cancelScheduledValues(now)
          out.gain.setValueAtTime(out.gain.value, now)
          out.gain.linearRampToValueAtTime(0, now + 0.03)
        } catch {
          // A fake or closed context.
        }
        for (const s of sources) {
          try {
            s.stop(now + 0.04)
          } catch {
            // Not started or already stopped.
          }
        }
        // `ended` will disconnect; this is the safety net if it never comes.
        setTimeout(disconnectAll, 200)
      },
    })
    if (id === null) return false

    out = ctx.createGain()
    out.gain.value = level
    out.connect(bus)
    nodes.push(out)
    const v = {
      ctx,
      out,
      white,
      pink: engine.noiseBuffer,
      node: (n) => {
        nodes.push(n)
        return n
      },
      source: (s) => {
        nodes.push(s)
        sources.push(s)
        return s
      },
    }
    try {
      build(v, t)
    } catch {
      // A browser without some node feature: drop the sound, keep the game.
    }
    let left = sources.length
    const ended = () => {
      left -= 1
      if (left <= 0) {
        disconnectAll()
        pool.release(id)
      }
    }
    if (left === 0) ended()
    for (const s of sources.slice()) s.onended = ended
    return true
  }

  engine.playSfx = function (event) {
    if (engine.muted) return
    const sound = resolveSound(event)
    if (!sound || !ready() || !sfxBus) return
    const [length, priority, play] = SOUNDS[sound.kind]
    const t = engine.ctx.currentTime + 0.005
    startVoice(sfxPool, sound.kind, t, length, priority, sfxBus, (v, at) => play(v, at, sound))
  }

  /* ── Music ─────────────────────────────────────────────────────────── */

  function playNote(note, t) {
    const length = note.length
    const vel = note.vel
    switch (note.voice) {
      case 'bass':
        startVoice(musicPool, 'bass', t, length + 0.05, 2, musicBus, (v) => {
          tone(v, { type: 'triangle', f: midiToHz(note.midi), t, attack: 0.012, decay: length, peak: 0.5 * vel })
        })
        break
      case 'chord':
        startVoice(musicPool, 'chord', t, length + 0.1, 1, musicBus, (v) => {
          const g = envGain(v, t, 0.13 * vel, 0.01, length)
          g.connect(v.out)
          note.midi.forEach((m, i) => {
            const osc = v.source(v.ctx.createOscillator())
            osc.type = 'triangle'
            osc.frequency.setValueAtTime(midiToHz(m), t)
            osc.connect(g)
            osc.start(t + i * 0.014)
            osc.stop(t + length + 0.06)
          })
        })
        break
      case 'melody':
        startVoice(musicPool, 'melody', t, length + 0.05, 2, musicBus, (v) => chime(v, t, note.midi, 0.3 * vel, length))
        break
      case 'bell':
        startVoice(musicPool, 'bell', t, length + 0.05, 1, musicBus, (v) => {
          const f = midiToHz(note.midi)
          tone(v, { type: 'sine', f, t, attack: 0.003, decay: length, peak: 0.16 * vel })
          tone(v, { type: 'sine', f: f * 2.76, t, attack: 0.002, decay: length * 0.3, peak: 0.04 * vel })
        })
        break
      default:
        break
    }
  }

  function scheduleMusic(now) {
    if (!music.playing) {
      if (now - music.stoppedAt > MUSIC_RESTART_AFTER) {
        music.seed = (Math.random() * 4294967296) >>> 0
        music.bar = 0
      }
      music.playing = true
      music.nextBarAt = now + 0.15
    }
    // Fell far behind (a stalled frame, a resumed context): pick up from now
    // rather than play the missed bars in a heap.
    if (music.nextBarAt < now - 0.1) music.nextBarAt = now + 0.05
    while (music.nextBarAt < now + MUSIC_LOOKAHEAD) {
      for (const note of composeBar(music.seed, music.bar)) {
        playNote(note, music.nextBarAt + note.step * EIGHTH + Math.random() * 0.008)
      }
      music.bar += 1
      music.nextBarAt += BAR_SECONDS
    }
  }

  function stopMusic(now) {
    if (music.playing) music.stoppedAt = now
    music.playing = false
  }

  engine.updateContinuous = function (telemetry) {
    if (!telemetry || !ready()) return
    const now = engine.ctx.currentTime

    if (!telemetry.active || engine.muted) {
      wind?.level.gain.setTargetAtTime(0, now, 0.05)
      wash?.level.gain.setTargetAtTime(0, now, 0.05)
      creak?.level.gain.setTargetAtTime(0, now, 0.05)
      musicBus?.gain.setTargetAtTime(0, now, 0.6)
      stopMusic(now)
      nextGullAt = Infinity
      return
    }

    // Ambience, softer than the raw telemetry: it sits under the music.
    const wg = 0.55 * Math.max(0, Math.min(1.0, telemetry.windGain ?? 0))
    const wf = Math.max(100, Math.min(2400, telemetry.windHz ?? 350))
    wind?.level.gain.setTargetAtTime(wg, now, 0.08)
    wind?.filter.frequency.setTargetAtTime(wf, now, 0.08)

    const wg2 = 0.8 * Math.max(0, Math.min(1.0, telemetry.washGain ?? 0))
    const wf2 = Math.max(60, Math.min(900, telemetry.washHz ?? 180))
    wash?.level.gain.setTargetAtTime(wg2, now, 0.08)
    wash?.filter.frequency.setTargetAtTime(wf2, now, 0.08)

    const cg = Math.max(0, Math.min(0.15, telemetry.hullGain ?? 0))
    creak?.level.gain.setTargetAtTime(cg, now, 0.08)

    musicBus?.gain.setTargetAtTime(MUSIC_LEVEL, now, 0.8)
    scheduleMusic(now)

    if (nextGullAt === Infinity) nextGullAt = now + 10 + Math.random() * 20
    if (now >= nextGullAt) {
      engine.playSfx('gull')
      nextGullAt = now + 18 + Math.random() * 30
    }
  }

  const baseSetMuted = engine.setMuted.bind(engine)
  engine.setMuted = function (nextMuted) {
    baseSetMuted(nextMuted)
    if (engine.muted) {
      sfxPool.clear()
      musicPool.clear()
      if (engine.ctx) stopMusic(engine.ctx.currentTime)
    }
  }

  if (typeof document !== 'undefined' && document.addEventListener) {
    // Save the phone's battery and the audio thread while the game is out
    // of sight; the next frame after it is back resumes the context.
    document.addEventListener('visibilitychange', () => {
      if (!document.hidden || !engine.ctx) return
      stopMusic(engine.ctx.currentTime)
      engine.ctx.suspend?.().catch?.(() => {})
    })
    // Every button on the page gives a soft tick.
    document.addEventListener(
      'click',
      (event) => {
        const el = event.target?.closest?.('button, [role="button"], a[href], summary, select, input[type="checkbox"], input[type="radio"]')
        if (el && !el.disabled) engine.playSfx('click')
      },
      { capture: true, passive: true },
    )
  }

  return engine
}
