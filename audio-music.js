/**
 * The sailing music of Brick Sails: a gentle, endless 6/8 sea-shanty lilt,
 * composed bar by bar from a seed. Pure (no Web Audio): `audio.js` asks for
 * the notes of each bar just ahead of time and plays them on three small
 * synth voices. Tested in `audio-music.test.mjs`.
 *
 * Why it does not tire over a long voyage:
 * - Nothing loops verbatim: every bar's melody is drawn afresh from a seeded
 *   random walk over the chord, and sections of eight bars change texture
 *   (`SECTION_KINDS`): plain accompaniment, a tune on top, a sparse "breeze"
 *   with a high music-box line, and "rest" bars of almost nothing.
 * - The key moves every 32 bars (C, F, G major), so the colour shifts slowly.
 * - Soft timbres only (triangle bass, plucked chords, music box), at a low
 *   level under the sound effects.
 */

/** Seconds per eighth note: a dotted-quarter pulse of 80 per minute. */
export const EIGHTH = 0.25

/** Eighths per bar (6/8). */
export const STEPS_PER_BAR = 6

/** Seconds per bar. */
export const BAR_SECONDS = EIGHTH * STEPS_PER_BAR

/** Bars per section, and sections per key. */
export const SECTION_BARS = 8
export const KEY_BARS = 32

/** Tonic offsets (semitones from C) the music moves through. */
export const KEYS = [0, 5, -5]

/** Section textures. */
export const SECTION_KINDS = ['calm', 'tune', 'breeze', 'rest']

/** The major scale, in semitones. */
const MAJOR = [0, 2, 4, 5, 7, 9, 11]

/** Four-bar chord progressions, as scale degrees (0 = I). */
const PROGRESSIONS = [
  [0, 3, 0, 4], // I  IV I  V
  [0, 5, 3, 4], // I  vi IV V
  [5, 3, 0, 4], // vi IV I  V
  [0, 4, 5, 3], // I  V  vi IV
  [3, 0, 4, 0], // IV I  V  I
]

/** Melody rhythms for one bar of 6/8: the eighths a note starts on. */
const RHYTHMS = [
  [0, 2, 3, 5],
  [0, 3, 4],
  [0, 1, 2, 3, 5],
  [0, 3],
  [0, 2, 4],
  [0, 3, 5],
]

/** Note ranges (MIDI) of each voice. */
export const RANGES = {
  bass: [40, 57],
  chord: [55, 74],
  melody: [69, 88],
  bell: [79, 98],
}

/** A small deterministic PRNG (mulberry32). */
export function rng(seed) {
  let a = seed >>> 0
  return function next() {
    a = (a + 0x6d2b79f5) >>> 0
    let t = a
    t = Math.imul(t ^ (t >>> 15), t | 1)
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61)
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

function hash(seed, a, b = 0) {
  let h = (seed | 0) ^ Math.imul(a + 1, 0x9e3779b1) ^ Math.imul(b + 7, 0x85ebca77)
  h = Math.imul(h ^ (h >>> 16), 0x7feb352d)
  h = Math.imul(h ^ (h >>> 15), 0x846ca68b)
  return (h ^ (h >>> 16)) >>> 0
}

function pick(next, list) {
  return list[Math.floor(next() * list.length) % list.length]
}

export function midiToHz(midi) {
  return 440 * Math.pow(2, (midi - 69) / 12)
}

/** The texture of section `index`: never two rests or three tunes in a row. */
export function sectionKind(seed, index) {
  if (index <= 0) return 'calm'
  // Each section depends on the two before it: computed forwards once per
  // seed and remembered (a voyage of an hour is about 300 sections).
  let memo = sectionMemo.get(seed)
  if (!memo) {
    memo = ['calm']
    sectionMemo.clear()
    sectionMemo.set(seed, memo)
  }
  while (memo.length <= index) {
    const i = memo.length
    memo.push(nextSectionKind(seed, i, memo[i - 1], i >= 2 ? memo[i - 2] : 'calm'))
  }
  return memo[index]
}

const sectionMemo = new Map()

function nextSectionKind(seed, index, prev, prev2) {
  const next = rng(hash(seed, index, 1))
  const weights = { calm: 3, tune: 4, breeze: 2, rest: 1 }
  if (prev === 'rest') weights.rest = 0
  if (prev === 'tune' && prev2 === 'tune') weights.tune = 0
  if (prev === 'breeze') weights.breeze = 0.5
  const total = Object.values(weights).reduce((s, w) => s + w, 0)
  let r = next() * total
  for (const kind of SECTION_KINDS) {
    r -= weights[kind]
    if (r < 0) return kind
  }
  return 'calm'
}

/** Tonic of `bar` (semitones from C). */
export function keyOf(bar) {
  return KEYS[Math.floor(Math.max(0, bar) / KEY_BARS) % KEYS.length]
}

/** Scale-degree root of the chord of `bar`. */
export function chordDegree(seed, bar) {
  const phrase = Math.floor(bar / 4)
  const prog = PROGRESSIONS[hash(seed, phrase, 2) % PROGRESSIONS.length]
  return prog[bar % 4]
}

/** Semitone (from the tonic) of scale degree `d` (may exceed an octave). */
function degreeSemis(d) {
  const oct = Math.floor(d / 7)
  return MAJOR[((d % 7) + 7) % 7] + 12 * oct
}

/** MIDI notes of the triad on degree `root` near `low`, in the key `tonic`. */
function triad(tonic, root, low) {
  const notes = [root, root + 2, root + 4].map((d) => 60 + tonic + degreeSemis(d))
  return notes.map((n) => {
    let m = n
    while (m < low) m += 12
    while (m >= low + 12) m -= 12
    return m
  })
}

function fit(midi, [lo, hi]) {
  let m = midi
  while (m < lo) m += 12
  while (m > hi) m -= 12
  return m
}

/** Whether `midi` is in the major scale of `tonic`. */
export function inKey(midi, tonic) {
  return MAJOR.includes((((midi - 60 - tonic) % 12) + 12) % 12)
}

/**
 * The notes of `bar`: a list of `{ step, voice, midi, length, vel }`, where
 * `step` is the eighth (0..5) the note starts on, `length` its ring in
 * seconds and `vel` its loudness (0..1). Voices: `bass`, `chord` (one event
 * per strum, its `midi` a list of three notes), `melody` and `bell`.
 */
export function composeBar(seed, bar) {
  const b = Math.max(0, Math.floor(bar))
  const tonic = keyOf(b)
  const kind = sectionKind(seed, Math.floor(b / SECTION_BARS))
  const inSection = b % SECTION_BARS
  const root = chordDegree(seed, b)
  const next = rng(hash(seed, b, 3))
  const events = []

  const bassRoot = fit(60 + tonic + degreeSemis(root) - 24, RANGES.bass)
  const bassFifth = fit(bassRoot + 7, RANGES.bass)
  const chord = triad(tonic, root, 60)

  if (kind === 'rest') {
    // A breath: one soft strum on the downbeat, every other bar.
    if (inSection % 2 === 0) events.push({ step: 0, voice: 'chord', midi: chord, length: 1.4, vel: 0.45 })
    if (inSection % 4 === 0) events.push({ step: 0, voice: 'bass', midi: bassRoot, length: 1.2, vel: 0.5 })
    return events
  }

  events.push({ step: 0, voice: 'bass', midi: bassRoot, length: 0.6, vel: 0.8 })
  if (kind !== 'breeze') {
    events.push({ step: 3, voice: 'bass', midi: next() < 0.7 ? bassFifth : bassRoot, length: 0.5, vel: 0.6 })
    events.push({ step: 2, voice: 'chord', midi: chord, length: 0.35, vel: 0.5 })
    events.push({ step: 5, voice: 'chord', midi: chord, length: 0.35, vel: 0.42 })
  } else {
    events.push({ step: 0, voice: 'chord', midi: chord, length: 1.1, vel: 0.35 })
  }

  if (kind === 'tune') {
    // A random walk over the scale, landing on chord tones on strong beats;
    // every fourth bar ends its phrase on one long chord tone.
    const phraseEnd = inSection % 4 === 3
    const rhythm = phraseEnd ? [0, 3] : pick(next, RHYTHMS)
    const chordPcs = chord.map((n) => (n - 60 - tonic + 120) % 12)
    let degree = 7 + root + pick(next, [0, 2, 4])
    for (const step of rhythm) {
      const strong = step === 0 || step === 3
      if (!strong) degree += pick(next, [-2, -1, -1, 1, 1, 2])
      let midi = fit(60 + tonic + degreeSemis(degree), RANGES.melody)
      if (strong && !chordPcs.includes((midi - 60 - tonic + 120) % 12)) {
        midi = fit(chord[Math.floor(next() * 3)] + 12, RANGES.melody)
      }
      const last = step === rhythm[rhythm.length - 1]
      events.push({
        step,
        voice: 'melody',
        midi,
        length: phraseEnd && last ? 1.2 : 0.45,
        vel: strong ? 0.75 : 0.55,
      })
    }
  } else if (kind === 'breeze') {
    // A few high music-box notes, like light on the water.
    const count = next() < 0.5 ? 1 : 2
    const steps = count === 1 ? [pick(next, [0, 3])] : [0, pick(next, [3, 4])]
    for (const step of steps) {
      const midi = fit(pick(next, chord) + 24, RANGES.bell)
      events.push({ step, voice: 'bell', midi, length: 1.0, vel: 0.5 })
    }
  }

  return events
}
