// The sailing music (audio-music.js) is composed bar by bar from a seed:
// deterministic, in range, in key, and varied enough for a long voyage.

import { test } from 'node:test'
import assert from 'node:assert/strict'
import {
  RANGES,
  SECTION_BARS,
  STEPS_PER_BAR,
  composeBar,
  inKey,
  keyOf,
  midiToHz,
  sectionKind,
} from './audio-music.js'

const BARS = 2400 // an hour at 1.5 s a bar

test('the same seed and bar always give the same notes', () => {
  for (const bar of [0, 1, 17, 333, 2399]) {
    assert.deepEqual(composeBar(42, bar), composeBar(42, bar))
  }
})

test('every note is within its voice range, in key, and on a step of the bar', () => {
  for (const seed of [1, 7, 123456789]) {
    for (let bar = 0; bar < BARS; bar++) {
      const tonic = keyOf(bar)
      for (const note of composeBar(seed, bar)) {
        assert.ok(note.step >= 0 && note.step < STEPS_PER_BAR, `step ${note.step}`)
        assert.ok(note.vel > 0 && note.vel <= 1)
        assert.ok(note.length > 0 && note.length <= 2)
        const [lo, hi] = RANGES[note.voice]
        const notes = Array.isArray(note.midi) ? note.midi : [note.midi]
        for (const m of notes) {
          assert.ok(m >= lo - 12 && m <= hi, `${note.voice} ${m} out of range`)
          assert.ok(inKey(m, tonic), `${note.voice} ${m} not in key ${tonic} (bar ${bar})`)
        }
      }
    }
  }
})

test('bars stay light: few notes each, so few voices', () => {
  for (let bar = 0; bar < BARS; bar++) {
    assert.ok(composeBar(9, bar).length <= 9)
  }
})

test('the textures vary: all four kinds, no two rests in a row', () => {
  const kinds = []
  for (let s = 0; s < BARS / SECTION_BARS; s++) kinds.push(sectionKind(5, s))
  assert.deepEqual(new Set(kinds), new Set(['calm', 'tune', 'breeze', 'rest']))
  for (let i = 1; i < kinds.length; i++) {
    assert.ok(!(kinds[i] === 'rest' && kinds[i - 1] === 'rest'), `rest twice at ${i}`)
  }
  for (let i = 2; i < kinds.length; i++) {
    assert.ok(!(kinds[i] === 'tune' && kinds[i - 1] === 'tune' && kinds[i - 2] === 'tune'), `tune thrice at ${i}`)
  }
})

test('the tune does not repeat itself bar for bar', () => {
  const melodies = new Set()
  let tuneBars = 0
  for (let bar = 0; bar < 400; bar++) {
    const mel = composeBar(11, bar).filter((n) => n.voice === 'melody')
    if (mel.length === 0) continue
    tuneBars++
    melodies.add(JSON.stringify(mel.map((n) => [n.step, n.midi])))
  }
  assert.ok(tuneBars > 30)
  assert.ok(melodies.size > tuneBars * 0.4, `${melodies.size} distinct of ${tuneBars}`)
})

test('the key moves over a long voyage', () => {
  assert.equal(keyOf(0), 0)
  assert.notEqual(keyOf(40), keyOf(0))
})

test('midiToHz is concert pitch', () => {
  assert.equal(midiToHz(69), 440)
  assert.ok(Math.abs(midiToHz(60) - 261.63) < 0.01)
})
