// The voice pool of the synthesizer (audio-voices.js) bounds how many sounds
// play at once, so a long battle cannot pile up Web Audio nodes.

import { test } from 'node:test'
import assert from 'node:assert/strict'
import { VoicePool } from './audio-voices.js'

test('the pool never holds more than maxVoices', () => {
  const pool = new VoicePool({ maxVoices: 4 })
  for (let i = 0; i < 50; i++) pool.request('hit', i * 0.01, { endsAt: 10 })
  assert.equal(pool.size, 4)
})

test('a full pool steals the oldest voice of the lowest priority', () => {
  const stolen = []
  const pool = new VoicePool({ maxVoices: 2 })
  pool.request('a', 0, { endsAt: 9, priority: 2, onEvict: (s) => stolen.push(['a', s]) })
  pool.request('b', 0.1, { endsAt: 9, priority: 1, onEvict: (s) => stolen.push(['b', s]) })
  assert.ok(pool.request('c', 0.2, { endsAt: 9, priority: 2 }))
  assert.deepEqual(stolen, [['b', true]])
})

test('a new voice does not steal more important ones', () => {
  const pool = new VoicePool({ maxVoices: 2 })
  pool.request('bell', 0, { endsAt: 9, priority: 3 })
  pool.request('bell', 0, { endsAt: 9, priority: 3 })
  assert.equal(pool.request('click', 0.1, { endsAt: 9, priority: 1 }), null)
  assert.equal(pool.size, 2)
})

test('each kind has its own cap, stealing its own oldest first', () => {
  const stolen = []
  const pool = new VoicePool({ maxVoices: 10, perKind: { splash: 2 } })
  for (let i = 0; i < 5; i++) pool.request('splash', i, { endsAt: 100, onEvict: () => stolen.push(i) })
  assert.equal(pool.countOf('splash'), 2)
  assert.deepEqual(stolen, [0, 1, 2])
})

test('a kind may not start again within its minimum interval', () => {
  const pool = new VoicePool({ maxVoices: 10, minInterval: { splash: 0.1 } })
  assert.ok(pool.request('splash', 1.0, { endsAt: 2 }))
  assert.equal(pool.request('splash', 1.05, { endsAt: 2 }), null)
  assert.ok(pool.request('splash', 1.11, { endsAt: 2 }))
  assert.ok(pool.request('hit', 1.11, { endsAt: 2 }))
})

test('released and long-ended voices free their slot', () => {
  const pruned = []
  const pool = new VoicePool({ maxVoices: 3, grace: 0.25 })
  const a = pool.request('a', 0, { endsAt: 0.5 })
  pool.request('b', 0, { endsAt: 0.5, onEvict: (s) => pruned.push(s) })
  pool.release(a)
  assert.equal(pool.size, 1)
  pool.prune(0.6)
  assert.equal(pool.size, 1)
  pool.prune(0.8)
  assert.equal(pool.size, 0)
  assert.deepEqual(pruned, [false])
})

test('clear stops every voice', () => {
  const stolen = []
  const pool = new VoicePool({ maxVoices: 5 })
  for (let i = 0; i < 3; i++) pool.request('x', 0, { endsAt: 9, onEvict: (s) => stolen.push(s) })
  pool.clear()
  assert.equal(pool.size, 0)
  assert.deepEqual(stolen, [true, true, true])
})
