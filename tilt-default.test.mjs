// Tilt steering is the default on phones, and a saved choice wins over it.
// Brick Sails uses the shared calibrated tilt of brick-studio's
// tilt-steer.js with its own storage key, which must not change (players'
// saved choices live under it).

import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import {
  defaultTiltOn,
  resolveMode,
} from './vendor/packages/brick-frame-game/runtime/tilt-steer.js'

const screen = (coarse) => ({
  matchMedia: (query) => ({ matches: query === '(pointer: coarse)' && coarse }),
})

test('tilt is on by default on a touch phone, off on a desktop', () => {
  assert.equal(defaultTiltOn(screen(true)), true)
  assert.equal(defaultTiltOn(screen(false)), false)
})

test('a saved choice wins over the default', () => {
  assert.equal(resolveMode(null, true), true)
  assert.equal(resolveMode('touch', true), false)
  assert.equal(resolveMode('tilt', false), true)
  assert.equal(resolveMode(null, false), false)
})

test('the game keeps its tilt storage key', () => {
  const main = readFileSync(new URL('./main.js', import.meta.url), 'utf8')
  assert.match(main, /tiltSteer:\s*\{\s*storageKey:\s*'sails\.steerMode'\s*\}/)
})
