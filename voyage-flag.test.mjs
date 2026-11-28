// The voyage flag (`voyage-flag.js`): set while a voyage is on, cleared when
// it ends normally, and a leftover one logged once on the next load. Storage
// that is missing or throws never breaks it.

import { test } from 'node:test'
import assert from 'node:assert/strict'
import { VOYAGE_FLAG_KEY, markVoyage, checkLeftoverVoyage, initVoyageFlag } from './voyage-flag.js'

function memoryStorage() {
  const items = new Map()
  return {
    getItem: (k) => (items.has(k) ? items.get(k) : null),
    setItem: (k, v) => items.set(k, String(v)),
    removeItem: (k) => items.delete(k),
    items,
  }
}

function throwingStorage() {
  const fail = () => {
    throw new Error('SecurityError')
  }
  return { getItem: fail, setItem: fail, removeItem: fail }
}

function port() {
  const listeners = []
  return {
    subscribe: (f) => listeners.push(f),
    send: (v) => listeners.forEach((f) => f(v)),
  }
}

test('a voyage sets the flag and a normal end clears it', () => {
  const storage = memoryStorage()
  assert.equal(markVoyage(true, storage), true)
  assert.ok(storage.getItem(VOYAGE_FLAG_KEY))
  assert.equal(markVoyage(false, storage), true)
  assert.equal(storage.getItem(VOYAGE_FLAG_KEY), null)
})

test('a leftover flag is logged once and cleared', () => {
  const storage = memoryStorage()
  markVoyage(true, storage)
  const logged = []
  const log = (m) => logged.push(m)
  assert.ok(checkLeftoverVoyage(storage, log))
  assert.equal(checkLeftoverVoyage(storage, log), null)
  assert.equal(logged.length, 1)
  assert.match(logged[0], /mid-voyage/)
  assert.equal(storage.getItem(VOYAGE_FLAG_KEY), null)
})

test('no flag, nothing logged', () => {
  const logged = []
  assert.equal(checkLeftoverVoyage(memoryStorage(), (m) => logged.push(m)), null)
  assert.deepEqual(logged, [])
})

test('missing or throwing storage never throws', () => {
  for (const storage of [null, throwingStorage()]) {
    assert.equal(markVoyage(true, storage), false)
    assert.equal(markVoyage(false, storage), false)
    assert.equal(checkLeftoverVoyage(storage, () => assert.fail('logged')), null)
  }
})

test('the port drives the flag after the leftover check', () => {
  const storage = memoryStorage()
  storage.setItem(VOYAGE_FLAG_KEY, '1700000000000')
  const logged = []
  const voyageInProgress = port()
  initVoyageFlag({ voyageInProgress }, storage, (m) => logged.push(m))
  assert.equal(logged.length, 1)
  assert.equal(storage.getItem(VOYAGE_FLAG_KEY), null)
  voyageInProgress.send(true)
  assert.ok(storage.getItem(VOYAGE_FLAG_KEY))
  voyageInProgress.send(false)
  assert.equal(storage.getItem(VOYAGE_FLAG_KEY), null)
})

test('no ports, no crash', () => {
  assert.doesNotThrow(() => initVoyageFlag(undefined, memoryStorage(), () => {}))
})

function fakeWindow() {
  const handlers = {}
  return {
    addEventListener: (type, f) => (handlers[type] = f),
    fire: (type, event = {}) => handlers[type]?.(event),
  }
}

test('a normal reload clears the flag, so it is not logged as a crash', () => {
  const storage = memoryStorage()
  const voyageInProgress = port()
  const win = fakeWindow()
  initVoyageFlag({ voyageInProgress }, storage, () => {}, win)
  voyageInProgress.send(true)
  win.fire('pagehide')
  assert.equal(storage.getItem(VOYAGE_FLAG_KEY), null)
})

test('a page restored from the back/forward cache gets its flag back', () => {
  const storage = memoryStorage()
  const voyageInProgress = port()
  const win = fakeWindow()
  initVoyageFlag({ voyageInProgress }, storage, () => {}, win)
  voyageInProgress.send(true)
  win.fire('pagehide')
  win.fire('pageshow', { persisted: true })
  assert.ok(storage.getItem(VOYAGE_FLAG_KEY))
})

test('hiding the page with no voyage running leaves storage alone', () => {
  const storage = memoryStorage()
  const win = fakeWindow()
  initVoyageFlag({ voyageInProgress: port() }, storage, () => {}, win)
  win.fire('pagehide')
  win.fire('pageshow', { persisted: true })
  assert.equal(storage.getItem(VOYAGE_FLAG_KEY), null)
})
