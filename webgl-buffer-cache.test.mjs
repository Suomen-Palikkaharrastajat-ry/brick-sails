import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createBufferCache, deleteEntryBuffers, installBufferCache, GLOBAL_NAME, IDLE_SWEEPS } from './webgl-buffer-cache.js'

function fakeGl({ connected = true, lost = false } = {}) {
  const deleted = []
  const gl = {
    canvas: { isConnected: connected },
    isContextLost: () => lost,
    deleteBuffer: (b) => deleted.push(b),
  }
  return { gl, deleted }
}

function fakeTimers() {
  const t = { set: 0, cleared: 0, fn: null }
  t.setInterval = (fn) => {
    t.set += 1
    t.fn = fn
    return 7
  }
  t.clearInterval = (id) => {
    if (id === 7) t.cleared += 1
  }
  return t
}

const entry = (name) => ({ indexBuffer: `${name}-index`, buffers: { position: `${name}-pos`, normal: `${name}-nrm` } })

test('the cache gets back what was set, like the WeakMap it replaces', () => {
  const { gl } = fakeGl()
  const cache = createBufferCache(gl, fakeTimers())
  const mesh = {}
  assert.equal(cache.get(mesh), undefined)
  cache.set(mesh, entry('a'))
  assert.deepEqual(cache.get(mesh), entry('a'))
  assert.equal(cache.has(mesh), true)
})

test('it sweeps on a timer', () => {
  const timers = fakeTimers()
  createBufferCache(fakeGl().gl, timers)
  assert.equal(timers.set, 1)
  assert.equal(typeof timers.fn, 'function')
})

test('a mesh left undrawn while frames go on is freed, with all its buffers', () => {
  const { gl, deleted } = fakeGl()
  const cache = createBufferCache(gl, fakeTimers())
  const kept = {}
  const dropped = {}
  cache.set(kept, entry('kept'))
  cache.set(dropped, entry('dropped'))
  for (let i = 0; i < IDLE_SWEEPS; i++) {
    cache.get(kept)
    assert.equal(cache.sweep(), 0)
  }
  cache.get(kept)
  assert.equal(cache.sweep(), 1)
  assert.deepEqual(deleted.sort(), ['dropped-index', 'dropped-nrm', 'dropped-pos'])
  assert.equal(cache.get(dropped), undefined)
  assert.deepEqual(cache.get(kept), entry('kept'))
  assert.deepEqual(cache.stats(), { meshes: 1, evicted: 1, deletedBuffers: 3, disposed: false })
})

test('a freed mesh drawn again is simply set again', () => {
  const { gl } = fakeGl()
  const cache = createBufferCache(gl, fakeTimers(), { idleSweeps: 0 })
  const mesh = {}
  const other = {}
  cache.set(mesh, entry('a'))
  cache.get(other)
  cache.set(other, entry('b'))
  assert.equal(cache.sweep(), 2)
  assert.equal(cache.get(mesh), undefined)
  cache.set(mesh, entry('c'))
  assert.deepEqual(cache.get(mesh), entry('c'))
})

test('without frames (a hidden tab) the idle clock stands still', () => {
  const { gl, deleted } = fakeGl()
  const cache = createBufferCache(gl, fakeTimers())
  cache.set({}, entry('a'))
  cache.sweep()
  for (let i = 0; i < 5 * IDLE_SWEEPS; i++) assert.equal(cache.sweep(), 0)
  assert.deepEqual(deleted, [])
  assert.equal(cache.stats().meshes, 1)
})

test('a canvas taken off the page stops the cache and frees what it held', () => {
  const { gl, deleted } = fakeGl()
  const timers = fakeTimers()
  const cache = createBufferCache(gl, timers)
  cache.set({}, entry('a'))
  cache.sweep()
  gl.canvas.isConnected = false
  cache.sweep()
  assert.equal(timers.cleared, 1)
  assert.equal(deleted.length, 3)
  assert.deepEqual(cache.stats(), { meshes: 0, evicted: 0, deletedBuffers: 3, disposed: true })
  assert.equal(cache.sweep(), 0)
})

test('a canvas not yet on the page is waited for, then given up on', () => {
  const { gl } = fakeGl({ connected: false })
  const timers = fakeTimers()
  const cache = createBufferCache(gl, timers, { unshownSweeps: 3 })
  cache.sweep()
  cache.sweep()
  assert.equal(cache.stats().disposed, false)
  cache.sweep()
  assert.equal(cache.stats().disposed, true)
  assert.equal(timers.cleared, 1)
})

test('a lost context deletes nothing (its buffers are gone already)', () => {
  const { gl, deleted } = fakeGl({ lost: true })
  assert.equal(deleteEntryBuffers(gl, entry('a')), 0)
  assert.deepEqual(deleted, [])
})

test('an entry without an index buffer frees its attribute buffers', () => {
  const { gl, deleted } = fakeGl()
  assert.equal(deleteEntryBuffers(gl, { indexBuffer: null, buffers: { position: 'p' } }), 1)
  assert.deepEqual(deleted, ['p'])
})

test('installBufferCache puts the factory where the patched package looks', () => {
  const target = {}
  const factory = installBufferCache(target)
  assert.equal(target[GLOBAL_NAME], factory)
  const timers = globalThis.setInterval
  const cache = factory(fakeGl().gl)
  assert.equal(factory.caches.length, 1)
  cache.dispose()
  assert.equal(factory.caches.length, 0)
  assert.equal(globalThis.setInterval, timers)
})
