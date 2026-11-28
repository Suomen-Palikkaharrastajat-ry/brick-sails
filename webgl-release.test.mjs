import { test } from 'node:test'
import assert from 'node:assert/strict'
import { RELEASE_ATTRIBUTE, markedCanvases, releaseCanvas, releaseRemoved, initWebglRelease } from './webgl-release.js'

function fakeCanvas({ marked = true, connected = false, lost = false, ext = true, kind = 'webgl' } = {}) {
  const calls = { lose: 0, getContext: [] }
  const gl = {
    isContextLost: () => lost,
    getExtension: (name) => (ext && name === 'WEBGL_lose_context' ? { loseContext: () => { calls.lose += 1 } } : null),
  }
  const canvas = {
    nodeType: 1,
    tagName: 'CANVAS',
    isConnected: connected,
    hasAttribute: (name) => marked && name === RELEASE_ATTRIBUTE,
    querySelectorAll: () => [],
    getContext: (type) => {
      calls.getContext.push(type)
      return type === kind ? gl : null
    },
  }
  return { canvas, calls }
}

function fakeDiv(children) {
  return { nodeType: 1, tagName: 'DIV', hasAttribute: () => false, querySelectorAll: () => children }
}

test('a detached marked canvas loses its context', () => {
  const { canvas, calls } = fakeCanvas()
  assert.equal(releaseCanvas(canvas), true)
  assert.equal(calls.lose, 1)
})

test('a canvas made with experimental-webgl is found too', () => {
  const { canvas, calls } = fakeCanvas({ kind: 'experimental-webgl' })
  assert.equal(releaseCanvas(canvas), true)
  assert.deepEqual(calls.getContext, ['webgl', 'experimental-webgl'])
})

test('a canvas back on the page is kept', () => {
  const { canvas, calls } = fakeCanvas({ connected: true })
  assert.equal(releaseCanvas(canvas), false)
  assert.equal(calls.lose, 0)
  assert.deepEqual(calls.getContext, [])
})

test('an already lost context or a missing extension is left alone', () => {
  const lost = fakeCanvas({ lost: true })
  assert.equal(releaseCanvas(lost.canvas), false)
  assert.equal(lost.calls.lose, 0)
  assert.equal(releaseCanvas(fakeCanvas({ ext: false }).canvas), false)
})

test('a throwing canvas does not throw', () => {
  const canvas = { isConnected: false, getContext: () => { throw new Error('boom') } }
  assert.equal(releaseCanvas(canvas), false)
  assert.equal(releaseCanvas(null), false)
})

test('marked canvases are found in a removed node and its descendants', () => {
  const a = fakeCanvas().canvas
  const b = fakeCanvas().canvas
  assert.deepEqual(markedCanvases(a), [a])
  assert.deepEqual(markedCanvases(fakeDiv([b])), [b])
  assert.deepEqual(markedCanvases(fakeCanvas({ marked: false }).canvas), [])
  assert.deepEqual(markedCanvases({ nodeType: 3 }), [])
  assert.deepEqual(markedCanvases(null), [])
})

test('releaseRemoved releases each removed canvas once, keeping re-inserted ones', () => {
  const gone = fakeCanvas()
  const moved = fakeCanvas({ connected: true })
  const records = [
    { removedNodes: [gone.canvas, fakeDiv([gone.canvas])] },
    { removedNodes: [moved.canvas] },
    { removedNodes: [] },
  ]
  assert.equal(releaseRemoved(records), 1)
  assert.equal(gone.calls.lose, 1)
  assert.equal(moved.calls.lose, 0)
  assert.equal(releaseRemoved(undefined), 0)
})

test('initWebglRelease observes the subtree and releases on mutations', () => {
  let callback = null
  let observed = null
  class Observer {
    constructor(cb) { callback = cb }
    observe(root, options) { observed = { root, options } }
  }
  const root = {}
  assert.ok(initWebglRelease(root, Observer))
  assert.deepEqual(observed, { root, options: { childList: true, subtree: true } })
  const { canvas, calls } = fakeCanvas()
  callback([{ removedNodes: [canvas] }])
  assert.equal(calls.lose, 1)
})

test('initWebglRelease without a page or MutationObserver does nothing', () => {
  assert.equal(initWebglRelease(null, class {}), null)
  assert.equal(initWebglRelease({}, undefined), null)
})
