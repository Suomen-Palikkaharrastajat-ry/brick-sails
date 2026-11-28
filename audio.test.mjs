// The Brick Sails synthesizer (audio.js) on a fake Web Audio context: it
// waits for a user gesture, respects mute, and never piles up nodes, however
// many sounds a long voyage fires (mobile browsers crash when nodes leak).

import { test } from 'node:test'
import assert from 'node:assert/strict'

class FakeParam {
  constructor(value = 0) {
    this.value = value
  }
  setValueAtTime(v) {
    this.value = v
  }
  linearRampToValueAtTime(v) {
    this.value = v
  }
  exponentialRampToValueAtTime(v) {
    if (!(v > 0)) throw new RangeError('exponential ramp to a non-positive value')
    this.value = v
  }
  setTargetAtTime(v) {
    this.value = v
  }
  cancelScheduledValues() {}
}

class FakeNode {
  constructor(ctx) {
    this.ctx = ctx
    this.outputs = new Set()
    ctx.created += 1
  }
  connect(dest) {
    if (this.outputs.size === 0) this.ctx.live += 1
    this.outputs.add(dest)
    return dest
  }
  disconnect() {
    if (this.outputs.size > 0) this.ctx.live -= 1
    this.outputs.clear()
  }
}

class FakeSource extends FakeNode {
  constructor(ctx) {
    super(ctx)
    this.startAt = null
    this.stopAt = Infinity
    this.endedFired = false
    this.onended = null
    ctx.sources.push(this)
  }
  start(t = 0) {
    if (this.startAt !== null) throw new Error('started twice')
    this.startAt = t
  }
  stop(t = 0) {
    if (this.startAt === null) throw new Error('stop before start')
    this.stopAt = Math.max(t, this.ctx.currentTime)
  }
}

class FakeContext {
  constructor() {
    this.currentTime = 0
    this.sampleRate = 4000
    this.state = 'running'
    this.created = 0
    this.live = 0
    this.sources = []
    this.destination = new FakeNode(this)
    FakeContext.created += 1
  }
  createGain() {
    const n = new FakeNode(this)
    n.gain = new FakeParam(1)
    return n
  }
  createBiquadFilter() {
    const n = new FakeNode(this)
    n.frequency = new FakeParam(350)
    n.Q = new FakeParam(1)
    n.type = 'lowpass'
    return n
  }
  createDynamicsCompressor() {
    const n = new FakeNode(this)
    for (const k of ['threshold', 'knee', 'ratio', 'attack', 'release']) n[k] = new FakeParam(0)
    return n
  }
  createOscillator() {
    const n = new FakeSource(this)
    n.frequency = new FakeParam(440)
    n.type = 'sine'
    return n
  }
  createBufferSource() {
    const n = new FakeSource(this)
    n.buffer = null
    n.loop = false
    return n
  }
  createBuffer(channels, length, sampleRate) {
    const data = new Float32Array(length)
    return { duration: length / sampleRate, getChannelData: () => data }
  }
  resume() {
    this.state = 'running'
    return Promise.resolve()
  }
  suspend() {
    this.state = 'suspended'
    return Promise.resolve()
  }
  /** Move the clock on, firing `ended` for sources that have stopped. */
  advance(dt) {
    this.currentTime += dt
    const due = []
    const keep = []
    for (const s of this.sources) {
      if (!s.loop && s.startAt !== null && s.stopAt <= this.currentTime) due.push(s)
      else keep.push(s)
    }
    this.sources = keep
    for (const s of due) {
      s.endedFired = true
      s.onended?.()
    }
  }
  /** Nodes still wired to something. */
  liveNodes() {
    return this.live
  }
}
FakeContext.created = 0

globalThis.window = { AudioContext: FakeContext }

const { createAudioEngine, resolveSound, SOUNDS, SFX_LIMITS, MUSIC_LIMITS } = await import('./audio.js')

const sailing = {
  active: true,
  windGain: 0.5,
  windHz: 400,
  washGain: 0.3,
  washHz: 200,
  hullGain: 0.05,
  toneHz: 0,
  toneGain: 0,
}

function unlocked(muted = false) {
  const engine = createAudioEngine(muted)
  engine.unlock()
  return { engine, ctx: engine.ctx }
}

test('nothing is made before the first user gesture', () => {
  const before = FakeContext.created
  const engine = createAudioEngine(false)
  engine.updateContinuous(sailing)
  engine.playSfx({ type: 'cannon' })
  assert.equal(FakeContext.created, before)
  assert.equal(engine.ctx, null)
  engine.unlock()
  assert.equal(FakeContext.created, before + 1)
})

test('events resolve to sounds; the shared Game.Audio names still work', () => {
  assert.equal(resolveSound({ type: 'cannon' }).kind, 'cannon')
  assert.equal(resolveSound('click').kind, 'click')
  assert.equal(resolveSound({ type: 'impact', strength: 1.0 }).kind, 'hitShip')
  assert.equal(resolveSound({ type: 'impact', strength: 0.5 }).kind, 'hullHit')
  assert.equal(resolveSound({ type: 'checkpoint' }).kind, 'coins')
  assert.equal(resolveSound({ type: 'coins', count: 25 }).count, 25)
  assert.equal(resolveSound({ type: 'splash', strength: 9 }).strength, 1.5)
  for (const t of ['bell', 'finish', 'horn', 'splash']) assert.equal(resolveSound({ type: t }).kind, t)
  assert.equal(resolveSound({ type: 'nope' }), null)
  assert.equal(resolveSound(null), null)
})

test('every sound plays and then frees all of its nodes', () => {
  const { engine, ctx } = unlocked()
  const baseline = ctx.liveNodes()
  for (const kind of Object.keys(SOUNDS)) {
    const created = ctx.created
    engine.playSfx({ type: kind, strength: 1, count: 30 })
    assert.ok(ctx.created > created, `${kind} made no nodes`)
    ctx.advance(3)
    assert.equal(ctx.liveNodes(), baseline, `${kind} left nodes connected`)
    assert.equal(engine.sfxPool.size, 0, `${kind} kept its voice`)
  }
})

test('muted, nothing is made; unmuted again, sounds play', () => {
  const { engine, ctx } = unlocked(true)
  const created = ctx.created
  engine.playSfx({ type: 'cannon' })
  engine.updateContinuous(sailing)
  assert.equal(ctx.created, created)
  engine.setMuted(false)
  engine.playSfx({ type: 'cannon' })
  assert.ok(ctx.created > created)
})

test('a long noisy battle keeps the voices and nodes bounded', () => {
  const { engine, ctx } = unlocked()
  const baseline = ctx.liveNodes()
  const kinds = Object.keys(SOUNDS)
  let maxLive = 0
  let maxVoices = 0
  // Ten minutes at 60 frames a second, a burst of sounds most frames.
  for (let frame = 0; frame < 36000; frame++) {
    engine.updateContinuous(sailing)
    const n = frame % 3 === 0 ? 4 : 1
    for (let i = 0; i < n; i++) {
      engine.playSfx({ type: kinds[(frame * 7 + i * 13) % kinds.length], strength: 1, count: 5 })
    }
    ctx.advance(1 / 60)
    maxLive = Math.max(maxLive, ctx.liveNodes())
    maxVoices = Math.max(maxVoices, engine.sfxPool.size)
    assert.ok(engine.musicPool.size <= MUSIC_LIMITS.maxVoices)
  }
  assert.ok(maxVoices <= SFX_LIMITS.maxVoices, `voices ${maxVoices}`)
  assert.ok(maxLive < 300, `live nodes peaked at ${maxLive}`)
  console.log(`peak: ${maxVoices} effect voices, ${maxLive} live nodes`)

  // The voyage ends: everything winds down to the fixed graph.
  engine.updateContinuous({ ...sailing, active: false })
  ctx.advance(5)
  assert.equal(engine.sfxPool.size, 0)
  assert.equal(engine.musicPool.size, 0)
  assert.equal(ctx.liveNodes(), baseline)
})

test('the music plays while sailing and stops when the voyage ends', () => {
  const { engine, ctx } = unlocked()
  const baseline = ctx.liveNodes()
  let notes = 0
  for (let frame = 0; frame < 60 * 30; frame++) {
    const before = ctx.created
    engine.updateContinuous(sailing)
    if (ctx.created > before) notes++
    ctx.advance(1 / 60)
  }
  assert.ok(notes >= 18, `only ${notes} frames scheduled music`)
  engine.updateContinuous({ ...sailing, active: false })
  ctx.advance(3)
  const created = ctx.created
  for (let frame = 0; frame < 120; frame++) {
    engine.updateContinuous({ ...sailing, active: false })
    ctx.advance(1 / 60)
  }
  assert.equal(ctx.created, created, 'music went on after the voyage ended')
  assert.equal(ctx.liveNodes(), baseline)
})

test('muting stops every voice at once', () => {
  const { engine, ctx } = unlocked()
  for (let i = 0; i < 10; i++) engine.playSfx({ type: 'bell' })
  engine.updateContinuous(sailing)
  assert.ok(engine.sfxPool.size > 0)
  engine.setMuted(true)
  assert.equal(engine.sfxPool.size, 0)
  assert.equal(engine.musicPool.size, 0)
  ctx.advance(1)
})

test('every sound Sails.Sound sends has a voice in audio.js', async () => {
  const { readFileSync } = await import('node:fs')
  const elm = readFileSync(new URL('./src/Sails/Sound.elm', import.meta.url), 'utf8')
  const names = [...elm.matchAll(/(?:kind|withStrength|Encode\.string) "([A-Za-z]+)"/g)].map((m) => m[1])
  const types = names.filter((n) => n !== 'type')
  assert.ok(types.length >= 20, `found ${types.length}`)
  for (const t of types) assert.ok(resolveSound({ type: t }), `${t} has no sound`)
})
