/**
 * Procedural Web Audio synthesizer for Brick Sails.
 * Extends BaseAudioEngine with wind whistle, hull water lap & wake splash,
 * rigging creak, buoy bells, and cannonball impact / broadside sounds.
 */

import { BaseAudioEngine } from './vendor/packages/brick-frame-game/runtime/audio-engine.js'

export function createAudioEngine(initialMuted = false) {
  const engine = new BaseAudioEngine(initialMuted)

  let windGainNode = null
  let windFilter = null
  let washGainNode = null
  let washFilter = null
  let creakGainNode = null
  let creakOsc = null

  engine.initContinuousLoops = function (ctx, noiseBuffer, masterGain) {
    // 1. Wind bandpass loop
    const windSrc = ctx.createBufferSource()
    windSrc.buffer = noiseBuffer
    windSrc.loop = true
    windFilter = ctx.createBiquadFilter()
    windFilter.type = 'bandpass'
    windFilter.frequency.value = 350
    windFilter.Q.value = 1.2
    windGainNode = ctx.createGain()
    windGainNode.gain.value = 0
    windSrc.connect(windFilter)
    windFilter.connect(windGainNode)
    windGainNode.connect(masterGain)
    windSrc.start()

    // 2. Water wash / wave lap lowpass loop
    const washSrc = ctx.createBufferSource()
    washSrc.buffer = noiseBuffer
    washSrc.loop = true
    washFilter = ctx.createBiquadFilter()
    washFilter.type = 'lowpass'
    washFilter.frequency.value = 180
    washGainNode = ctx.createGain()
    washGainNode.gain.value = 0
    washSrc.connect(washFilter)
    washFilter.connect(washGainNode)
    washGainNode.connect(masterGain)
    washSrc.start()

    // 3. Subtle hull / rigging creak oscillation
    creakOsc = ctx.createOscillator()
    creakOsc.type = 'triangle'
    creakOsc.frequency.value = 55
    creakGainNode = ctx.createGain()
    creakGainNode.gain.value = 0
    creakOsc.connect(creakGainNode)
    creakGainNode.connect(masterGain)
    creakOsc.start()
  }

  engine.updateContinuous = function (telemetry) {
    if (!telemetry || !engine.ensureContext()) return
    const now = engine.ctx.currentTime

    if (!telemetry.active || engine.muted) {
      windGainNode?.gain.setTargetAtTime(0, now, 0.05)
      washGainNode?.gain.setTargetAtTime(0, now, 0.05)
      creakGainNode?.gain.setTargetAtTime(0, now, 0.05)
      return
    }

    const wg = Math.max(0, Math.min(1.0, telemetry.windGain ?? 0))
    const wf = Math.max(100, Math.min(2400, telemetry.windHz ?? 350))
    windGainNode?.gain.setTargetAtTime(wg, now, 0.05)
    windFilter?.frequency.setTargetAtTime(wf, now, 0.05)

    const wg2 = Math.max(0, Math.min(1.0, telemetry.washGain ?? 0))
    const wf2 = Math.max(60, Math.min(900, telemetry.washHz ?? 180))
    washGainNode?.gain.setTargetAtTime(wg2, now, 0.05)
    washFilter?.frequency.setTargetAtTime(wf2, now, 0.05)

    const cg = Math.max(0, Math.min(0.3, telemetry.hullGain ?? 0))
    creakGainNode?.gain.setTargetAtTime(cg, now, 0.05)
  }

  engine.playSfx = function (event) {
    if (engine.muted || !engine.ensureContext()) return
    const now = engine.ctx.currentTime
    const type = typeof event === 'string' ? event : event?.type

    if (type === 'bell') {
      engine.playTone(1180, 1.8, 0, 'sine', 0.35)
      engine.playTone(2360, 1.8, 0, 'sine', 0.25)
    } else if (type === 'splash') {
      const strength = Math.max(0.1, Math.min(1.0, event?.strength ?? 0.6))
      const splashGain = engine.ctx.createGain()
      splashGain.gain.setValueAtTime(0.5 * strength, now)
      splashGain.gain.exponentialRampToValueAtTime(0.001, now + 0.6)
      const splashFilter = engine.ctx.createBiquadFilter()
      splashFilter.type = 'bandpass'
      splashFilter.frequency.setValueAtTime(450, now)
      splashFilter.Q.value = 1.0

      const src = engine.createNoiseBurst(0.7, 0.2)
      if (src) {
        src.connect(splashFilter)
        splashFilter.connect(splashGain)
        splashGain.connect(engine.masterGain)
        src.start(now)
        src.stop(now + 0.7)
      }
    } else if (type === 'impact') {
      const strength = Math.max(0.2, Math.min(1.5, event?.strength ?? 0.8))
      const osc = engine.ctx.createOscillator()
      const gain = engine.ctx.createGain()
      osc.type = 'triangle'
      osc.frequency.setValueAtTime(140, now)
      osc.frequency.exponentialRampToValueAtTime(36, now + 0.35)
      gain.gain.setValueAtTime(0.75 * strength, now)
      gain.gain.exponentialRampToValueAtTime(0.001, now + 0.5)
      osc.connect(gain)
      gain.connect(engine.masterGain)
      osc.start(now)
      osc.stop(now + 0.5)

      const noise = engine.createNoiseBurst(0.25, 0.04)
      if (noise) {
        const noiseGain = engine.ctx.createGain()
        noiseGain.gain.setValueAtTime(0.6 * strength, now)
        noiseGain.gain.exponentialRampToValueAtTime(0.001, now + 0.25)
        const noiseFilter = engine.ctx.createBiquadFilter()
        noiseFilter.type = 'lowpass'
        noiseFilter.frequency.setValueAtTime(800, now)
        noise.connect(noiseFilter)
        noiseFilter.connect(noiseGain)
        noiseGain.connect(engine.masterGain)
        noise.start(now)
        noise.stop(now + 0.25)
      }
    }
  }

  return engine
}
