/**
 * Procedural Web Audio synthesizer for Brick Sails.
 * Zero external sound assets: builds wind whistle, hull water lap & wake splash,
 * rigging creak, buoy bells, and foghorn.
 */
export function createAudioEngine(initialMuted = false) {
  let ctx = null
  let masterGain = null
  let windGainNode = null
  let windFilter = null
  let washGainNode = null
  let washFilter = null
  let creakGainNode = null
  let creakOsc = null
  let muted = Boolean(initialMuted)
  let unlocked = false

  function ensureContext() {
    if (typeof window === 'undefined') return null
    const AudioCtx = window.AudioContext || window.webkitAudioContext
    if (!AudioCtx) return null
    if (ctx) {
      if (ctx.state === 'suspended') {
        ctx.resume().catch(() => {})
      }
      return ctx
    }

    ctx = new AudioCtx()

    masterGain = ctx.createGain()
    masterGain.gain.value = muted ? 0 : 0.45
    masterGain.connect(ctx.destination)

    // Shared 2-second pink noise buffer
    const sampleRate = ctx.sampleRate
    const bufferLen = sampleRate * 2
    const noiseBuffer = ctx.createBuffer(1, bufferLen, sampleRate)
    const data = noiseBuffer.getChannelData(0)
    let b0 = 0
    let b1 = 0
    let b2 = 0
    for (let i = 0; i < bufferLen; i++) {
      const white = Math.random() * 2 - 1
      b0 = 0.99765 * b0 + white * 0.099046
      b1 = 0.963 * b1 + white * 0.2965164
      b2 = 0.57 * b2 + white * 1.0526913
      data[i] = Math.max(-1, Math.min(1, (b0 + b1 + b2 + white * 0.1848) * 0.16))
    }

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

    unlocked = true
    return ctx
  }

  function unlock() {
    ensureContext()
  }

  function setMuted(nextMuted) {
    muted = Boolean(nextMuted)
    if (!masterGain || !ctx) return
    const now = ctx.currentTime
    masterGain.gain.cancelScheduledValues(now)
    masterGain.gain.linearRampToValueAtTime(muted ? 0 : 0.45, now + 0.05)
  }

  function updateContinuous(telemetry) {
    if (!telemetry || !ensureContext()) return
    const now = ctx.currentTime

    if (!telemetry.active || muted) {
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

  function playSfx(event) {
    if (muted || !ensureContext()) return
    const now = ctx.currentTime

    const type = typeof event === 'string' ? event : event?.type

    if (type === 'bell') {
      // Navigation buoy bell: high harmonic chime with long decay
      const osc = ctx.createOscillator()
      const osc2 = ctx.createOscillator()
      const gain = ctx.createGain()
      osc.type = 'sine'
      osc.frequency.setValueAtTime(1180, now)
      osc2.type = 'sine'
      osc2.frequency.setValueAtTime(2360, now)
      gain.gain.setValueAtTime(0.35, now)
      gain.gain.exponentialRampToValueAtTime(0.0001, now + 1.8)
      osc.connect(gain)
      osc2.connect(gain)
      gain.connect(masterGain)
      osc.start(now)
      osc2.start(now)
      osc.stop(now + 1.8)
      osc2.stop(now + 1.8)
    } else if (type === 'splash') {
      // Water splash: burst of filtered noise
      const strength = Math.max(0.1, Math.min(1.0, event?.strength ?? 0.6))
      const splashGain = ctx.createGain()
      splashGain.gain.setValueAtTime(0.5 * strength, now)
      splashGain.gain.exponentialRampToValueAtTime(0.001, now + 0.6)
      const splashFilter = ctx.createBiquadFilter()
      splashFilter.type = 'bandpass'
      splashFilter.frequency.setValueAtTime(450, now)
      splashFilter.Q.value = 1.0

      const sampleRate = ctx.sampleRate
      const buffer = ctx.createBuffer(1, sampleRate * 0.7, sampleRate)
      const d = buffer.getChannelData(0)
      for (let i = 0; i < d.length; i++) {
        d[i] = (Math.random() * 2 - 1) * Math.exp(-i / (sampleRate * 0.2))
      }
      const src = ctx.createBufferSource()
      src.buffer = buffer
      src.connect(splashFilter)
      splashFilter.connect(splashGain)
      splashGain.connect(masterGain)
      src.start(now)
      src.stop(now + 0.7)
    } else if (type === 'horn') {
      // Ship foghorn: deep dual tone
      const osc1 = ctx.createOscillator()
      const osc2 = ctx.createOscillator()
      const hGain = ctx.createGain()
      osc1.type = 'sawtooth'
      osc1.frequency.setValueAtTime(140, now)
      osc2.type = 'sine'
      osc2.frequency.setValueAtTime(210, now)
      hGain.gain.setValueAtTime(0.001, now)
      hGain.gain.linearRampToValueAtTime(0.4, now + 0.1)
      hGain.gain.setValueAtTime(0.4, now + 0.8)
      hGain.gain.exponentialRampToValueAtTime(0.001, now + 1.2)
      osc1.connect(hGain)
      osc2.connect(hGain)
      hGain.connect(masterGain)
      osc1.start(now)
      osc2.start(now)
      osc1.stop(now + 1.2)
      osc2.stop(now + 1.2)
    } else if (type === 'checkpoint') {
      const osc = ctx.createOscillator()
      const cGain = ctx.createGain()
      osc.type = 'sine'
      osc.frequency.setValueAtTime(587.33, now) // D5
      osc.frequency.setValueAtTime(880.0, now + 0.08) // A5
      cGain.gain.setValueAtTime(0.3, now)
      cGain.gain.exponentialRampToValueAtTime(0.001, now + 0.4)
      osc.connect(cGain)
      cGain.connect(masterGain)
      osc.start(now)
      osc.stop(now + 0.4)
    } else if (type === 'finish') {
      const osc = ctx.createOscillator()
      const fGain = ctx.createGain()
      osc.type = 'triangle'
      osc.frequency.setValueAtTime(523.25, now) // C5
      osc.frequency.setValueAtTime(659.25, now + 0.1) // E5
      osc.frequency.setValueAtTime(783.99, now + 0.2) // G5
      osc.frequency.setValueAtTime(1046.5, now + 0.3) // C6
      fGain.gain.setValueAtTime(0.35, now)
      fGain.gain.exponentialRampToValueAtTime(0.001, now + 0.9)
      osc.connect(fGain)
      fGain.connect(masterGain)
      osc.start(now)
      osc.stop(now + 0.9)
    }
  }

  return {
    ensureContext,
    unlock,
    setMuted,
    updateContinuous,
    playSfx,
  }
}
