/**
 * Voice bookkeeping for the Brick Sails synthesizer (`audio.js`), kept free
 * of Web Audio so it can be unit-tested (`audio-voices.test.mjs`).
 *
 * Every one-shot sound and every music note is a "voice": a few short-lived
 * Web Audio nodes that end on their own. A long battle fires many sounds a
 * second, so the pool bounds how many may sound at once:
 *
 * - `maxVoices` in all. When full, the quietest-priority, oldest voice is
 *   stolen (faded out and stopped at once) to make room, or the new sound is
 *   dropped when everything playing matters more.
 * - `perKind[kind]` of one kind at once (e.g. three splashes); the oldest of
 *   that kind is stolen first.
 * - `minInterval[kind]` seconds between two starts of one kind: a volley of
 *   splashes in one frame plays once, not ten times over each other.
 *
 * A voice leaves the pool when its nodes report `ended` (`release`), and at
 * the latest once its planned end time has passed (`prune`), so a missed
 * `ended` event can never leak a slot.
 */

export class VoicePool {
  constructor({ maxVoices = 12, perKind = {}, minInterval = {}, grace = 0.25 } = {}) {
    this.maxVoices = maxVoices
    this.perKind = perKind
    this.minInterval = minInterval
    this.grace = grace
    this.voices = []
    this.lastStart = new Map()
    this.nextId = 1
  }

  get size() {
    return this.voices.length
  }

  countOf(kind) {
    let n = 0
    for (const v of this.voices) if (v.kind === kind) n++
    return n
  }

  /** Drop voices whose planned end (plus a grace period) has passed. */
  prune(now) {
    const keep = []
    for (const v of this.voices) {
      if (v.endsAt + this.grace > now) keep.push(v)
      else v.onEvict?.(false)
    }
    this.voices = keep
  }

  /**
   * Ask for a slot. Returns `null` when the sound should not play (too soon
   * after the last of its kind, or the pool is full of more important
   * voices); otherwise a voice id, after stealing whatever had to make room.
   * `onEvict(stolen)` is called when the voice is taken out of the pool by
   * `prune` (`stolen` false) or to make room for another (`stolen` true).
   */
  request(kind, now, { endsAt = now + 1, priority = 1, onEvict } = {}) {
    this.prune(now)

    const gap = this.minInterval[kind]
    const last = this.lastStart.get(kind)
    if (typeof gap === 'number' && last !== undefined && now - last < gap) return null

    const kindCap = this.perKind[kind]
    if (typeof kindCap === 'number') {
      if (kindCap <= 0) return null
      while (this.countOf(kind) >= kindCap) {
        const oldest = this.voices.find((v) => v.kind === kind)
        this.steal(oldest)
      }
    }

    while (this.voices.length >= this.maxVoices) {
      const victim = this.victim(priority)
      if (!victim) return null
      this.steal(victim)
    }

    const id = this.nextId++
    this.voices.push({ id, kind, startedAt: now, endsAt, priority, onEvict })
    this.lastStart.set(kind, now)
    return id
  }

  /** The voice to steal for a new one of `priority`: lowest priority, then oldest. */
  victim(priority) {
    let best = null
    for (const v of this.voices) {
      if (v.priority > priority) continue
      if (!best || v.priority < best.priority || (v.priority === best.priority && v.startedAt < best.startedAt)) {
        best = v
      }
    }
    return best
  }

  steal(voice) {
    if (!voice) return
    this.voices = this.voices.filter((v) => v !== voice)
    voice.onEvict?.(true)
  }

  /** The voice ended on its own: free its slot. */
  release(id) {
    this.voices = this.voices.filter((v) => v.id !== id)
  }

  /** Stop and forget every voice (e.g. on mute). */
  clear() {
    const all = this.voices
    this.voices = []
    for (const v of all) v.onEvict?.(true)
  }
}
