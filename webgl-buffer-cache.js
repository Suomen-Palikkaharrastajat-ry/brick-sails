// A buffer cache for elm-explorations/webgl that frees GPU memory.
//
// The package uploads a mesh's vertex and index buffers the first time the
// mesh is drawn and keeps them in a WeakMap keyed by the mesh, for as long as
// the mesh lives. It never calls `deleteBuffer`. The game keeps its meshes
// for the whole voyage (islands, props, every ship design and its wreck
// pieces), so GPU memory only grows as more of the world is seen: on iPhones,
// where the GPU and the page share one memory budget, that grows until iOS
// kills the tab.
//
// This cache stands in for that WeakMap (see `webgl-kernel-patch.mjs`, which
// swaps it in at build time). It has the same `get` / `set` and remembers when
// each mesh was last drawn. A sweep every few seconds deletes the GPU buffers
// of the meshes that have not been drawn for a while. A mesh drawn again later
// is simply uploaded again from the vertex list the mesh still holds, the
// way the package handles any new mesh. The idle clock only counts while
// frames are drawn, so a hidden tab (no frames) keeps everything.
//
// A cache stops sweeping once its canvas has left the page (`webgl-release.js`
// then loses that canvas's context), or if the canvas never joined the page.

/** How often the cache sweeps (ms). */
export const SWEEP_MS = 5000

/** Sweeps with frames drawn but without a mesh in them before it is freed (6 × 5 s = 30 s). */
export const IDLE_SWEEPS = 6

/** Sweeps a canvas may stay off the page before its first showing; the cache then stops. */
export const UNSHOWN_SWEEPS = 12

/** Delete the GPU buffers of one cache entry (`{ indexBuffer, buffers }` from the package). */
export function deleteEntryBuffers(gl, entry) {
  let deleted = 0
  try {
    if (!gl || gl.isContextLost?.()) return 0
    if (entry?.indexBuffer) {
      gl.deleteBuffer(entry.indexBuffer)
      deleted += 1
    }
    for (const name of Object.keys(entry?.buffers ?? {})) {
      const buffer = entry.buffers[name]
      if (buffer) {
        gl.deleteBuffer(buffer)
        deleted += 1
      }
    }
  } catch {
    // A context gone mid-sweep: its buffers are gone with it.
  }
  return deleted
}

/**
 * The cache for one WebGL context. `timers` is for tests. Returns an object
 * with the package's `get` and `set`, plus `sweep`, `dispose` and `stats`.
 */
export function createBufferCache(gl, timers = globalThis, options = {}) {
  const idleSweeps = options.idleSweeps ?? IDLE_SWEEPS
  const unshownSweeps = options.unshownSweeps ?? UNSHOWN_SWEEPS
  // mesh -> { entry, epoch }
  const meshes = new Map()
  let epoch = 0
  let drawnSinceSweep = false
  let shown = false
  let unshownFor = 0
  let timer = null
  let disposed = false
  const totals = { evicted: 0, deletedBuffers: 0 }

  const canvasShown = () => {
    const canvas = gl?.canvas
    // Without `isConnected` (OffscreenCanvas, old browsers) count it as shown.
    return !canvas || canvas.isConnected !== false
  }

  const cache = {
    get(mesh) {
      const slot = meshes.get(mesh)
      if (!slot) return undefined
      slot.epoch = epoch
      drawnSinceSweep = true
      return slot.entry
    },

    set(mesh, entry) {
      meshes.set(mesh, { entry, epoch })
      drawnSinceSweep = true
      return cache
    },

    has(mesh) {
      return meshes.has(mesh)
    },

    /** One sweep: free the meshes left undrawn too long. Returns how many were freed. */
    sweep() {
      if (disposed) return 0
      if (canvasShown()) {
        shown = true
        unshownFor = 0
      } else if (shown) {
        cache.dispose()
        return 0
      } else if (++unshownFor >= unshownSweeps) {
        cache.dispose()
        return 0
      }
      // No frames since the last sweep (a hidden tab, a paused view): the
      // idle clock stands still.
      if (!drawnSinceSweep) return 0
      drawnSinceSweep = false
      epoch += 1
      let freed = 0
      for (const [mesh, slot] of meshes) {
        if (epoch - slot.epoch > idleSweeps) {
          totals.deletedBuffers += deleteEntryBuffers(gl, slot.entry)
          meshes.delete(mesh)
          freed += 1
        }
      }
      totals.evicted += freed
      return freed
    },

    /** Stop sweeping and free every buffer still held. */
    dispose() {
      if (disposed) return
      disposed = true
      if (timer !== null) timers.clearInterval?.(timer)
      timer = null
      for (const slot of meshes.values()) totals.deletedBuffers += deleteEntryBuffers(gl, slot.entry)
      meshes.clear()
    },

    stats() {
      return { meshes: meshes.size, evicted: totals.evicted, deletedBuffers: totals.deletedBuffers, disposed }
    },
  }

  try {
    timer = timers.setInterval?.(() => cache.sweep(), options.sweepMs ?? SWEEP_MS) ?? null
  } catch {
    timer = null
  }
  return cache
}

/** The name the patched package looks the cache factory up by. */
export const GLOBAL_NAME = '__brickWebglBufferCache'

/**
 * Make the factory available to the patched package. Every cache made is
 * kept in `caches` while it is live, for checking from the console:
 * `__brickWebglBufferCache.caches.map((c) => c.stats())`.
 */
export function installBufferCache(target = globalThis) {
  try {
    const caches = []
    const factory = (gl) => {
      const cache = createBufferCache(gl)
      const dispose = cache.dispose
      cache.dispose = () => {
        dispose()
        const at = caches.indexOf(cache)
        if (at >= 0) caches.splice(at, 1)
      }
      caches.push(cache)
      return cache
    }
    factory.caches = caches
    target[GLOBAL_NAME] = factory
    return factory
  } catch {
    return null
  }
}
