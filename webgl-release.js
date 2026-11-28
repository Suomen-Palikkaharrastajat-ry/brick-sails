// Release the WebGL context of a game canvas once Elm has dropped it from the
// page. elm-explorations/webgl creates a new <canvas> and context for every
// canvas element it renders and never frees one: a canvas removed from the DOM
// keeps its context (and the GPU memory of every mesh uploaded to it) until the
// garbage collector gets to it, which browsers do not hurry. The shipyard
// preview and the sail view are different canvases, so every voyage would
// leave one or two contexts behind, and browsers cap the live contexts (lower
// on phones) by losing the oldest.
//
// Only canvases marked with `data-webgl-release` (`Sails.Render.canvasWith`)
// are released, and only once they are still detached after the mutation that
// removed them (Html.Keyed moves a node by removing and re-inserting it).
// The `webglcontextlost` event a release fires reaches only the detached
// canvas; `Pages.Sail` ignores it there (see `graphicsEvents`).

export const RELEASE_ATTRIBUTE = 'data-webgl-release'

/** The marked canvases in a removed node: the node itself or its descendants. */
export function markedCanvases(node) {
  const found = []
  if (!node || node.nodeType !== 1) return found
  if (node.tagName === 'CANVAS' && node.hasAttribute?.(RELEASE_ATTRIBUTE)) found.push(node)
  try {
    for (const canvas of node.querySelectorAll?.(`canvas[${RELEASE_ATTRIBUTE}]`) ?? []) found.push(canvas)
  } catch {
    // Not an element we can search: nothing to release.
  }
  return found
}

/**
 * Lose the context of a canvas that is no longer on the page. Returns whether
 * a context was released. A canvas back on the page, one whose context is
 * already lost, or a browser without `WEBGL_lose_context` is left alone.
 */
export function releaseCanvas(canvas) {
  try {
    // Older browsers lack `isConnected`: fall back to a containment test, and
    // when even that is unavailable, treat the canvas as connected (never release).
    const connected = canvas?.isConnected ?? (globalThis.document?.documentElement?.contains(canvas) ?? true)
    if (!canvas || connected) return false
    // Elm's WebGL asked for 'webgl' (or 'experimental-webgl' where that
    // failed); asking again returns that same context. A marked canvas always
    // has one, so this never creates a new context.
    const gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl')
    if (!gl || gl.isContextLost?.()) return false
    const ext = gl.getExtension('WEBGL_lose_context')
    if (!ext) return false
    ext.loseContext()
    return true
  } catch {
    return false
  }
}

/**
 * Handle a batch of MutationObserver records: release every marked canvas
 * removed in it that is still detached once the batch is done. Returns the
 * number released.
 */
export function releaseRemoved(records) {
  const candidates = new Set()
  for (const record of records ?? []) {
    for (const node of record?.removedNodes ?? []) {
      for (const canvas of markedCanvases(node)) candidates.add(canvas)
    }
  }
  let released = 0
  for (const canvas of candidates) {
    if (releaseCanvas(canvas)) released += 1
  }
  return released
}

/** Watch the page for removed game canvases. Returns the observer, or null. */
export function initWebglRelease(root = globalThis.document?.body, Observer = globalThis.MutationObserver) {
  try {
    if (!root || !Observer) return null
    const observer = new Observer((records) => {
      releaseRemoved(records)
    })
    observer.observe(root, { childList: true, subtree: true })
    return observer
  } catch {
    return null
  }
}
