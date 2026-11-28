// A sessionStorage flag set while a voyage is on (`Ports.voyageInProgress`)
// and cleared when it ends normally (finished, wrecked or left). A page load
// that still finds it set was not a normal exit: most likely the browser
// killed the tab mid-voyage (on phones, usually for memory). That is only
// logged, once, for crash reports; the player sees nothing new.
//
// Storage may be missing or blocked (private mode, sandboxed frames), so
// every access is guarded: the flag is a diagnostic, never a reason to fail.

export const VOYAGE_FLAG_KEY = 'brickSailsVoyageInProgress'

function defaultStorage() {
  try {
    return globalThis.sessionStorage ?? null
  } catch {
    return null
  }
}

/** Set (`true`) or clear (`false`) the flag. Returns whether it was stored. */
export function markVoyage(inProgress, storage = defaultStorage()) {
  try {
    if (!storage) return false
    if (inProgress) {
      storage.setItem(VOYAGE_FLAG_KEY, String(Date.now()))
    } else {
      storage.removeItem(VOYAGE_FLAG_KEY)
    }
    return true
  } catch {
    return false
  }
}

/**
 * On load: read a flag left by the previous page in this tab, clear it, and
 * log it once. Returns the leftover value (the voyage's start time, ms since
 * the epoch, as text) or `null` when there was none.
 */
export function checkLeftoverVoyage(storage = defaultStorage(), log = console.warn) {
  let leftover = null
  try {
    leftover = storage ? storage.getItem(VOYAGE_FLAG_KEY) : null
  } catch {
    return null
  }
  if (leftover === null) return null
  try {
    storage.removeItem(VOYAGE_FLAG_KEY)
  } catch {
    // Not cleared: the next load would log it again, which is harmless.
  }
  try {
    const started = Number(leftover)
    const when = Number.isFinite(started) && started > 0 ? new Date(started).toISOString() : 'unknown time'
    log(`[brick-sails] The previous page ended mid-voyage (started ${when}): the tab was likely killed by the browser.`)
  } catch {
    // Logging must not break the boot.
  }
  return leftover
}

/**
 * Wire the flag to the Elm port and check for a leftover one. A normal reload
 * or navigation away (`pagehide`) clears the flag, so only a tab that was
 * killed without that event is logged as a crash on the next load. A page
 * restored from the back/forward cache (`pageshow` with `persisted`) gets its
 * flag back.
 */
export function initVoyageFlag(ports, storage = defaultStorage(), log = console.warn, win = globalThis) {
  checkLeftoverVoyage(storage, log)
  let active = false
  ports?.voyageInProgress?.subscribe((inProgress) => {
    active = Boolean(inProgress)
    markVoyage(active, storage)
  })
  try {
    win?.addEventListener?.('pagehide', () => {
      if (active) markVoyage(false, storage)
    })
    win?.addEventListener?.('pageshow', (event) => {
      if (event?.persisted && active) markVoyage(true, storage)
    })
  } catch {
    // Without these hooks the flag may over-report; it is only a diagnostic.
  }
}
