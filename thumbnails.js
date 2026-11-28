// Part pictures for the ship editor's dock and live pictures of whole hulls and
// saved builds (`Pages.Build`), from the shared thumbnail pipeline and the same
// LDraw catalog the game loads its parts from.
//
// The pipeline renders one picture at a time from one queue per
// `initThumbnails` call. The editor asks for every dock part when it opens, so
// with a single queue the hull pictures waited behind the whole parts palette
// and the hull cards kept their bundled placeholders for many seconds (longer
// on a phone). The model pictures get a queue of their own, so a hull's
// picture is drawn as soon as its own parts have loaded from the catalog.

import { initThumbnails } from './vendor/packages/brick-frame-viewport/runtime/thumbnail-pipeline.js'

/**
 * Wire the part and the model picture ports to two independent pipeline
 * queues (each set only when the app declares both of its ports).
 */
export function initGameThumbnails(ports, ldrawBase) {
  const parts =
    ports?.requestThumbnails && ports?.thumbnailLoaded
      ? { requestThumbnails: ports.requestThumbnails, thumbnailLoaded: ports.thumbnailLoaded }
      : {}
  const models =
    ports?.requestModelThumbnails && ports?.modelThumbnailLoaded
      ? { requestModelThumbnails: ports.requestModelThumbnails, modelThumbnailLoaded: ports.modelThumbnailLoaded }
      : {}
  return Promise.all([initThumbnails(parts, ldrawBase), initThumbnails(models, ldrawBase)]).then(() => undefined)
}
