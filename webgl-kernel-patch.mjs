// Build-time patch of elm-explorations/webgl's compiled kernel: its buffer
// cache, `new WeakMap()`, becomes the cache from `webgl-buffer-cache.js`,
// which frees the GPU buffers of meshes that stay undrawn. Where that cache is
// not installed (`installBufferCache` in main.js) the package keeps its own
// WeakMap, so a page without it works as before.
//
// The package's kernel sets the cache up once per canvas, as
// `model.__cache.buffers = new WeakMap();` (the field name of `__cache`
// differs between debug and optimized builds; `buffers` and `gl` do not).

import { GLOBAL_NAME } from './webgl-buffer-cache.js'

const BUFFERS_WEAKMAP = /(\.buffers\s*=\s*)new WeakMap\(\)/g

/** Whether compiled Elm output holds elm-explorations/webgl's kernel. */
export function hasWebglKernel(code) {
  return typeof code === 'string' && code.includes('_WebGL_drawGL')
}

/**
 * Patch compiled Elm output. Output without the WebGL kernel is returned as
 * it is; output with the kernel but not exactly one buffer cache throws, so a
 * package update that moves it fails the build instead of quietly leaking.
 */
export function patchElmWebgl(code) {
  if (!hasWebglKernel(code)) return code
  const found = code.match(BUFFERS_WEAKMAP)?.length ?? 0
  if (found !== 1) {
    throw new Error(
      `webgl-kernel-patch: expected one "buffers = new WeakMap()" in elm-explorations/webgl, found ${found}`,
    )
  }
  return code.replace(
    BUFFERS_WEAKMAP,
    `$1(typeof globalThis !== 'undefined' && typeof globalThis.${GLOBAL_NAME} === 'function' ? globalThis.${GLOBAL_NAME}(gl) : new WeakMap())`,
  )
}

/**
 * The Vite plugin: runs after vite-plugin-elm has compiled an Elm module. A
 * production build that patched no module fails (the game always draws with
 * WebGL, so the kernel is there; not finding it means the patch missed it).
 */
export function elmWebglBufferCache() {
  let patched = 0
  return {
    name: 'elm-webgl-buffer-cache',
    enforce: 'post',
    transform(code, id) {
      if (!/\.elm(\?|$)/.test(id) || !hasWebglKernel(code)) return null
      patched += 1
      return { code: patchElmWebgl(code), map: null }
    },
    generateBundle() {
      if (patched === 0) {
        this.error('webgl-kernel-patch: no compiled Elm module with elm-explorations/webgl was patched')
      }
    },
  }
}
