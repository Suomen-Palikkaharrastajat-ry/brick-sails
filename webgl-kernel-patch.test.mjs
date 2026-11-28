import { test } from 'node:test'
import assert from 'node:assert/strict'
import { patchElmWebgl, hasWebglKernel, elmWebglBufferCache } from './webgl-kernel-patch.mjs'
import { GLOBAL_NAME } from './webgl-buffer-cache.js'

// The lines of elm-explorations/webgl 1.1.3's kernel (`_WebGL_render`) the
// patch works on, as the compiler leaves them (the `__cache` field renamed).
const kernel = `
var _WebGL_drawGL = F2(function (model, domNode) {});
function _WebGL_render(model) {
  var gl = canvas.getContext('webgl');
  if (gl && typeof WeakMap !== 'undefined') {
    model.f.lastProgId = null;
    model.f.buffers = new WeakMap();
    model.f.textures = new WeakMap();
  }
}
`

function run(code, gl, factory) {
  const g = globalThis
  const saved = g[GLOBAL_NAME]
  try {
    if (factory) g[GLOBAL_NAME] = factory
    else delete g[GLOBAL_NAME]
    const model = { f: {} }
    new Function('model', 'gl', code.replace(/^[\s\S]*if \(gl[^\n]*\n/, '').replace(/\n  \}\n\}\n$/, ''))(model, gl)
    return model.f
  } finally {
    if (saved === undefined) delete g[GLOBAL_NAME]
    else g[GLOBAL_NAME] = saved
  }
}

test('only output holding the WebGL kernel is touched', () => {
  assert.equal(hasWebglKernel('var x = new WeakMap();'), false)
  assert.equal(patchElmWebgl('a.buffers = new WeakMap();'), 'a.buffers = new WeakMap();')
  assert.equal(hasWebglKernel(kernel), true)
})

test('the buffer cache comes from the installed factory, for that context', () => {
  const patched = patchElmWebgl(kernel)
  assert.match(patched, /textures = new WeakMap\(\)/)
  const made = { get() {}, set() {} }
  const gl = { name: 'gl' }
  let seen = null
  const cache = run(patched, gl, (g) => {
    seen = g
    return made
  })
  assert.equal(cache.buffers, made)
  assert.equal(seen, gl)
  assert.ok(cache.textures instanceof WeakMap)
})

test('without the factory the package keeps its own WeakMap', () => {
  const cache = run(patchElmWebgl(kernel), {}, null)
  assert.ok(cache.buffers instanceof WeakMap)
})

test('a kernel whose buffer cache moved fails the build', () => {
  assert.throws(() => patchElmWebgl('var _WebGL_drawGL; model.f.buffers = new Map();'), /found 0/)
})

test('the Vite plugin patches compiled Elm modules only', () => {
  const plugin = elmWebglBufferCache()
  assert.equal(plugin.enforce, 'post')
  assert.equal(plugin.transform(kernel, '/x/main.js'), null)
  assert.equal(plugin.transform('elm without webgl', '/x/src/Main.elm'), null)
  assert.match(plugin.transform(kernel, '/x/src/Main.elm').code, new RegExp(GLOBAL_NAME))
})

test('a build that patched nothing fails', () => {
  const plugin = elmWebglBufferCache()
  const errors = []
  plugin.generateBundle.call({ error: (m) => errors.push(m) })
  assert.equal(errors.length, 1)
  plugin.transform(kernel, '/x/src/Main.elm')
  plugin.generateBundle.call({ error: (m) => errors.push(m) })
  assert.equal(errors.length, 1)
})
