// Render each starter hull as one picture, from its real LDraw parts, for the
// hull cards of the ship editor (`Pages.Build`). Ported from Brick Racer's
// script of the same name, which draws its car bases the same way.
//
//   node starters/tools/render_base_previews.mjs [--renderer <thumbnail-pipeline.js>]
//                                                [--ldraw <base url>] [--out <dir>]
//
// The parts are flattened with the shared thumbnail pipeline's `flattenPart`
// (so the part resolution matches the palette thumbnails), placed with the
// hull's own colours from LDConfig, and drawn once with WebGL in headless
// Chromium. Requires the `playwright` package (pre-installed in the devenv).

import { readFile, readdir, writeFile, mkdir } from 'node:fs/promises'
import { createRequire } from 'node:module'
import path from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'

const require = createRequire(import.meta.url)
const { chromium } = require('playwright')

const here = path.dirname(fileURLToPath(import.meta.url))
const repo = path.resolve(here, '..', '..')

const args = process.argv.slice(2)
const argOf = (name, fallback) => {
  const i = args.indexOf(name)
  return i >= 0 ? args[i + 1] : fallback
}
const rendererPath = path.resolve(
  argOf('--renderer', path.join(repo, 'vendor/brick-studio/packages/brick-frame-viewport/runtime/thumbnail-pipeline.js')),
)
const ldrawBase = argOf('--ldraw', 'https://catalog.palikkaharrastajat.fi/raw').replace(/\/+$/, '')
const outDir = path.resolve(argOf('--out', path.join(repo, 'public/base-previews')))
const startersDir = path.join(repo, 'starters/sails')

const SIZE = 384
// The hull drawer's background (rgba(5, 19, 29) in `Pages.Build.hullsView`).
const CARD_BACKGROUND = [0x05 / 255, 0x13 / 255, 0x1d / 255]
// Same camera as the palette thumbnails: azimuth and elevation in render space.
const AZIMUTH = 0.75
const ELEVATION = 0.6

const { flattenPart, candidateRelativePaths } = await import(pathToFileURL(rendererPath).href)

// ── Colours from LDConfig ────────────────────────────────────────────────────

async function loadColours() {
  const text = await (await fetch(`${ldrawBase}/LDConfig.ldr`)).text()
  const colours = new Map()
  for (const line of text.split(/\r?\n/)) {
    const m = line.match(/^0\s+!COLOUR\s+.*?\bCODE\s+(\d+)\s+VALUE\s+#([0-9a-fA-F]{6})/)
    if (m) {
      const hex = m[2]
      colours.set(Number(m[1]), [0, 2, 4].map((i) => parseInt(hex.slice(i, i + 2), 16) / 255))
    }
  }
  return colours
}

// ── Parts ────────────────────────────────────────────────────────────────────

function parseMpd(text) {
  const sections = new Map()
  let current = null
  for (const raw of text.split(/\r?\n/)) {
    const line = raw.trim()
    const m = line.match(/^0\s+FILE\s+(.+)$/i)
    if (m) {
      current = m[1].trim().toLowerCase()
      sections.set(current, [])
    } else if (line.startsWith('0 NOFILE')) {
      current = null
    } else if (current) {
      sections.get(current).push(raw)
    }
  }
  return sections
}

const partCache = new Map()

async function fetchPart(file) {
  const normalized = file.toLowerCase().replace(/\\/g, '/')
  if (partCache.has(normalized)) return partCache.get(normalized)
  let text = null
  for (const rel of candidateRelativePaths(normalized)) {
    const resp = await fetch(`${ldrawBase}/${rel}`).catch(() => null)
    if (resp && resp.ok) {
      const body = await resp.text()
      if (!body.trimStart().startsWith('<')) {
        text = body
        break
      }
    }
  }
  partCache.set(normalized, text)
  return text
}

// ── Geometry in render space (Y up), projected to clip space ─────────────────

const norm = (v) => {
  const l = Math.hypot(...v)
  return v.map((c) => c / l)
}
const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]

function cameraBasis() {
  const back = [
    Math.cos(ELEVATION) * Math.sin(AZIMUTH),
    Math.sin(ELEVATION),
    Math.cos(ELEVATION) * Math.cos(AZIMUTH),
  ]
  const right = norm(cross([0, 1, 0], back))
  const up = cross(back, right)
  return { right, up, back }
}

const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
const toRender = ([x, y, z]) => [x, -y, z]

function buildScene(geometry, colours) {
  const { right, up, back } = cameraBasis()
  const colourOf = (code) => {
    if (code === 24) return [0.13, 0.13, 0.13]
    return colours.get(code) ?? colours.get(7)
  }
  // Nudge edges towards the eye so they win the depth test against their faces.
  const nudge = (p) => [p[0] + back[0] * 0.3, p[1] + back[1] * 0.3, p[2] + back[2] * 0.3]

  const project = (p) => [dot(p, right), dot(p, up), dot(p, back)]
  const tris = geometry.triangles.map((t) => ({
    pts: t.v.map((v) => project(toRender(v))),
    color: colourOf(t.color),
  }))
  const lines = []
  for (let i = 0; i < geometry.lines.length; i += 2) {
    lines.push(project(nudge(toRender(geometry.lines[i]))), project(nudge(toRender(geometry.lines[i + 1]))))
  }

  // Fit the whole build into the square with a little margin.
  const all = [...tris.flatMap((t) => t.pts), ...lines]
  // A loop, not Math.min(...points): a hull has too many points to spread.
  const min = [Infinity, Infinity, Infinity]
  const max = [-Infinity, -Infinity, -Infinity]
  for (const p of all) {
    for (let k = 0; k < 3; k++) {
      if (p[k] < min[k]) min[k] = p[k]
      if (p[k] > max[k]) max[k] = p[k]
    }
  }
  const span = Math.max(max[0] - min[0], max[1] - min[1], 1) * 1.08
  const cx = (min[0] + max[0]) / 2
  const cy = (min[1] + max[1]) / 2
  const depth = Math.max(max[2] - min[2], 1)
  const toNdc = ([x, y, z]) => [
    ((x - cx) * 2) / span,
    ((y - cy) * 2) / span,
    (-(z - (min[2] + max[2]) / 2) * 2) / (depth * 1.2),
  ]

  const light = norm([0.35, 0.85, 0.4])
  const triData = []
  for (const t of tris) {
    const [a, b, c] = t.pts
    const e1 = [b[0] - a[0], b[1] - a[1], b[2] - a[2]]
    const e2 = [c[0] - a[0], c[1] - a[1], c[2] - a[2]]
    const n = norm([e1[1] * e2[2] - e1[2] * e2[1], e1[2] * e2[0] - e1[0] * e2[2], e1[0] * e2[1] - e1[1] * e2[0]])
    const shade = 0.55 + 0.45 * Math.abs(dot(n, light))
    for (const p of t.pts) {
      const q = toNdc(p)
      triData.push(q[0], q[1], q[2], t.color[0] * shade, t.color[1] * shade, t.color[2] * shade)
    }
  }
  const lineData = []
  for (const p of lines) {
    const q = toNdc(p)
    lineData.push(q[0], q[1], q[2], 0.13, 0.13, 0.13)
  }
  return { triangles: triData, lines: lineData }
}

// ── WebGL drawing in headless Chromium ───────────────────────────────────────

function drawInPage({ size, background, triangles, lines }) {
  const canvas = document.createElement('canvas')
  canvas.width = size
  canvas.height = size
  const gl = canvas.getContext('webgl', { antialias: true, alpha: false })
  const vs = `attribute vec3 p; attribute vec3 c; varying vec3 vc;
    void main(){ vc = c; gl_Position = vec4(p, 1.0); }`
  const fs = `precision mediump float; varying vec3 vc; void main(){ gl_FragColor = vec4(vc, 1.0); }`
  const compile = (type, src) => {
    const s = gl.createShader(type)
    gl.shaderSource(s, src)
    gl.compileShader(s)
    return s
  }
  const prog = gl.createProgram()
  gl.attachShader(prog, compile(gl.VERTEX_SHADER, vs))
  gl.attachShader(prog, compile(gl.FRAGMENT_SHADER, fs))
  gl.linkProgram(prog)
  gl.useProgram(prog)
  const buf = gl.createBuffer()
  gl.bindBuffer(gl.ARRAY_BUFFER, buf)
  const all = new Float32Array(triangles.length + lines.length)
  all.set(triangles, 0)
  all.set(lines, triangles.length)
  gl.bufferData(gl.ARRAY_BUFFER, all, gl.STATIC_DRAW)
  const pLoc = gl.getAttribLocation(prog, 'p')
  const cLoc = gl.getAttribLocation(prog, 'c')
  gl.enableVertexAttribArray(pLoc)
  gl.vertexAttribPointer(pLoc, 3, gl.FLOAT, false, 24, 0)
  gl.enableVertexAttribArray(cLoc)
  gl.vertexAttribPointer(cLoc, 3, gl.FLOAT, false, 24, 12)
  gl.viewport(0, 0, size, size)
  gl.clearColor(background[0], background[1], background[2], 1)
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)
  gl.enable(gl.DEPTH_TEST)
  gl.depthFunc(gl.LEQUAL)
  gl.enable(gl.POLYGON_OFFSET_FILL)
  gl.polygonOffset(1, 1)
  gl.drawArrays(gl.TRIANGLES, 0, triangles.length / 6)
  gl.disable(gl.POLYGON_OFFSET_FILL)
  gl.drawArrays(gl.LINES, triangles.length / 6, lines.length / 6)
  return canvas.toDataURL('image/png')
}

// ── Main ─────────────────────────────────────────────────────────────────────

const colours = await loadColours()
await mkdir(outDir, { recursive: true })

const files = (await readdir(startersDir)).filter((f) => f.endsWith('.mpd')).sort()
const browser = await chromium.launch({ args: ['--use-gl=swiftshader', '--enable-unsafe-swiftshader', '--ignore-gpu-blocklist'] })
const page = await browser.newPage()

for (const file of files) {
  const id = path.basename(file, '.mpd')
  const sections = parseMpd(await readFile(path.join(startersDir, file), 'utf8'))
  const main = sections.get(file.toLowerCase()) ?? [...sections.values()][0] ?? []
  // The main model holds one reference to the hull submodel (a `1` line ending in .ldr).
  const ref = main
    .map((l) => l.trim().split(/\s+/))
    .find((t) => t[0] === '1' && t.length >= 15 && t[t.length - 1].toLowerCase().endsWith('.ldr'))
  if (!ref) throw new Error(`${file}: no hull submodel reference in the main model`)
  const submodel = ref[ref.length - 1].toLowerCase()

  const load = async (name) => {
    const key = name.toLowerCase()
    if (sections.has(key)) return sections.get(key).join('\n')
    return fetchPart(name)
  }
  const geometry = await flattenPart(submodel, load)
  if (geometry.triangles.length === 0) throw new Error(`${file}: no triangles`)

  const scene = buildScene(geometry, colours)
  const dataUrl = await page.evaluate(drawInPage, {
    size: SIZE,
    background: CARD_BACKGROUND,
    triangles: scene.triangles,
    lines: scene.lines,
  })
  const png = Buffer.from(dataUrl.replace(/^data:image\/png;base64,/, ''), 'base64')
  await writeFile(path.join(outDir, `${id}.png`), png)
  console.log(`${id}: ${geometry.triangles.length} triangles -> ${path.join(outDir, id + '.png')}`)
}

await browser.close()
