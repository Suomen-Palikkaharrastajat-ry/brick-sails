import fs from 'node:fs'
import path from 'node:path'
import { defineConfig, loadEnv } from 'vite'
import tailwindcss from '@tailwindcss/vite'
import elmTailwind from 'elm-tailwind-classes/vite'
import elm from 'vite-plugin-elm'
import { elmWebglBufferCache } from './webgl-kernel-patch.mjs'

const candidateRoots = [
  path.resolve(import.meta.dirname, '../brick-catalog/dist'),
  path.resolve(import.meta.dirname, '../brick-catalog'),
  path.resolve(import.meta.dirname, '../brick-studio/viewer-app/public'),
  path.resolve(import.meta.dirname, '../brick-play/public'),
]

const mimeTypes = {
  '.ldr': 'text/plain',
  '.mpd': 'text/plain',
  '.dat': 'text/plain',
  '.json': 'application/json',
  '.ttf': 'font/ttf',
  '.svg': 'image/svg+xml',
  '.png': 'image/png',
  '.ico': 'image/x-icon',
}

function serveViewerAssets() {
  const middleware = (req, res, next) => {
    const url = new URL(req.url ?? '/', 'http://localhost')
    const pathname = decodeURIComponent(url.pathname)

    for (const root of candidateRoots) {
      if (!fs.existsSync(root)) continue
      const file = path.join(root, pathname)
      if (!file.startsWith(root) || !fs.existsSync(file) || !fs.statSync(file).isFile()) {
        continue
      }
      res.setHeader('Content-Type', mimeTypes[path.extname(file)] ?? 'application/octet-stream')
      fs.createReadStream(file).pipe(res)
      return
    }

    next()
  }
  return {
    name: 'serve-viewer-assets',
    configureServer(server) {
      server.middlewares.use(middleware)
    },
    // `make preview` serves the sibling catalog too, so a local catalog build
    // (new parts not yet on the CDN) can be checked against a production build.
    configurePreviewServer(server) {
      server.middlewares.use(middleware)
    },
  }
}

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const configuredBase = String(env.VITE_PUBLIC_BASE ?? './').trim()
  const siteBase = configuredBase.endsWith('/') ? configuredBase : `${configuredBase}/`
  const base = siteBase === './' ? './' : `${siteBase}sails/`

  return {
    cacheDir: '.vite',
    publicDir: 'public',
    // `elmWebglBufferCache` lets elm-explorations/webgl free the GPU
    // buffers of meshes left undrawn (webgl-kernel-patch.mjs).
    plugins: [elmTailwind(), elm({ debug: false }), elmWebglBufferCache(), tailwindcss(), serveViewerAssets()],
    build: {
      outDir: 'build',
      emptyOutDir: true,
    },
    base,
    server: {
      port: 5177,
      fs: { allow: ['..'] },
    },
  }
})
