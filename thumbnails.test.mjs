// The hull cards of the ship editor show live pictures of each starter hull,
// drawn from its real parts in the catalog (`thumbnails.js`): the model
// pictures have a queue of their own, so they never wait behind the dock's part
// pictures, and every starter hull has a bundled placeholder for the moment
// before its live picture arrives.

import { test } from 'node:test'
import assert from 'node:assert/strict'
import { existsSync, readdirSync } from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { initGameThumbnails } from './thumbnails.js'

const here = path.dirname(fileURLToPath(import.meta.url))

function port() {
  const listeners = []
  return {
    subscribe: (f) => listeners.push(f),
    send: (v) => listeners.forEach((f) => f(v)),
    listeners,
  }
}

test('a hull picture is not held up by part pictures that are still loading', async () => {
  const fetched = []
  const realFetch = globalThis.fetch
  // Part files never answer; the hull's own brick does.
  globalThis.fetch = (url) => {
    fetched.push(String(url))
    if (String(url).includes('hullbrick')) {
      return Promise.resolve(new Response('0 Hull brick\n', { status: 200, headers: { 'content-type': 'text/plain' } }))
    }
    return new Promise(() => {})
  }
  try {
    const ports = {
      requestThumbnails: port(),
      thumbnailLoaded: port(),
      requestModelThumbnails: port(),
      modelThumbnailLoaded: port(),
    }
    await initGameThumbnails(ports, 'https://catalog.test/raw')
    ports.requestThumbnails.send([{ file: 'stuckpart.dat', color: 4, rgb: [1, 0, 0] }])
    ports.requestModelThumbnails.send([
      { key: 'hull:test', text: '1 70 0 0 0 1 0 0 0 1 0 0 0 1 hullbrick.dat', colours: [] },
    ])
    await new Promise((r) => setTimeout(r, 50))
    assert.ok(
      fetched.some((u) => u.includes('hullbrick.dat')),
      `the hull's brick was fetched from the catalog (fetched: ${fetched.join(', ')})`,
    )
  } finally {
    globalThis.fetch = realFetch
  }
})

test('only the ports an app declares are wired', async () => {
  const models = { requestModelThumbnails: port(), modelThumbnailLoaded: port() }
  await initGameThumbnails(models, 'https://catalog.test/raw')
  assert.equal(models.requestModelThumbnails.listeners.length, 1)
  await initGameThumbnails({}, 'https://catalog.test/raw')
})

test('every starter hull has a bundled placeholder picture', () => {
  const hulls = readdirSync(path.join(here, 'starters/sails')).filter((f) => f.endsWith('.mpd'))
  assert.ok(hulls.length > 0)
  for (const file of hulls) {
    const png = path.join(here, 'public/base-previews', file.replace(/\.mpd$/, '.png'))
    assert.ok(existsSync(png), `missing ${path.relative(here, png)}`)
  }
})
