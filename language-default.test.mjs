// The interface is Finnish unless a ?lang=en link or a saved choice says
// English. Brick Sails reads the language with brick-studio's shared
// language.js (through bootGame) and must not override it.

import { test, beforeEach } from 'node:test'
import assert from 'node:assert/strict'
import { readFileSync } from 'node:fs'
import { readLanguage, LANG_KEY } from './vendor/packages/brick-frame-game/runtime/language.js'

let stored

function at(url) {
  const u = new URL(url)
  globalThis.window = {
    location: { search: u.search, hash: u.hash },
    localStorage: {
      getItem: (key) => (key in stored ? stored[key] : null),
      setItem: (key, value) => {
        stored[key] = String(value)
      },
    },
  }
}

beforeEach(() => {
  stored = {}
})

test('Finnish is the default when no language is given', () => {
  at('https://ohoi.example/')
  assert.equal(readLanguage(), 'fi')
})

test('a lang=en link gives English', () => {
  at('https://ohoi.example/?lang=en')
  assert.equal(readLanguage(), 'en')
  at('https://ohoi.example/#lang=en')
  assert.equal(readLanguage(), 'en')
})

test('a saved choice is used when the link names no language', () => {
  stored[LANG_KEY] = 'en'
  at('https://ohoi.example/')
  assert.equal(readLanguage(), 'en')
  at('https://ohoi.example/?lang=fi')
  assert.equal(readLanguage(), 'fi')
})

test('main.js leaves the language to bootGame and Elm reads the flag', () => {
  const main = readFileSync(new URL('./main.js', import.meta.url), 'utf8')
  assert.match(main, /bootGame\(\{/)
  assert.doesNotMatch(main, /language:/)
  const elmMain = readFileSync(new URL('./src/Main.elm', import.meta.url), 'utf8')
  assert.match(elmMain, /language = SailsI18n\.fromFlag flags\.language/)
})
