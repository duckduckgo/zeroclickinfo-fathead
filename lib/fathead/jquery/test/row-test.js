/* global describe, it */

'use strict'

const assert = require('assert')

describe('row()', () => {
  const row = require('../lib/row')

  it('backslash escapes backslashes', () => {
    assert.strictEqual(
      row({title: 'one\\two\\three'}),
      'one\\\\two\\\\three\t\t\t\t\t\t\t\t\t\t\t\t\n'
    )
  })

  it('backslash escapes newlines', () => {
    assert.strictEqual(
      row({title: 'one\ntwo\nthree'}),
      'one\\ntwo\\nthree\t\t\t\t\t\t\t\t\t\t\t\t\n'
    )
  })

  it('backslash escapes tabs', () => {
    assert.strictEqual(
      row({title: 'one\ttwo\tthree'}),
      'one\\ttwo\\tthree\t\t\t\t\t\t\t\t\t\t\t\t\n'
    )
  })

  it('includes a title', () => {
    assertColumn('title', 0)
  })

  it('includes a type', () => {
    assertColumn('type', 1)
  })

  it('includes a redirect title', () => {
    assertColumn('redirectTitle', 2)
  })

  it('includes categories', () => {
    assertColumn('categories', 4)
  })

  it('includes related', () => {
    assertColumn('related', 6)
  })

  it('includes links', () => {
    assertColumn('links', 8)
  })

  it('includes disambiguation content', () => {
    assertColumn('disambiguationContent', 9)
  })

  it('includes an image', () => {
    assertColumn('image', 10)
  })

  it('includes an abstract', () => {
    assertColumn('abstract', 11)
  })

  it('includes a url', () => {
    assertColumn('url', 12)
  })

  const assertColumn = (key, expected) => {
    const actual =
      row({[key]: key})
        .slice(0, -1) // remove newline
        .split('\t')
        .indexOf(key)

    const msg =
      `expected '${key}' in column ${expected}, was ${actual}`

    assert.strictEqual(actual, expected, msg)
  }
})
