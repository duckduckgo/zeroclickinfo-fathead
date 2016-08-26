/* global describe, it, before, after */

'use strict'

const assert = require('assert')
const helpers = require('./helpers')

describe('xml-file', () => {
  before(helpers.silenceXmldomErrors)
  after(helpers.unsilenceXmldomErrors)

  describe('.read()', () => {
    const read = require('../lib/xml-file').read

    it('returns a Promise', () =>
      assert.ok(read() instanceof Promise)
    )

    it('rejects with an Error if the file is missing', () =>
      read(helpers.fixturePath('missing.xml'))
        .then(
          data =>
            Promise.reject(),
          err =>
            assert.ok(err instanceof Error)
        )
    )

    it('rejects with an Error if the file is not XML', () =>
      read(helpers.fixturePath('empty.txt'))
        .then(
          data =>
            Promise.reject(),
          err =>
            assert.ok(err instanceof Error)
        )
    )

    it('fulfills with an XML document', () =>
      read(helpers.fixturePath('example.xml'))
        .then(
          dom =>
            assert.strictEqual(dom.nodeName, '#document')
        )
    )
  })
})
