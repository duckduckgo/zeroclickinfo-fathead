/* global describe, it */

'use strict'

const assert = require('assert')

describe('fmt', () => {
  describe('.removeExample()', () => {
    const removeExample = require('../lib/fmt').removeExample

    it('removes an example selector in brackets', () => {
      assert.strictEqual(
        removeExample('Has Attribute Selector [name]'),
        'Has Attribute Selector'
      )
    })

    it('removes an example in parens', () => {
      assert.strictEqual(
        removeExample('Element Selector ("element")'),
        'Element Selector'
      )
    })

    it('leaves function parens untouched', () => {
      assert.strictEqual(
        removeExample('jQuery.ajax()'),
        'jQuery.ajax()'
      )
    })
  })

  describe('.splitSubwords()', () => {
    const splitSubwords = require('../lib/fmt').splitSubwords

    it('splits “mouse” at the beginning of an event name', () => {
      assert.strictEqual(
        splitSubwords('mouseleave'),
        'mouse leave'
      )
    })

    it('splits camelCase function names', () => {
      assert.strictEqual(
        splitSubwords('resolveWith'),
        'resolve With'
      )
    })

    it('replaces “jQuery” with “jquery”', () => {
      assert.strictEqual(
        splitSubwords('jQuery'),
        'jquery'
      )
    })
  })

  describe('.dollarize()', () => {
    const dollarize = require('../lib/fmt').dollarize

    it('replaces “jQuery.” with “$.”', () => {
      assert.strictEqual(
        dollarize('jQuery.ajax()'),
        '$.ajax()'
      )
    })
  })

  describe('.removeContenxt()', () => {
    const removeContext = require('../lib/fmt').removeContext

    it('removes a leading namespace', () => {
      assert.strictEqual(
        removeContext('jQuery.ajax()'),
        '.ajax()'
      )
    })
  })

  describe('.removeSpecialCharacters()', () => {
    const removeSpecialCharacters = require('../lib/fmt').removeSpecialCharacters

    it('lowercases', () => {
      assert.strictEqual(
        removeSpecialCharacters('jQuery'),
        'jquery'
      )
    })

    it('replaces non-alpha characters with spaces', () => {
      assert.strictEqual(
        removeSpecialCharacters(':gt() Selector'),
        'gt selector'
      )
    })
  })

  describe('.removeTrailingParens()', () => {
    const removeTrailingParens = require('../lib/fmt').removeTrailingParens

    it('removes a trailing empty pair of parens', () => {
      assert.strictEqual(
        removeTrailingParens('jQuery.ajax()'),
        'jQuery.ajax'
      )
    })
  })

  describe('.removeLeadingDot()', () => {
    const removeLeadingDot = require('../lib/fmt').removeLeadingDot

    it('removes a period at the start of a method name', () => {
      assert.strictEqual(
        removeLeadingDot('.ajax()'),
        'ajax()'
      )
    })

    it('does not remove a period from within a name', () => {
      assert.strictEqual(
        removeLeadingDot('jQuery.ajax()'),
        'jQuery.ajax()'
      )
    })
  })
})
