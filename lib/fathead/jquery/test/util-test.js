/* global describe, it */

'use strict'

const assert = require('assert')

describe('util', () => {
  describe('.groupBy()', () => {
    const groupBy = require('../lib/util').groupBy

    it('returns a Map', () => {
      assert.ok(groupBy('', []) instanceof Map)
    })

    it('groups an array of objects by a common property', () => {
      const actual =
        groupBy('length', ['zero', 'one', 'two', 'three', 'four'])

      assert.deepStrictEqual(
        Array.from(actual),
        [[4, ['zero', 'four']], [3, ['one', 'two']], [5, ['three']]]
      )
    })
  })

  describe('.sortBy()', () => {
    const sortBy = require('../lib/util').sortBy

    it('returns its array argument', () => {
      const arg = []

      assert.strictEqual(sortBy('', arg), arg)
    })

    it('sorts an array of objects by comparing a common property', () => {
      const actual =
        sortBy('length', ['zero', 'one', 'two', 'three', 'four'])

      assert.deepStrictEqual(
        actual,
        ['one', 'two', 'zero', 'four', 'three']
      )
    })
  })
})
