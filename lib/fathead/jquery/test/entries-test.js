/* global describe, it */

'use strict'

const assert = require('assert')
const xml = require('./helpers').xml

describe('entries()', () => {
  const entries = require('../lib/entries')
  const emptyEntry = Object.freeze({
    title: '',
    name: '',
    type: '',
    description: '',
    returns: 'undefined',
    samples: [],
    categories: [],
    argsLists: [],
    prefix: '$(selector).'
  })

  it('extracts an entry from a document with one entry', () => {
    assert.deepStrictEqual(
      entries(xml('<entry/>')),
      [emptyEntry]
    )
  })

  it('extracts entries from a document with multiple entries', () => {
    assert.deepStrictEqual(
      entries(xml('<entries><entry/><entry/></entries>')),
      [emptyEntry, emptyEntry]
    )
  })

  describe('each entry', () => {
    it('has a title', () => {
      assert.deepStrictEqual(
        entries(xml('<entry><title>A Title</title></entry>'))
          .map(entry => entry.title),
        ['A Title']
      )
    })

    it('has a name', () => {
      assert.deepStrictEqual(
        entries(xml('<entry name="an entry"/>'))
          .map(entry => entry.name),
        ['an entry']
      )
    })

    it('has a type', () => {
      assert.deepStrictEqual(
        entries(xml('<entry type="a-type"/>'))
          .map(entry => entry.type),
        ['a-type']
      )
    })

    it('has a description', () => {
      assert.deepStrictEqual(
        entries(xml('<entry><desc>A description.</desc></entry>'))
          .map(entry => entry.description),
        ['A description.']
      )
    })

    it('has a return type from an attribute', () => {
      assert.deepStrictEqual(
        entries(xml('<entry return="return-type"/>'))
          .map(entry => entry.returns),
        ['return-type']
      )
    })

    it('has a union of return types from nodes', () => {
      assert.deepStrictEqual(
        entries(xml('<entry><return type="type-1"/><return type="type-2"/></entry>'))
          .map(entry => entry.returns),
        ['type-1|type-2']
      )
    })

    it('has a return type of undefined when blank', () => {
      assert.deepStrictEqual(
        entries(xml('<entry return=""/>'))
          .map(entry => entry.returns),
        ['undefined']
      )
    })

    it('has a top level sample', () => {
      assert.deepStrictEqual(
        entries(xml('<entry><sample>sample</sample></entry>'))
          .map(entry => entry.samples),
        [['$("sample") → jQuery']]
      )
    })

    it('has signature samples', () => {
      assert.deepStrictEqual(
        entries(xml('<entry><signature><sample>sample 1</sample><sample>sample 2</sample></signature></entry>'))
          .map(entry => entry.samples),
        [['$("sample 1") → jQuery', '$("sample 2") → jQuery']]
      )
    })

    it('lists categories', () => {
      assert.deepStrictEqual(
        entries(xml('<entry><category slug="category/a"/><category slug="category/b"/></entry>'))
          .map(entry => entry.categories),
        [['category/a', 'category/b']]
      )
    })

    it('has one blank argument list for a property', () => {
      assert.deepStrictEqual(
        entries(xml('<entry type="property"/>'))
          .map(entry => entry.argsLists),
        [['']]
      )
    })

    it('has an empty argument list for a method with no arguments', () => {
      assert.deepStrictEqual(
        entries(xml('<entry><signature/></entry>'))
          .map(entry => entry.argsLists),
        [['()']]
      )
    })

    it('lists signature arguments', () => {
      assert.deepStrictEqual(
        entries(xml('<entry><signature><argument name="arg1"/><argument name="arg2"/></signature></entry>'))
          .map(entry => entry.argsLists),
        [['(arg1, arg2)']]
      )
    })

    it('guesses arguments from files included in signatures', () => {
      assert.deepStrictEqual(
        entries(xml('<entry xmlns:xi="http://www.w3.org/2003/XInclude"><signature><xi:include href="path/to/arg1-argument.xml"/><xi:include href="path/to/arg2-argument.xml"/></signature></entry>'))
          .map(entry => entry.argsLists),
        [['(arg1, arg2)']]
      )
    })

    it('allows a mix of inline arguments an includes', () => {
      assert.deepStrictEqual(
        entries(xml('<entry xmlns:xi="http://www.w3.org/2003/XInclude"><signature><xi:include href="path/to/arg1-argument.xml"/><argument name="arg2"/></signature></entry>'))
          .map(entry => entry.argsLists),
        [['(arg1, arg2)']]
      )
    })

    it('does not need a prefix if it already has a namespace', () => {
      assert.deepStrictEqual(
        entries(xml('<entry name="namespace.name"/>'))
          .map(entry => entry.prefix),
        ['']
      )
    })

    it('has a selector example prefix for jQuery methods', () => {
      assert.deepStrictEqual(
        entries(xml('<entry name="name"/>'))
          .map(entry => entry.prefix),
        ['$(selector).']
      )
    })
  })
})
