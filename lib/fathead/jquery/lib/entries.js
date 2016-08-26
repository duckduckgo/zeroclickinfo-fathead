'use strict'

const path = require('path')
const xpath = require('xpath').useNamespaces({
  xi: 'http://www.w3.org/2003/XInclude'
})

// Extract and parse <entry> nodes from an XML document.
module.exports =
  xml =>
    xpath('entry | entries/entry', xml)
      .map(entry)

const entry =
  node => {
    const selectText =
      path =>
        xpath(path, node)
          .map(node => node.textContent)

    const selectValues =
      path =>
        xpath(path, node)
          .map(node => node.value)

    return {
      title:
        String(selectText('title')),

      name:
        String(selectValues('@name')),

      type:
        String(selectValues('@type')),

      description:
        String(selectText('desc')),

      returns:
        selectValues('@return | return/@type')
          .join('|') ||
          'undefined',

      samples:
        selectText('sample | signature/sample')
          .map(sample => `$("${sample}") → jQuery`),

      categories:
        selectValues('category/@slug'),

      argsLists:
        String(selectValues('@type')) === 'property'
        ? ['']
        : xpath('signature', node)
            .map(signatureArguments)
            .map(args => '(' + args.join(', ') + ')'),

      prefix:
        String(selectValues('@name')).includes('.')
        ? ''
        : '$(selector).'
    }
  }

const signatureArguments =
  node =>
    xpath('argument/@name | xi:include/@href', node)
      .map(maybePath => path.basename(maybePath.value, '-argument.xml'))
