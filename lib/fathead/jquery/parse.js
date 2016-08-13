#!/usr/bin/env node

'use strict'

const ignore = Symbol('ignore')
const headers = Object.freeze([
  'title',
  'type',
  'redirectTitle',
  ignore,
  'categories',
  ignore,
  'related',
  ignore,
  'links',
  'disambiguationContent',
  'image',
  'abstract',
  'url'
])

function outputRow (fields) {
  return headers.map(header =>
    escape(fields[header] || '')
  ).join('\t') + '\n'
}

function escape (text) {
  return text
    .replace(/\\/g, '\\\\')
    .replace(/\n/g, '\\n')
    .replace(/\t/g, '\\t')
}

if (require.main === module) {
  const {readFile} = require('fs')
  const {basename} = require('path')

  const xpath = require('xpath')
  const {DOMParser} = require('xmldom')

  const url = path => 'https://api.jquery.com/' + basename(path, '.xml') + '/'

  const categories = {
    method: 'jQuery methods',
    property: 'jQuery properties',
    selector: 'jQuery selectors'
  }

  const [, , ...paths] = process.argv

  paths.forEach(path => {
    readFile(path, (err, data) => {
      if (err) throw err

      const doc = new DOMParser().parseFromString(data.toString())

      const entries = xpath.select('//entry', doc)

      const title = xpath.select1('./title', entries[0]).textContent
      const entryType = entries[0].getAttribute('type')

      const descriptions = entries.map(entry => {
        const desc = xpath.select1('./desc', entry)
        const description = desc ? desc.textContent : ''
        const returnType = entry.getAttribute('return')
        const signatures = xpath.select('./signature', entry)
          .map(signature => {
            const args = xpath.select('.//argument', signature)
              .map(arg => arg.getAttribute('name'))

            return title.replace('()', `(${args.join(', ')}) → ${returnType}`)
          })

        return `${description}<pre><code>${signatures.join('\n')}</code></pre>`
      })

      const output = outputRow({
        title: title,
        type: 'A', // article
        categories: categories[entryType],
        abstract: descriptions.join('\n'),
        url: url(path)
      })

      process.stdout.write(output)
    })
  })
}
