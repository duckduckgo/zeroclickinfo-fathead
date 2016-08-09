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

  const cheerio = require('cheerio')

  const url = path => 'https://api.jquery.com/' + basename(path, '.xml') + '/'

  const categories = {
    method: 'jQuery methods',
    property: 'jQuery properties',
    selector: 'jQuery selectors'
  }

  const [, , ...paths] = process.argv

  paths.forEach((path, i) => {
    readFile(path, (err, data) => {
      if (err) throw err

      const $ = cheerio.load(data)

      const title = $('title').text()
      const description = $('entry > desc').text()

      const entryType = $('entry').attr('type')
      const returnType = $('entry').attr('return')

      const signatures = $('signature').get()
        .map(signature => {
          const args = $('argument', signature).get()
            .map(arg => $(arg).attr('name'))

          return title.replace('()', `(${args.join(', ')}) → ${returnType}`)
        }).join('\n')

      const output = outputRow({
        title: title,
        type: 'A', // article
        categories: categories[entryType],
        abstract: `${description}<pre><code>${signatures}</code></pre>`,
        url: url(path)
      })

      process.stdout.write(output)
    })
  })
}
