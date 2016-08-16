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

function splitSubwords (title) {
  return title
    .replace(/(mouse)/, '$1 ') // “mouseleave” → “mouse leave”
    .replace(/([a-z])([A-Z])/g, '$1 $2') // “resolveWith” -> “resolve With”
}

function removeContext (title) {
  return title
    .replace(/^\w+\./, '') // “jQuery.noop()” → “.noop()”
    .replace(/ \[.+\]$/, '') // “Has Attribute Selector [name]” → “has attribute selector”
    .replace(/ \(.+\)$/, '') // “Element Selector ("element")” → “element selector”
}

function removeSpecialCharacters (title) {
  return title
    .toLowerCase()
    .replace(/[^a-z]+/g, ' ') // “:gt() Selector” → “gt selector”``
    .trim()
}

function cleanTitle (title) {
  return [removeContext, splitSubwords, removeSpecialCharacters]
    .reduce((s, fn) => fn(s), title)
}

if (require.main === module) {
  const readFile = require('fs').readFile
  const basename = require('path').basename

  const xpath = require('xpath')
  const DOMParser = require('xmldom').DOMParser

  const url = path => 'https://api.jquery.com/' + basename(path, '.xml') + '/'

  const CATEGORIES = {
    method: 'jQuery methods',
    property: 'jQuery properties',
    selector: 'jQuery selectors'
  }

  const paths = process.argv.slice(2)

  paths.forEach(path => {
    readFile(path, (err, data) => {
      if (err) throw err

      const doc = new DOMParser().parseFromString(data.toString())

      const entries = xpath.select('//entry', doc)

      const title = xpath.select1('./title', entries[0]).textContent
      const entryType = entries[0].getAttribute('type')

      const mainCategories = xpath.select('//category', doc)
        .reduce((set, category) => {
          const mainCategory = category.getAttribute('slug').split('/')[0]
          return set.add(mainCategory)
        }, new Set())

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

      process.stdout.write(outputRow({
        title: title,
        type: 'A', // article
        categories: CATEGORIES[entryType],
        abstract: descriptions.join('\n'),
        url: url(path)
      }))

      const redirectTitle = cleanTitle(title)

      if (redirectTitle !== title) {
        process.stdout.write(outputRow({
          title: redirectTitle,
          type: 'R', // redirect
          redirectTitle: title
        }))
      }
      if (mainCategories.has('events') && title.startsWith('.')) {
        const event = title.replace('.', '').replace('()', '')
        const titles = ['on ' + event, event + ' event']

        if (redirectTitle !== event) {
          titles.push('on ' + redirectTitle, redirectTitle + ' event')
        }

        titles.forEach(redirectTitle => process.stdout.write(outputRow({
          title: redirectTitle,
          type: 'R', // redirect
          redirectTitle: title
        })))
      }
    })
  })
}
