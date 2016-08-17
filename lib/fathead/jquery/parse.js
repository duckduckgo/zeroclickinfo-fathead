#!/usr/bin/env node

'use strict'

const output = (function () {
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

  return {
    writeRow (fields) {
      process.stdout.write(row(fields).join('\t') + '\n')
    }
  }

  function row (fields) {
    return headers.map(header => escape(fields[header] || ''))
  }

  function escape (text) {
    return text
      .replace(/\\/g, '\\\\')
      .replace(/\n/g, '\\n')
      .replace(/\t/g, '\\t')
  }
})()

const fmt = {
  splitSubwords (title) {
    return title
      .replace(/(mouse)/, '$1 ') // “mouseleave” → “mouse leave”
      .replace(/jQuery/, 'jquery') // don’t split jQuery
      .replace(/([a-z])([A-Z])/g, '$1 $2') // “resolveWith” -> “resolve With”
  },
  dollarize (title) {
    return title
      .replace(/^jQuery./, '$.')
  },
  removeContext (title) {
    return title
      .replace(/^\w+\./, '.') // “jQuery.noop()” → “.noop()”
      .replace(/ \[.+\]$/, '') // “Has Attribute Selector [name]” → “has attribute selector”
      .replace(/ \(.+\)$/, '') // “Element Selector ("element")” → “element selector”
  },
  removeSpecialCharacters (title) {
    return title
      .toLowerCase()
      .replace(/[^a-z]+/g, ' ') // “:gt() Selector” → “gt selector”``
      .trim()
  },
  removeTrailingParens (title) {
    return title
      .replace(/\(\)$/, '')
  },
  removeLeadingDot (title) {
    return title
      .replace(/^\./, '')
  }
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

      output.writeRow({
        title: title,
        type: 'A', // article
        categories: CATEGORIES[entryType],
        abstract: descriptions.join('\n'),
        url: url(path)
      })

      const redirects = new Set([
        title,
        fmt.removeContext(title),
        fmt.dollarize(title)
      ])

      Array.from(redirects).forEach(redirect => {
        redirects.add(fmt.removeLeadingDot(redirect))
      })

      Array.from(redirects).forEach(redirect => {
        redirects.add(fmt.removeTrailingParens(redirect))
      })

      Array.from(redirects).forEach(redirect => {
        redirects.add(fmt.removeSpecialCharacters(redirect))
        redirects.add(fmt.removeSpecialCharacters(fmt.splitSubwords(redirect)))
      })

      if (mainCategories.has('events') && title.startsWith('.')) {
        Array.from(redirects).forEach(redirect => {
          redirects.add('on ' + redirect)
          redirects.add(redirect + ' event')
        })
      }

      redirects.delete(title)

      redirects.forEach(redirect => output.writeRow({
        title: redirect,
        type: 'R', // redirect
        redirectTitle: title
      }))
    })
  })
}
