#!/usr/bin/env node

'use strict'

const fs = require('fs')
const DOMParser = require('xmldom').DOMParser
const xpath = require('xpath').useNamespaces({
  xi: 'http://www.w3.org/2003/XInclude'
})
const basename = require('path').basename

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

const util = {
  groupBy (property, objects) {
    const map = new Map()

    objects.forEach(object => {
      const key = object[property]

      if (!map.has(key)) {
        map.set(key, [])
      }

      map.get(key).push(object)
    })

    return map
  },
  sortBy (property, objects) {
    return objects.sort((a, b) => {
      if (a[property] > b[property]) {
        return +1
      }
      if (a[property] < b[property]) {
        return -1
      }
      return 0
    })
  }
}

const fmt = {
  removeExample (title) {
    return title
    .replace(/ \[.+\]$/, '') // “Has Attribute Selector [name]” → “has attribute selector”
    .replace(/ \(.+\)$/, '') // “Element Selector ("element")” → “element selector”
  },
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
  },
  removeSpecialCharacters (title) {
    return title
      .toLowerCase()
      .replace(/[^a-z]+/g, ' ') // “:gt() Selector” → “gt selector”
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

function readXMLDoc (path) {
  return new Promise((resolve, reject) => {
    fs.readFile(path, (err, data) => {
      if (err) throw err

      const parser = new DOMParser()
      const xml = data.toString()

      resolve(parser.parseFromString(xml))
    })
  })
}

function sample (entry) {
  const type = entry.getAttribute('type')

  if (type === 'selector') {
    return xpath('./sample | ./signature/sample', entry).map(sample => {
      return `$("${sample.textContent}") → jQuery`
    }).join('\n')
  }

  const name = entry.getAttribute('name')
  const returnType = entry.getAttribute('return') ||
    xpath('./return', entry).map(ret => ret.getAttribute('type')).join('|') ||
    'undefined'
  const prefix = name.includes('.') ? '' : '$(selector).'

  if (type === 'property') {
    return `${prefix}${name} → ${returnType}`
  }

  return xpath('./signature', entry).map(signature => {
    return `${prefix}${name}(${signatureArgs(signature).join(', ')}) → ${returnType}`
  }).join('\n')
}

function signatureArgs (signature) {
  const includes = xpath('./xi:include', signature)

  if (includes.length) {
    return includes.map(include => {
      const file = basename(include.getAttribute('href'))
      return file.replace(/-argument.xml$/, '')
    })
  }

  return xpath('./argument', signature).map(arg => arg.getAttribute('name'))
}

if (require.main === module) {
  const url = path => 'https://api.jquery.com/' + basename(path, '.xml') + '/'

  const CATEGORIES = {
    method: 'jQuery methods',
    property: 'jQuery properties',
    selector: 'jQuery selectors'
  }

  Promise.all(process.argv.slice(2).map(path => {
    return readXMLDoc(path).then(doc => {
      const entries = xpath('//entry', doc)

      const title = fmt.removeExample(xpath('//entry/title', doc)[0].textContent)
      const entryType = entries[0].getAttribute('type')

      const mainCategories = xpath('//category', doc)
        .reduce((set, category) => {
          const mainCategory = category.getAttribute('slug').split('/')[0]
          return set.add(mainCategory)
        }, new Set())

      const descriptions = entries.map(entry => {
        const desc = xpath('./desc', entry)[0]
        const description = desc ? desc.textContent : ''

        return `${description}<pre><code>${sample(entry)}</code></pre>`
      })

      const pages = [{
        title: title,
        type: 'A', // article
        categories: CATEGORIES[entryType],
        abstract: descriptions.join('\n'),
        url: url(path)
      }]

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

      redirects.forEach(redirect => pages.push({
        title: redirect,
        type: 'R', // redirect
        redirectTitle: title
      }))

      return pages
    })
  })).then(pageSets => {
    const pages = Array.prototype.concat.apply([], pageSets).sort()
    const articles = new Map()

    pages.filter(page => page.type === 'A').forEach(article => {
      articles.set(article.title, article)
    })

    util.groupBy('title', pages.filter(page => {
      // Only add redirects which don’t share a name with an existing article.
      return page.type === 'R' && !articles.has(page.title)
    })).forEach((redirects, title) => {
      util.sortBy('redirectTitle', redirects)
      articles.set(title, redirects[0])
    })

    articles.forEach(article => output.writeRow(article))
  }).catch(err => console.error(err.stack))
}
