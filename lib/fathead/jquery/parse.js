#!/usr/bin/env node

'use strict'

const basename = require('path').basename

const row = require('./lib/row')
const util = require('./lib/util')
const fmt = require('./lib/fmt')
const xmlFile = require('./lib/xml-file')
const entries = require('./lib/entries')

if (require.main === module) {
  const url = path => 'https://api.jquery.com/' + basename(path, '.xml') + '/'

  Promise.all(process.argv.slice(2).map(path =>
    xmlFile
      .read(path)
      .then(entries)
      .then(entries => {
        const title = fmt.removeExample(entries[0].title)

        const categories = Array.prototype.concat
          .apply([], entries.map(entry => entry.categories))
          .filter(category => 
            !category.startsWith('version/') &&
            !category.startsWith('deprecated/') &&
            !category.startsWith('removed'))
          .reduce(
            (set, category) => {
              category.split('/')
                .forEach(c =>
                   set.add(fmt.splitSubwords(c)))
               return set 
            },
            new Set()
          )

        const descriptions = entries.map(entry => {
          const signatures = entry.samples.length
            ? entry.samples
            : entry.argsLists
                .map(args => entry.prefix + entry.name + args)
                .map(lhs => `${lhs} → ${entry.returns}`)

          return `<p>${entry.description}</p>` +
            `<pre><code>${signatures.join('\n')}</code></pre>`
        })

        const pages = [{
          title: title,
          type: 'A', // article
          categories: Array.from(categories)
            .map(category => 
              `jQuery ${fmt.capitalizeFirstLetter(category)}`)
            .join('\n'),
          abstract: `<section class="prog__container">` +
            `${descriptions.join('\n')}</section>`,
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
          // Queries that begin with the keyword “jquery” mess up triggering, so
          // they are removed here.
          [redirect, fmt.splitSubwords(redirect)]
            .map(fmt.removeSpecialCharacters)
            .filter(redirect => !redirect.startsWith('jquery '))
            .forEach(redirect => redirects.add(redirect))
        })

        if (categories.has('events') && title.startsWith('.')) {
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
      }
    )
  )).then(pageSets => {
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

    Array
      .from(articles.values(), row)
      .forEach(row => process.stdout.write(row))
  }).catch(err => console.error(err.stack))
}
