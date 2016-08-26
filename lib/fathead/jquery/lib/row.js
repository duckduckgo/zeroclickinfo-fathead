'use strict'

// Returns values from obj for each key in HEADERS, delimeted by tabs.
//
// Backslashes, newlines and tabs in values are backslash escaped, and missing
// values are left empty. The row is terminated by a newline.
module.exports =
  obj =>
    HEADERS
      .map(key => obj[key] || '')
      .map(escape)
      .join('\t')
      .concat('\n')

const escape =
  text =>
    text
      .replace(/\\/g, '\\\\')
      .replace(/\n/g, '\\n')
      .replace(/\t/g, '\\t')

const HEADERS =
  (ignore =>
    [
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
    ]
  )(Symbol('ignore'))
