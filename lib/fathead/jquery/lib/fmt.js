'use strict'

// Removes a trailing example usage.
exports.removeExample =
  title =>
    title
    .replace(/ \[.+\]$/, '')
    .replace(/ \(.+\)$/, '')

// Splits at likely word breaks.
exports.splitSubwords =
  title =>
    title
      .replace(/(mouse)/, '$1 ')
      .replace(/jQuery/, 'jquery')
      .replace(/([a-z])([A-Z])/g, '$1 $2')

// Replaces “jQuery” with its alias “$”.
exports.dollarize =
  title =>
    title
      .replace(/^jQuery./, '$.')

// Removes a leading namespace.
exports.removeContext =
  title =>
    title
      .replace(/^\w+\./, '.')

// Replaces all non-alpha characters with a space.
exports.removeSpecialCharacters =
  title =>
    title
      .toLowerCase()
      .replace(/[^a-z]+/g, ' ')
      .trim()

// Removes a trailing empty pair of parens.
exports.removeTrailingParens =
  title =>
    title
      .replace(/\(\)$/, '')

// Removes a leading period.
exports.removeLeadingDot =
  title =>
    title
      .replace(/^\./, '')
