'use strict'

const fs = require('fs')
const DOMParser = require('xmldom').DOMParser

// Reads an XML file at path and parses it into a DOM.
exports.read =
  path =>
    readFile(path)
      .then(String)
      .then(Document)

// Promise wrapper around fs.readFile().
const readFile =
  path =>
    new Promise((resolve, reject) =>
      fs.readFile(
        path,
        (err, data) =>
          err ? reject(err) : resolve(data)
      )
    )

const Document =
  string => {
    const parser = new DOMParser()
    const dom = parser.parseFromString(string)

    if (!dom) throw new Error('could not parse DOM')

    return dom
  }
