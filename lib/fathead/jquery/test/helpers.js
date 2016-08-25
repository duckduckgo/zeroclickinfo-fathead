'use strict'

const path = require('path')
const DOMParser = require('xmldom').DOMParser

exports.xml =
  string =>
    new DOMParser()
      .parseFromString(string)

exports.fixturePath =
  filename =>
    path.resolve(__dirname, 'fixtures', filename)

exports.silenceXmldomErrors = () => {
  console.error = function (msg) {
    if (!String(msg).startsWith('[xmldom error]')) {
      return __original__error.apply(this, arguments)
    }
  }
}

exports.unsilenceXmldomErrors = () => {
  console.error = __original__error
}

const __original__error = console.error
