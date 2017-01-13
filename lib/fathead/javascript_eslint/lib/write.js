'use strict';

var fs = require('fs');
var convert = require('./convert.js');
var format = require('./format.js');

/**
 * Write the given content to the output file.
 * @param  {string} fileName    Path to file.
 * @param  {array} content      Content to write. Array with one entry per line.
 * @return {undefined}
 */
function write(fileName, content) {
    // initialize folder
    fs.writeFileSync(fileName, '');

    content.forEach(function(element) {
        element.category = 'Possible Errors';
        var outputField = convert(element);
        var outputLine = format(outputField);

        fs.appendFileSync(fileName, outputLine);
    });
}

module.exports = write;
