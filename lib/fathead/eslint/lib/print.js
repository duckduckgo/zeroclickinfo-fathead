'use strict';

var fs = require('fs');

/**
 * Print given data to a file.
 * @param  {object | file | number} file name or descriptor.
 * @param  {object | sting} data    Content to write.
 * @return {undefied}               undefied
 */
function print(file, data) {
    fs.appendFileSync(file, data);
}

module.exports = print;
