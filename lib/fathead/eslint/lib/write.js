'use strict';

var fs = require('fs');
var convert = require('./convert.js');
var format = require('./format.js');

/**
 * Write the ESLint rule set to the output file.
 * @param  {string} fileName    Path to file.
 * @param  {object} ruleSet      The rule set.
 * @return {undefined}
 */
function write(fileName, ruleSet) {
    // initialize folder
    fs.writeFileSync(fileName, '');

    ruleSet.categories.forEach(function(category) {
        category.rules.forEach(function(rule) {
            rule.category = category.name;
            var outputField = convert(rule);
            var outputLine = format(outputField);

            fs.appendFileSync(fileName, outputLine);
        });
    });
}

module.exports = write;
