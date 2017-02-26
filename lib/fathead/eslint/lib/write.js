'use strict';

var fs = require('fs');
var path = require('path');

var convert = require('./convert.js');
var format = require('./format.js');

/**
 * Write the ESLint rule set to the output file.
 * @param  {string} fileName            Path to file.
 * @param  {object} ruleSet             The rule set.
 * @param  {string} detailsDirectory    Path to folder with detail files.
 * @return {undefined}
 */
function write(fileName, ruleSet, detailsDirectory) {
    // initialize folder
    fs.writeFileSync(fileName, '');

    ruleSet.categories.forEach(function(category) {
        category.rules.forEach(function(rule) {
            rule.category = category.name;
            rule.details = '' + fs.readFileSync(path.join(detailsDirectory, rule.name + '.md'));

            var outputField = convert(rule);
            var outputLine = format(outputField);

            fs.appendFileSync(fileName, outputLine);
        });
    });
}

module.exports = write;
