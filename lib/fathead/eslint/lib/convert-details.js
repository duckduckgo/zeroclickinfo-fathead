'use strict';

/**
 * Extract relevant details from a given rule description.
 * @param  {string} description The rule description (file content)
 * @return {object}             The extracted details. Result contains **title**
 *                              and **abstract** of the rule.
 */
function convertDetails(description) {
    var heading1Map = [];
    var heading2Map = [];

    if(typeof description !== typeof '') {
        throw new ReferenceError('"description" must be of type string.');
    }

    var lines = description.split('\n');
    lines.forEach(function createFileMap(line, index) {
        if(line.startsWith('# ')) {
            heading1Map.push(index);
        } else if(line.startsWith('## ')) {
            heading2Map.push(index);
        }
    });

    if(heading1Map.length === 0) {
        throw new Error('Invalid content: Could not find title.');
    }

    if(heading2Map.length > 0 && heading1Map[0] > heading2Map[0]) {
        throw new Error('Invalid content: First heading should be first level. Found second level heading instead.');
    }

    var result = {
        title: lines[heading1Map[0]].slice(2), // remove markdown syntax '# '
        abstract: lines.slice(heading1Map[0] + 1, heading2Map[1]).join('\n')
    };

    return result;
}

module.exports = convertDetails;
