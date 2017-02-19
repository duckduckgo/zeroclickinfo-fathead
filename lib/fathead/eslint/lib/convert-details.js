'use strict';

/**
 * Extract relevant details from a given rule description.
 * @param  {string} description The rule description (file content)
 * @return {object}             The extracted details.
 */
function convertDetails(description) {
    /**
     * Detect <h1> elements as first level headings.
     * @param  {string} line The line to check.
     * @return {boolean}     True, when the line matches.
     */
    function detectFirstLevelHeading(line) {
        return line.startsWith('# ');
    }

    /**
     * Detect <h2> elements as second level headings.
     * @param  {string} line The line to check.
     * @return {boolean}     True, when the line matches.
     */
    function detectSecondLevelHeading(line) {
        return line.startsWith('## ');
    }

    var lines = description.split('\n');

    var firstH1Index = lines.findIndex(detectFirstLevelHeading);
    var firstH2Index = lines.findIndex(detectSecondLevelHeading);

    if(firstH1Index === -1) {
        throw new Error('Invalid content: Could not find title.');
    }

    if(firstH1Index > firstH2Index && firstH2Index !== -1) {
        throw new Error('Invalid content: First heading should be first level. Found second level heading instead.');
    }

    var result = {
        title: lines[firstH1Index].slice(2), // remove markdown syntax '# '
        abstract: lines.slice(firstH1Index + 1).join('\n')
    };

    return result;
}

module.exports = convertDetails;
