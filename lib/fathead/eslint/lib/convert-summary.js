'use strict';

/**
 * Apply special treatment to the summary text.
 * The summary text from fathead source must be treated specially:
 * - make first letter upper case.
 * - Add a dot at the end of the sentence.
 * @param  {string} content Content in source format.
 * @return {string}         Content in target format.
 */
function convertSummary(content) {
    var result =  content.charAt(0).toUpperCase() + content.slice(1);
    if(result.slice(-1) !== '.') {
        result = result + '.';
    }
    return result;
}

module.exports = convertSummary;
