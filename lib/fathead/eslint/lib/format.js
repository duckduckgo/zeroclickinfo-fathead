'use strict';

/**
 * Formats an article object to a string.
 * Formatting rules are specified here: https://docs.duckduckhack.com/resources/fathead-overview.html#output-fields
 * @param  {object} article     The input as object.
 * @return {string}             The result as string.
 */
function format(article) {
    const DELIMITER = '\t';
    const LINE_BREAK = '\n';
    var result = '';

    result +=
        article.title + DELIMITER +     //  1
        article.type + DELIMITER +      //  2
        DELIMITER +                     //  3
        DELIMITER +                     //  4
        article.category + DELIMITER +  //  5
        DELIMITER +                     //  6
        DELIMITER +                     //  7
        DELIMITER +                     //  8
        DELIMITER +                     //  9
        DELIMITER +                     // 10
        article.imageUrl + DELIMITER +  // 11
        article.abstract + DELIMITER +  // 12
        article.sourceUrl + LINE_BREAK; // 13
    return result;
}

module.exports = format;
