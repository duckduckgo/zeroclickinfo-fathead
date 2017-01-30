'use strict';

var markdown = require('markdown').markdown;

/**
 * Format pretty HTML for abstract.
 * Parse the text from source format (Markdown)
 * and convert to a pretty rendered HTML snippet.
 * @param  {string} content Content in source format.
 * @return {string}         Content in target format.
 */
function formatHtml(content) {
    const ABSTRACT_PREFIX = '<section class="prog__container">';
    const ABSTRACT_POSTFIX = '</section>';

    return ABSTRACT_PREFIX + markdown.toHTML(content) + ABSTRACT_POSTFIX;
}

/**
 * Apply special treatment to the summary text.
 * The summary text from fathead source must be treated specially:
 * - make first letter uppr case.
 * - Add a dot at the end.
 * @param  {string} content Content in source format.
 * @return {string}         Content in target format.
 */
function formatSummary(content) {
    var result =  content.charAt(0).toUpperCase() + content.slice(1);
    if(result.slice(-1) !== '.') {
        result = result + '.';
    }
    return result;
}

/**
 * Convert a documentation node from the source format
 * to DDG's target format.
 * @param  {object} node    Documentation node in source format.
 * @return {object}         Documentation node in target format.
 */
function convert(node) {
    const BASE_URL = 'http://eslint.org/docs/rules/';
    const TYPE_ARTICLE = 'A';
    var article = {};

    if(!node) {
        throw new ReferenceError('"node" is not defined');
    }

    if (!node.name) {
        throw new ReferenceError('"node.name" is not defined');
    }

    if (!node.category) {
        throw new ReferenceError('"node.category" is not defined');
    }

    if (!node.description) {
        throw new ReferenceError('"node.description" is not defined');
    }

    var summary = formatSummary(node.description);

    article.title = node.name;
    article.type = TYPE_ARTICLE;
    article.category = node.category;
    article.abstract = formatHtml(summary);
    article.imageUrl = 'http://eslint.org/img/logo.svg';
    article.sourceUrl = BASE_URL + node.name;

    return article;
}

module.exports = convert;
