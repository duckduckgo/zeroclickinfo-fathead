'use strict';

var markdown = require('markdown').markdown;
var convertDetails = require('./convert-details.js');

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
 * - make first letter upper case.
 * - Add a dot at the end of the sentence.
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
 * Convert the category property.
 * The rule's category will combine several input node values, like recommended or fixable.
 * Categories should generally end with a plural noun.
 * @param  {object} node Documentation node in source format.
 * @return {string}      Categories in target format.
 */
function convertCategory(node) {
    var result = '' + node.category;
    result += node.fixable ? '\\nFixable Rules' : '';
    result += node.recommended ? '\\nRecommended Rules' : '';
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

    if(typeof node.category !== typeof '') {
        throw new ReferenceError('"node.category" must be of type string.');
    }

    if (!node.description) {
        throw new ReferenceError('"node.description" is not defined');
    }

    if (!node.details) {
        throw new ReferenceError('"node.details" is not defined');
    }

    var summary = formatSummary(node.description);
    var details = convertDetails(node.details);

    article.title = details.title;
    article.abstract = formatHtml(summary);

    article.name = node.name;
    article.type = TYPE_ARTICLE;
    article.category = convertCategory(node);
    article.imageUrl = 'http://eslint.org/img/logo.svg';
    article.sourceUrl = BASE_URL + node.name;

    return article;
}

module.exports = convert;
