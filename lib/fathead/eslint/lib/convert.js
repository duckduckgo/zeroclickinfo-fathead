'use strict';

/**
 * Format pretty HTML for abstract.
 * Parse the text from source format (Markdown)
 * and convert to a pretty rendered HTML snippet.
 * @param  {string} content Content in source format.
 * @return {string}         Content in target format.
 */
function formatHtml(content) {
    const ABSTRACT_PREFIX = '<section class="prog__container"><p id="obj.desc">';
    const ABSTRACT_POSTFIX = '</p></section>';

    return ABSTRACT_PREFIX + content + ABSTRACT_POSTFIX;
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

    article.title = node.name;
    article.type = TYPE_ARTICLE;
    article.category = node.category;
    article.abstract = formatHtml(node.description);
    article.imageUrl = 'http://eslint.org/img/logo.svg';
    article.sourceUrl = BASE_URL + node.name;

    return article;
}

module.exports = convert;
