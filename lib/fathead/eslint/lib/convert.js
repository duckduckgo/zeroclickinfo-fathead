'use strict';

var sliceAbstract = require('./slice-abstract.js');
var markdownToHtml = require('./markdown-to-html.js');

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

    var details = sliceAbstract(node.details);

    //// if(!details.title)  {
    ////     throw new ReferenceError('"details.title" is not defined');
    //// }

    if(!details.abstract)  {
        throw new ReferenceError('"details.abstract" is not defined');
    }

    //// article.title = details.title;
    article.abstract = markdownToHtml(details.abstract);

    article.title = node.name;
    article.type = TYPE_ARTICLE;
    article.category = convertCategory(node);
    article.imageUrl = 'http://eslint.org/img/logo.svg';
    article.sourceUrl = BASE_URL + node.name;

    return article;
}

module.exports = convert;
