'use strict';

var markdown = require('markdown-it')();

/**
 * Format pretty HTML for abstract.
 * - Convert markdown to HTML
 * - Add surrounding SECTION tags
 * - Exscape newlines `\n` and tabs `\t`
 * @param  {string} content Content in source format.
 * @return {string}         Content in target format.
  */
function markdownToHtml(content) {
    const ABSTRACT_PREFIX = '<section class="prog__container">';
    const ABSTRACT_POSTFIX = '</section>';

    var contentInHtml = ABSTRACT_PREFIX + markdown.render(content) + ABSTRACT_POSTFIX;
    var replacedNewlines = contentInHtml.split('\n').join('\\n');
    var replacedTabs = replacedNewlines.split('\t').join('  ');
    return replacedTabs;
}

module.exports = markdownToHtml;
