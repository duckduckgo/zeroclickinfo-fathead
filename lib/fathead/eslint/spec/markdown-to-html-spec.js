'use strict';

describe('"markdownToHtml()"', function() {
    var markdownToHtml = require('../lib/markdown-to-html.js');

    /**
     * Wrap input in HTML section.
     * @param  {string} content The input string.
     * @return {string}         Content wrapped in HTML.
     */
    function wrapHtmlSection(content) {
        return '<section class="prog__container"><p>' + content + '</p>\\n</section>';
    }

    it('should be wrapped in a HMTL section', function() {
        var description = 'Plain text paragraph.';
        var result = markdownToHtml(description);
        expect(result).toEqual(wrapHtmlSection(description));
    });

    it('should mark inline code', function() {
        var description = 'Disallow `console`.';
        var result = markdownToHtml(description);
        expect(result).toEqual(wrapHtmlSection('Disallow <code>console</code>.'));
    });

    it('should mark paragraphs', function() {
        var description = 'First paragraph.\n\nSecondparagraph.';
        var result = markdownToHtml(description);
        expect(result).toEqual(wrapHtmlSection('First paragraph.</p>\\n<p>Secondparagraph.'));
    });

    it('should escape "new lines"', function() {
        var description = 'This is\na text\nwith newlines.';
        var result = markdownToHtml(description);
        expect(result).toEqual(wrapHtmlSection('This is\\na text\\nwith newlines.'));
    });

    it('should escape "tabs"', function() {
        var description = 'This is\na text\nwith newlines\tand\ttabs.';
        var result = markdownToHtml(description);
        expect(result).toEqual(wrapHtmlSection('This is\\na text\\nwith newlines  and  tabs.'));
    });
});
