'use strict';

// Test functionality from third party library
describe('Third party library "markdown.markdown"', function() {
    var markdown = require('markdown').markdown;

    it('should wrap text in <p> tag.', function() {
        var text = 'Hello World';
        expect(markdown.toHTML(text)).toEqual('<p>' + text + '</p>');
    });

    it('should wrap headline in <h*> tag.', function() {
        expect(markdown.toHTML('# Headline\nText'))
            .toEqual('<h1>Headline</h1>\n\n<p>Text</p>');
    });

    it('should wrap code in <code> tag.', function() {
        expect(markdown.toHTML('This is text `this is code`'))
            .toEqual('<p>This is text <code>this is code</code></p>');
    });
});
