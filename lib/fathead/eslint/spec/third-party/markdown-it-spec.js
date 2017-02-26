'use strict';

// Test functionality from third party library
describe('Third party library "markdown-it.render()"', function() {
    var markdown = require('markdown-it')();

    describe('(basic examples)', function() {
        it('should wrap text in <p> tag.', function() {
            var text = 'Hello World';
            expect(markdown.render(text)).toEqual('<p>' + text + '</p>\n');
        });

        it('should wrap headline in <h*> tag.', function() {
            expect(markdown.render('# Headline\nText'))
                .toEqual('<h1>Headline</h1>\n<p>Text</p>\n');
        });

        it('should wrap code in <code> tag.', function() {
            expect(markdown.render('This is text `this is code`'))
                .toEqual('<p>This is text <code>this is code</code></p>\n');
        });
    });

    //// describe('(real world examples)', function() {
    ////     it('should fromat the "curly" rule.', function() {
    ////         var ruleCurly = '(fixable) The `--fix` option on the [command line](../user-guide/command-line-interface#fix) automatically fixes problems reported by this rule.' +
    ////             '\n\nJavaScript allows the omission of curly braces when a block contains only one statement. However, it is considered by many to be best practice to _never_ omit curly braces around blocks, even when they are optional, because it can lead to bugs and reduces code clarity. So the following:' +
    ////             '\n\n```js\nif (foo) foo++;\n```' +
    ////             '\n\nCan be rewritten as:' +
    ////             '\n\n```js\nif (foo) {\n    foo++;\n}\n```' +
    ////             '\n\nThere are, however, some who prefer to only use braces when there is more than one statement to be executed.' +
    ////             '\n\n## Rule Details\n\nThis rule is aimed at preventing bugs and increasing code clarity by ensuring that block statements are wrapped in curly braces. It will warn when it encounters blocks that omit curly braces.';
    ////         expect(markdown.render(ruleCurly)).toEqual('string');
    ////     });
    //// });
});
