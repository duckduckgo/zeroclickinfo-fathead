'use strict';

var convert = require('../lib/convert.js');

describe('"convert()"', function() {
    var input;

    beforeEach(function() {
        input = {
            description: 'Disallow the use of `console`',
            category: 'Possible Errors',
            fixable: false,
            name: 'no-console',
            recommended: true
        };
    });

    describe('called with an valid object', function() {
        it('should produce a valid result.', function() {
            var result = convert(input);
            expect(result.title).toBe(input.name); // 1
            expect(result.type).toBe('A'); // 2
            expect(result.category).toBe(input.category); // 5
            expect(result.imageUrl).toBe('http://eslint.org/img/logo.svg'); // 10
            expect(result.abstract).toContain('Disallow the use of '); // 12
            expect(result.abstract).toContain('console'); // 12
            expect(result.sourceUrl).toBe('http://eslint.org/docs/rules/' + input.name); // 13
        });

        describe('check abstract:', function() {
            /**
             * Wrap input in HTML section.
             * @param  {string} content The input string.
             * @return {string}         Content wrapped in HTML.
             */
            function wrapHtmlSection(content) {
                return '<section class="prog__container"><p>' + content + '</p></section>';
            }

            describe('format summary in abstract:', function() {
                it('should add a dot at the end, if not present', function() {
                    input.description = 'Valid sentence ends with a dot';
                    var result = convert(input);
                    expect(result.abstract).toEqual(wrapHtmlSection(input.description + '.'));
                });

                it('should make first letter uppercase, if not already', function() {
                    input.description = 'valid sentence start with an uppercase letter.';
                    var result = convert(input);
                    expect(result.abstract).toEqual(wrapHtmlSection('Valid sentence start with an uppercase letter.'));
                });

                it('should not change a valid sentence.', function() {
                    input.description = 'This is a valid sentence.';
                    var result = convert(input);
                    expect(result.abstract).toEqual(wrapHtmlSection(input.description));
                });
            });

            describe('check HTML format:', function() {

                it('should be wrapped in a HMTL section', function() {
                    input.description = 'Plain text paragraph.';
                    var result = convert(input);
                    expect(result.abstract).toEqual(wrapHtmlSection(input.description));
                });

                it('should mark inline code', function() {
                    input.description = 'Disallow `console`.';
                    var result = convert(input);
                    expect(result.abstract).toEqual(wrapHtmlSection('Disallow <code>console</code>.'));
                });
            });
        });
    });

    describe('called with an invalid object throws:', function() {
        it('input "undefied"', function() {
            expect(() => convert(undefined)).toThrow(new ReferenceError('"node" is not defined'));
        });

        it('"input.name" missing', function() {
            delete input.name;
            expect(() => convert(input)).toThrow(new ReferenceError('"node.name" is not defined'));
        });

        it('"input.category" missing', function() {
            delete input.category;
            expect(() => convert(input)).toThrow(new ReferenceError('"node.category" is not defined'));
        });

        it('"input.description" missing', function() {
            delete input.description;
            expect(() => convert(input)).toThrow(new ReferenceError('"node.description" is not defined'));
        });
    });

    it('is a function', function() {
        expect(typeof convert).toBe('function');
    });
});
