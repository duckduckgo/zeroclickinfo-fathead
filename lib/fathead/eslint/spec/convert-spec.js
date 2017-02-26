'use strict';

describe('"convert()"', function() {
    var proxyquire = require('proxyquire');
    var mock;
    var convert;
    var input;

    beforeEach(function arrangeMocks() {
        mock = {
            convertDetailsMock: function() {}
        };
        spyOn(mock, 'convertDetailsMock').and.returnValues(7);

        convert = proxyquire('../lib/convert.js', {
            './convert-details.js': mock.convertDetailsMock
        });
    });

    beforeEach(function arrangeVariables() {
        input = {
            category: 'Possible Errors',
            description: 'Disallow the use of `console`',
            details: 'Some details. Not evaluated.',
            fixable: false,
            name: 'no-console',
            recommended: false
        };
    });

    describe('called with a valid object', function() {
        it('should set all poperties, that can be matched to one.', function() {
            var result = convert(input);

            expect(result.name).toBe(input.name); // no number
            expect(result.type).toBe('A'); // 2
            expect(result.category).toBe(input.category); // 5
            expect(result.imageUrl).toBe('http://eslint.org/img/logo.svg'); // 10
            expect(result.abstract).toContain('Disallow the use of '); // 12
            expect(result.abstract).toContain('console'); // 12
            expect(result.sourceUrl).toBe('http://eslint.org/docs/rules/' + input.name); // 13
        });

        describe('check category:', function() {
            it('should contain rule category', function() {
                var result = convert(input);
                expect(result.category).toEqual(input.category);
            });

            it('should throw, when rule category is not a string', function() {
                input.category = {
                    'complex': 'object'
                };
                expect(() => convert(input)).toThrowError('"node.category" must be of type string.');
            });

            it('should contain "Fixable Rules" for "fixable" rules', function() {
                input.fixable = true;
                var result = convert(input);
                expect(result.category).toEqual(input.category + '\\nFixable Rules');
            });

            it('should contain "Recommended Rules" for "recommended" rules', function() {
                input.recommended = true;
                var result = convert(input);
                expect(result.category).toEqual(input.category + '\\nRecommended Rules');
            });
        });

        describe('(use mock for "convertDetails()")', function() {
            beforeEach(function arrangeMocks() {
                mock = {
                    convertDetailsMock: function() {}
                };
                spyOn(mock, 'convertDetailsMock').and.returnValues({
                    title: 'Disallow the use of `console`'
                });

                convert = proxyquire('../lib/convert.js', {
                    './convert-details.js': mock.convertDetailsMock
                });
            });

            it('should call "convertDetails()" with "details"', function() {
                convert(input);
                expect(mock.convertDetailsMock.calls.count()).toEqual(1);
                expect(mock.convertDetailsMock).toHaveBeenCalledWith(input.details);
            });

            describe('(check title)', function() {
                it('should set article title', function() {
                    var result = convert(input);

                    expect(result.title).toEqual('Disallow the use of `console`');
                });
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

        it('"input.details" missing', function() {
            delete input.details;
            expect(() => convert(input)).toThrow(new ReferenceError('"node.details" is not defined'));
        });
    });

    it('is a function', function() {
        expect(typeof convert).toBe('function');
    });
});
