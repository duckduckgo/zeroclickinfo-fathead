'use strict';

describe('"convert()"', function() {
    var proxyquire = require('proxyquire');
    var mock;
    var convert;
    var input;

    beforeEach(function arrangeMocks() {
        mock = {
            sliceAbstractMock: function() {}
        };
        spyOn(mock, 'sliceAbstractMock').and.returnValues({
            title: 'Article title',
            abstract: 'Article abstract.'
        });

        convert = proxyquire('../lib/convert.js', {
            './slice-abstract.js': mock.sliceAbstractMock
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

            expect(result.title).toBe(input.name); // 1
            expect(result.type).toBe('A'); // 2
            expect(result.category).toBe(input.category); // 5
            expect(result.imageUrl).toBe('http://eslint.org/img/logo.svg'); // 10
            expect(result.sourceUrl).toBe('http://eslint.org/docs/rules/' + input.name); // 13
        });

        describe('(check title and article)', function() {
            var mockReturnValue;

            beforeEach(function arrangeMocks() {
                mockReturnValue = {
                    title: 'Article title',
                    abstract: 'Article abstract.'
                };
                mock = {
                    sliceAbstractMock: function() {}
                };
                spyOn(mock, 'sliceAbstractMock').and.returnValues(mockReturnValue);

                convert = proxyquire('../lib/convert.js', {
                    './slice-abstract.js': mock.sliceAbstractMock
                });
            });

            //// it('should set article "title"', function() {
            ////     var result = convert(input);
            ////     expect(result.title).toEqual('Article title');
            //// });

            it('should set article "abstract"', function() {
                var result = convert(input);
                expect(result.abstract).toEqual('<section class="prog__container"><p>Article abstract.</p></section>');
            });

            //// it('should throw, when "title" is not defined', function() {
            ////     delete mockReturnValue.title;
            ////     expect(() => convert(input)).toThrow(new ReferenceError('"details.title" is not defined'));
            //// });

            it('should throw, when "abstract" is not defined', function() {
                delete mockReturnValue.abstract;
                expect(() => convert(input)).toThrow(new ReferenceError('"details.abstract" is not defined'));
            });

            it('should call "sliceAbstract()" with "details"', function() {
                convert(input);
                expect(mock.sliceAbstractMock.calls.count()).toEqual(1);
                expect(mock.sliceAbstractMock).toHaveBeenCalledWith(input.details);
            });
        });

        describe('(check category)', function() {
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
    });

    describe('should throw, when called with', function() {
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

    it('should be of type function', function() {
        expect(typeof convert).toBe('function');
    });
});
