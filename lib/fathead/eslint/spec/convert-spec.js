'use strict';

var convert = require('../lib/convert.js');

describe('"convert()"', function() {
    const ABSTRACT_PREFIX = '<section class="prog__container"><p id="obj.desc">';
    const ABSTRACT_POSTFIX = '</p></section>';
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
            expect(result.abstract).toContain(input.description); // 12
            expect(result.sourceUrl).toBe('http://eslint.org/docs/rules/' + input.name); // 13
        });

        describe('check HTML:', function() {
            it('should produce well formated HTML abstract.', function() {
                var result = convert(input);
                expect(result.abstract).toBe(ABSTRACT_PREFIX + input.description + ABSTRACT_POSTFIX);
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
