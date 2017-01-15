'use strict';

var format = require('../lib/format.js');

describe('"format()"', function() {
    const DELIMITER = '\t';
    const LINE_BREAK = '\n';
    var input;

    beforeEach(function() {
        input = {
            title: 'TITLE a.k.a. ID.',
            type: 'The article has a type.',
            category: 'This is the CATEGORY.',
            abstract: 'This is the abstract. With `code`.',
            sourceUrl: 'This is the source url.',
            imageUrl: 'This is the image url.'
        };
    });

    describe('valid input', function() {
        it('printed correctly', function() {
            var result = format(input);
            expect(result).toBe(
                input.title + DELIMITER +       //  1
                input.type + DELIMITER +        //  2
                DELIMITER +                     //  3
                DELIMITER +                     //  4
                input.category + DELIMITER +    //  5
                DELIMITER +                     //  6
                DELIMITER +                     //  7
                DELIMITER +                     //  8
                DELIMITER +                     //  9
                DELIMITER +                     // 10
                input.imageUrl + DELIMITER +    // 11
                input.abstract + DELIMITER +    // 12
                input.sourceUrl + LINE_BREAK);  // 13
        });
    });

    it('is a function', function() {
        expect(typeof format).toBe('function');
    });
});
