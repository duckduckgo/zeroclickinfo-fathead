'use strict';

describe('"convertSummary()"', function() {
    var convertSummary = require('../lib/convert-summary.js');

    describe('format summary in abstract:', function() {
        it('should add a dot at the end, if not present', function() {
            var description = 'Valid sentence ends with a dot';
            var result = convertSummary(description);
            expect(result).toEqual(description + '.');
        });

        it('should make first letter uppercase, if not already', function() {
            var description = 'valid sentence start with an uppercase letter.';
            var result = convertSummary(description);
            expect(result).toEqual('Valid sentence start with an uppercase letter.');
        });

        it('should not change a valid sentence.', function() {
            var description = 'This is a valid sentence.';
            var result = convertSummary(description);
            expect(result).toEqual(description);
        });
    });

    it('is a function', function() {
        expect(typeof convertSummary).toBe('function');
    });
});
