'use strict';

describe('"convertDetails()"', function() {
    var convertDetails = require('../lib/convert-details.js');

    it('returns rule title', function() {
        var content = 'Some content\nbefore the title\n\nThis may even contain # hashtags # as long as they are not at the beginning of a line.\n# The title\nAnd some conentent after\n\tthe title.\n# Even a second title.';
        var result = convertDetails(content);
        expect(result.title).toEqual('The title');
    });

    it('returns rule abstract with everything until second H2', function() {
        var content = 'Some content before\n# The title\nThese lines\nare beginning of the abstract.\n## First H2\nStill part or the abstract\n## Second H2\nBelow here\ncomes the rest of the file,\n\nwhich is not part of the abstract.';
        var result = convertDetails(content);
        expect(result.abstract).toEqual('These lines\nare beginning of the abstract.\n## First H2\nStill part or the abstract');
    });

    it('should throw, when input is invalid', function() {
        expect(() => convertDetails('Content\nwithout title.')).toThrowError('Invalid content: Could not find title.');
        expect(() => convertDetails('## Heading two\nbefore\n# Heading one\nshould throw.')).toThrowError('Invalid content: First heading should be first level. Found second level heading instead.');
    });

    it('is a function', function() {
        expect(typeof convertDetails).toBe('function');
    });
});
