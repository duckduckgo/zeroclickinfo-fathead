'use strict';

describe('"sliceAbstract()"', function() {
    var sliceAbstract = require('../lib/slice-abstract.js');

    it('should return rule title', function() {
        var content = 'Some content\nbefore the title\n\nThis may even contain # hashtags # as long as they are not at the beginning of a line.\n# The title\nAnd some conentent after\n\tthe title.\n# Even a second title.';
        var result = sliceAbstract(content);
        expect(result.title).toEqual('The title');
    });

    it('should return rule abstract with everything until second H2', function() {
        var content = 'Some content before\n# The title\nThese lines\nare beginning of the abstract.\n## First H2\nStill part or the abstract\n## Second H2\nBelow here\ncomes the rest of the file,\n\nwhich is not part of the abstract.';
        var result = sliceAbstract(content);
        expect(result.abstract).toEqual('These lines\nare beginning of the abstract.\n## First H2\nStill part or the abstract');
    });

    it('should cut the "reccomended" note from the abstract', function() {
        var content = '# Some Title\nSome content.\n(recommended) The `"extends": "eslint:recommended"` property in a configuration file enables this rule.\nEmpty line after note\nSome more content.';
        var result = sliceAbstract(content);
        expect(result.abstract).toEqual('Some content.\nSome more content.');
    });

    it('should cut the "fixable" note from the abstract', function() {
        var content = '# Some Title\nSome content.\n(fixable) The `--fix` option on the [command line](../user-guide/command-line-interface#fix) can automatically fix some of the problems reported by this rule.\nEmpty line after note\nSome more content.';
        var result = sliceAbstract(content);
        expect(result.abstract).toEqual('Some content.\nSome more content.');
    });

    it('should throw, when input is invalid', function() {
        expect(() => sliceAbstract(1)).toThrowError('"description" must be of type string.');
        expect(() => sliceAbstract('Content\nwithout title.')).toThrowError('Invalid content: Could not find title.');
        expect(() => sliceAbstract('## Heading two\nbefore\n# Heading one\nshould throw.')).toThrowError('Invalid content: First heading should be first level. Found second level heading instead.');
    });

    it('should be a function', function() {
        expect(typeof sliceAbstract).toBe('function');
    });
});
