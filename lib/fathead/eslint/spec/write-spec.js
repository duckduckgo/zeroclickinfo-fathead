'use strict';

describe('"write()"', function() {
    var fs = require('fs');
    var path = require('path');
    var write = require('../lib/write.js');

    var index = 0;
    var input;
    var workingDirectory = __dirname;
    var ruleNames = ['test-write-input-1-no-console', 'test-write-input-2-no-debugger', 'test-write-input-1-accessor-pairs'];
    var ruleDetails = ['Details for rule 0.0', 'Details for rule 0.1', 'Details for rule 1.0'];

    /**
     * Create a unique test path for every unit test.
     * @return {string} Unique path.
     */
    function createTestPath() {
        index += 1;
        var file = 'test-write-ouptut' + index + '.txt';
        return path.join(workingDirectory, file);
    }

    /**
     * Create a rule set for testing.
     * @return {object} Rule set in production syntax with test values.
     */
    function createTestRuleset() {
        return {
            categories: [{
                name: 'Category #1: Possible Errors',
                rules: [{
                    description: 'disallow the use of `console`',
                    fixable: false,
                    name: ruleNames[0],
                    recommended: true
                }, {
                    description: 'description: disallow the use of `debugger`',
                    fixable: false,
                    name: ruleNames[1],
                    recommended: true
                }]
            }, {
                name: 'Category #2: Best Practices',
                rules: [{
                    description: 'enforce getter and setter pairs in objects',
                    fixable: false,
                    name: ruleNames[2],
                    recommended: false
                }]
            }, {
                name: 'Category #3: empty',
                rules: []
            }]
        };
    }

    beforeAll(function() {
        // init file system
        fs.writeFileSync(path.join(workingDirectory, ruleNames[0] + '.md'), ruleDetails[0]);
        fs.writeFileSync(path.join(workingDirectory, ruleNames[1] + '.md'), ruleDetails[1]);
        fs.writeFileSync(path.join(workingDirectory, ruleNames[2] + '.md'), ruleDetails[2]);
    });

    afterAll(function() {
        // clean up file system
        fs.unlink(path.join(workingDirectory, ruleNames[0] + '.md'));
        fs.unlink(path.join(workingDirectory, ruleNames[1] + '.md'));
        fs.unlink(path.join(workingDirectory, ruleNames[2] + '.md'));
    });

    beforeEach(function() {
        input = createTestRuleset();
    });

    describe('consumes details from input files', function() {
        it('throws when file is not present', function() {
            var path = createTestPath();
            expect(() => write(path, input, path.join(workingDirectory, '/INVALID'))).toThrowError();
            fs.unlink(path);
        });
    });

    describe('produces an output file', function() {
        it('at the correct location.', function() {
            var path = createTestPath();

            write(path, input, workingDirectory);

            var stat = fs.statSync(path);
            expect(stat).toBeDefined();
            expect(stat.isFile()).toBeTruthy();

            fs.unlink(path);
        });

        it('with the correct length', function() {
            var path = createTestPath();

            write(path, input, workingDirectory);

            var content = fs.readFileSync(path);
            var producedLines = content.toString().split('\n');
            expect(producedLines.length).toBe(4); // +1 for empty line at EOF
            expect(producedLines[0]).toContain(input.categories[0].rules[0].name);
            expect(producedLines[1]).toContain(input.categories[0].rules[1].name);
            expect(producedLines[2]).toContain(input.categories[1].rules[0].name);

            fs.unlink(path);
        });

        it('replaces the output file, if present', function() {
            var path = createTestPath();
            fs.writeFileSync(path, 'this text must be deleted.\n');

            write(path, input, workingDirectory);

            var content = fs.readFileSync(path);
            expect(content.toString().split('\n').length).toBe(4); // +1 for empty line at EOF

            fs.unlink(path);
        });
    });

    it('is a function', function() {
        expect(typeof write).toBe('function');
    });
});
