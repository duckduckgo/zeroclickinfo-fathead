'use strict';

var path = require('path');
var fs = require('fs');
var write = require('../lib/write.js');

describe('"write()"', function() {
    describe('produces an output file', function() {
        var index = 0;
        var input;

        /**
         * Create a unique test path for every unit test.
         * @return {string} Unique path.
         */
        function createTestPath() {
            index += 1;
            var file = 'write-output-' + index + '.txt';
            return path.join(__dirname, file);
        }

        beforeEach(function() {
            input = {
                categories: [{
                    name: 'Category #1: Possible Errors',
                    rules: [{
                        description: 'disallow the use of `console`',
                        fixable: false,
                        name: 'Rule #1: no-console',
                        recommended: true
                    }, {
                        description: 'description: disallow the use of `debugger`',
                        fixable: false,
                        name: 'Rule #2: no-debugger',
                        recommended: true
                    }]
                }, {
                    name: 'Category #2: Best Practices',
                    rules: [{
                        description: 'enforce getter and setter pairs in objects',
                        fixable: false,
                        name: 'Rule #1: accessor-pairs',
                        recommended: false
                    }]
                }, {
                    name: 'Category #3: empty',
                    rules: []
                }]
            };
        });

        it('at the correct location.', function() {
            var path = createTestPath();

            write(path, input);

            var stat = fs.statSync(path);
            expect(stat).toBeDefined();
            expect(stat.isFile()).toBeTruthy();

            fs.unlink(path);
        });

        it('with the correct length', function() {
            var path = createTestPath();

            write(path, input);

            var content = fs.readFileSync(path);
            var producedLines = content.toString().split('\n');
            expect(producedLines.length).toBe(4);   // +1 for empty line at EOF
            expect(producedLines[0]).toContain(input.categories[0].rules[0].name);
            expect(producedLines[1]).toContain(input.categories[0].rules[1].name);
            expect(producedLines[2]).toContain(input.categories[1].rules[0].name);

            fs.unlink(path);
        });

        it('replaces the output file, if present', function() {
            var path = createTestPath();
            fs.writeFileSync(path, 'this text must be deleted.\n');

            write(path, input);

            var content = fs.readFileSync(path);
            expect(content.toString().split('\n').length).toBe(4);   // +1 for empty line at EOF

            fs.unlink(path);
        });
    });

    it('is a function', function() {
        expect(typeof write).toBe('function');
    });
});
