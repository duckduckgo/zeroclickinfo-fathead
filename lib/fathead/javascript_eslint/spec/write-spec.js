'use strict';

var path = require('path');
var fs = require('fs');
var write = require('../lib/write.js');

describe('"write()"', function() {
    it('is a function', function() {
        expect(typeof write).toBe('function');
    });

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
            input = [{
                description: 'disallow the use of `console`',
                category: 'Possible Errors',
                fixable: false,
                name: 'no-console',
                recommended: true
            }, {
                description: 'description: disallow the use of `debugger`',
                category: 'Possible Errors',
                fixable: false,
                name: 'no-debugger',
                recommended: true
            }];
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
            expect(producedLines.length).toBe(input.length + 1);
            expect(producedLines[0]).toContain(input[0].name);
            expect(producedLines[1]).toContain(input[1].name);

            fs.unlink(path);
        });

        it('replaces the output file, if present', function() {
            var path = createTestPath();
            fs.writeFileSync(path, 'this text must be deleted.\n');

            write(path, input);

            var content = fs.readFileSync(path);
            expect(content.toString().split('\n').length).toBe(input.length + 1);

            fs.unlink(path);
        });
    });
});
