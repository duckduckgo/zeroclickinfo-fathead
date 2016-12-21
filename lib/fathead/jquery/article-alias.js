'use strict';

var inputFileName = 'articles.txt';
var outputFileName = 'redirects.txt';
var filePath = ''

var appendString = ' example,';

var fs = require('fs');
var readline = require('readline');
var rl = readline.createInterface(fs.createReadStream(filePath + inputFileName), fs.createWriteStream(filePath + outputFileName,{'flags': 'a'}));

function parseInput(inputString) {
    var outputString = inputString;

    function upperToLowerSpace(match) {
        return ' ' + match.toLowerCase();
    }

    outputString = outputString.replace('jQuery', '').replace(/[\(\)]/g, '').replace(/[\.\:\-]/g, ' ').replace(/([A-Z]+)/g, upperToLowerSpace).replace(/\ \ /g, ' ').trim();

    return outputString;
}

var parsedString = '';

rl.on('line', (line) => {
    parsedString = parseInput(line);
    parsedString ? rl.output.write(parsedString + appendString + line + '\n') : null;
    rl.close();
});
