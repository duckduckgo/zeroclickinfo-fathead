'use strict';

var inputFileName = 'articles.txt';
var outputFileName = 'redirects.txt';
var filePath = ''

var appendString = ' example,';
var upperRegex = /([A-Z]+)/g;

var fs = require('fs');
var readline = require('readline');
var rl = readline.createInterface(fs.createReadStream(filePath + inputFileName), fs.createWriteStream(filePath + outputFileName,{'flags': 'a'}));

function parseInput(inputString, keepUpper) {
    var outputString = inputString;

    function upperToLowerSpace(match) {
        return ' ' + match.toLowerCase();
    }

    if (keepUpper) {
        outputString = outputString.replace('jQuery', '').replace(/[\(\)]/g, '').replace(/[\.\:\-]/g, ' ').trim();
    } else {
        outputString = outputString.replace('jQuery', '').replace(/[\(\)]/g, '').replace(/[\.\:\-]/g, ' ').replace(upperRegex, upperToLowerSpace).replace(/\ \ /g, ' ').trim();
    }

    return outputString;
}

var parsedString = '';

rl.on('line', (line) => {

    if (upperRegex.test(line.replace('jQuery', ''))) {
        parsedString = parseInput(line, false);
        parsedString ? rl.output.write(parsedString + appendString + line + '\n') : null;

        parsedString = parseInput(line, true);
        parsedString ? rl.output.write(parsedString + appendString + line + '\n') : null;
    } else {
        parsedString = parseInput(line, false);
        parsedString ? rl.output.write(parsedString + appendString + line + '\n') : null;
    }

    rl.close();
});
