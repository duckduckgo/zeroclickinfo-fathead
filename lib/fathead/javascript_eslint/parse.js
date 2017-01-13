'use strict';

var fs = require('fs');
var yaml = require('js-yaml');
var write = require('./lib/write.js');

var inputFile = __dirname + '/download/rules.yml';
var outputFile = './output.txt';

var content = yaml.safeLoad(fs.readFileSync(inputFile));

write(outputFile, content.categories[0].rules);
