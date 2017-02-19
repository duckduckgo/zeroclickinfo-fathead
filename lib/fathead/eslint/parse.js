'use strict';

var fs = require('fs');
var path = require('path');
var yaml = require('js-yaml');
var write = require('./lib/write.js');

var inputFile = path.join(__dirname, '/download/summary/rules.yml');
var outputFile = './output.txt';

var ruleSet = yaml.safeLoad(fs.readFileSync(inputFile));

write(outputFile, ruleSet);
