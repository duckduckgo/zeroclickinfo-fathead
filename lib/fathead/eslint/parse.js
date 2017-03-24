'use strict';

var fs = require('fs');
var path = require('path');
var yaml = require('js-yaml');
var write = require('./lib/write.js');

var summaryFile = path.join(__dirname, '/download/repo/_data/rules.yml');
var outputFile = './output.txt';
var detailsDirectory = path.join(__dirname, '/download/repo/docs/rules');

var ruleSet = yaml.safeLoad(fs.readFileSync(summaryFile));

write(outputFile, ruleSet, detailsDirectory);
