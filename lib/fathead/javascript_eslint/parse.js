'use strict';

var fs = require('fs');
var yaml = require('js-yaml');
var write = require('./lib/write.js');

var inputFile = __dirname + '/download/summary/rules.yml';
var outputFile = './output.txt';

var content = yaml.safeLoad(fs.readFileSync(inputFile));

// todo: Limited on 'Possible Errors' (idx 0) yet:
write(outputFile, content.categories[0].rules);
