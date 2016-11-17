var htmlParser = require('./../lib/htmlParser.js');
var fs = require('fs');
var assert = require('chai').assert;

var filterDateHtml = './download/api/ng/filter/date.html';
var $injectorHtml = './download/api/auto/service/$injector.html';

describe('htmlParser', function() {
    it('should get the abstract from filter date html correctly', function(done) {
        fs.readFile(filterDateHtml, 'utf-8', function(err, body) {
            if (err) {
                console.log(err);
                assert.fail(err);
            }
            var result = htmlParser.getAbstract(body);
            assert.include(result, 'Formats <code>date</code> to a string based on the requested <code>format</code>');
            assert.notInclude(result, '<code>null');
            assert.include(result, '<pre><code>{{ date_expression | date : format : timezone}}');
            done();
        });
    });
    
    it('should get the abstract from $injector html correctly', function(done) {
        fs.readFile($injectorHtml, 'utf-8', function(err, body) {
            if (err) {
                console.log(err);
                assert.fail(err);
            }
            var result = htmlParser.getAbstract(body);
            assert.include(result, ', instantiate types, invoke methods, and load modules.');
            assert.notInclude(result, '<code>null');
            done();
        });
    });
});