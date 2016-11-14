var fs = require('fs')
    json = require('./download/search-data.json'),
    async = require('async'),
    htmlParser = require('./lib/htmlParser.js');

var ANGULARAPI = 'https://docs.angularjs.org/partials/';
var jsonOutput = [];

var exclude = ['auto', 'ng'];
var asyncTasks = [];

json.forEach(function(item) {
   
    if (item.path.indexOf('api/') === 0 && exclude.indexOf(item.titleWords) === -1)
        asyncTasks.push(loadAndParse);

    function loadAndParse(callback) {   
        fs.readFile("download/" + item.path + '.html', 'utf-8', function(err, body) {
            if (err) {
                console.log(err);
                return callback();
            }
            
            console.log('parsing ' + item.path + '...');
            parse(item, body);
            console.log('done parsing ' + item.path);
            callback();
        });
    }

    function parse(item, body) {
        var parsedResponse = {
            title: getTitle(item.titleWords, item.path),
            type: 'A',
            redirect: '',
            otherUses: '',
            categories: htmlParser.getCategory(body),
            references: '',
            seeAlso: '',
            furtherReading: '',
            externalLinks: '',
            disambiguation: '',
            images: '',
            abstract: htmlParser.getAbstract(body),
            sourceUrl: 'https://docs.angularjs.org/' + item.path
        };
        jsonOutput.push(parsedResponse);
        Array.prototype.push.apply(
            jsonOutput, 
            getRedirects(parsedResponse.title, item.path).map(function(redirect) {
                return {
                    title: redirect,
                    type: 'R',
                    redirect: parsedResponse.title
                }
            }));
        
    }
});

async.parallel(asyncTasks, function() {
    console.log('');
    console.log('Now, writing output...');
    writeOutput();
});

function writeOutput() {
    var outputText = jsonOutput
    .filter(function(item) { return item.abstract !== ''; })
    .map(function(item) {
        return item.title + '\t' +
          item.type + '\t' +
          item.redirect + '\t' +
          (item.otherUses || '') + '\t' +
          (item.categories || '') + '\t' +
          (item.references || '') + '\t' +
          (item.seeAlso || '') + '\t' +
          (item.furtherReading || '') + '\t' +
          (item.externalLinks || '') + '\t' +
          (item.disambiguation || '') + '\t' +
          (item.images || '') + '\t' +
          (item.abstract || '') + '\t' +
          (item.sourceUrl || '')
    })
    .join('\n');
    
    fs.writeFile('output.txt', outputText, function(err) {
       if (err)
           console.log('Not possible to save file output.txt: ' + err);
       else
           console.log('Done!');
    });
}

function getTitle(titleWords, path) {
    var title = removeAngular(titleWords.split(' ')[0]);
    var exists = jsonOutput.some(function(item) {
        return item.title === title;
    });
    if (exists)
        return getModule(path).toLowerCase() + ' ' + title;
    else
        return title;
}

function getRedirects(title, path) {
    var redirectFns = [
        splitDotedTitle,
        remove$,
        [splitDotedTitle, splitCamelCase],
        [remove$, splitDotedTitle, splitCamelCase],
        splitCamelCaseForDirectives
    ];
    var redirects = [];
    
    redirectFns.forEach(function(fn) {
        var name = title        ;
        if (Array.isArray(fn))
            for(var i = 0; i < fn.length; i++)
                name = fn[i](name, path);
        else
            name = fn(title, path);
        var exists = redirects.some(function(redirect) {
            return redirect === name;
        }) || title === name ||
            jsonOutput.some(function(item) {
               return item.title === name;
            });
        if (!exists)
            redirects.push(name);
    });
    return redirects;
}

function removeAngular(title) {
    var reg = /angular[\.]{0,1}/g;
    return title.replace(reg, '');
}

function remove$(title) {
    var reg = /\$/g;
    return title.replace(reg, '');
}

function splitCamelCase(title) {
    var titleSplitted = [];
    title.split(' ').forEach(function(word) {
        var reg = /([a-z0-9])([A-Z])/g;
        var wordSplitted = word.replace(reg, '$1 $2').split(' ');
        for(var i = 1; i < wordSplitted.length; i++)
            wordSplitted[i] = wordSplitted[i].toLowerCase();
        if (titleSplitted.indexOf(wordSplitted.join(' ')) === -1)
            titleSplitted.push(wordSplitted.join(' '));
    });
    return titleSplitted.join(' ');
}

function splitCamelCaseForDirectives(title, path) {
    if (path.indexOf('directive') === -1)
        return title;
    var titleSplitted = [];
    title.split(' ').forEach(function(word) {
        var reg = /([a-z0-9])([A-Z])/g;
        var wordSplitted = word.replace(reg, '$1-$2').split('-');
        for(var i = 1; i < wordSplitted.length; i++)
            wordSplitted[i] = wordSplitted[i].toLowerCase();
        if (titleSplitted.indexOf(wordSplitted.join('-')) === -1)
            titleSplitted.push(wordSplitted.join('-'));
    });
    return titleSplitted.join(' ');
}

function splitDotedTitle(title) {
    var reg = /\./g;
    return title.replace(reg, ' ');
}

function getModule(path) {
    var reg = /\/[a-zA-Z0-9]+/;
    return path.match(reg)[0].replace(/\//g, '');
}