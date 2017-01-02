var fs = require('fs')
    json = require('./download/all.json'),

    URL_BASE = 'http://nodejs.org/api/all.html#all_',

    items = [];

var cheerio = require("cheerio");
var urlUtil = require('url');

// Map used for storing the original entries which are processed
var origMap = {};

// Array used for maintaining duplicate key entries, later used for disambiguation
var dupEntries = {};

// Map for unique urls for the articles, seems there are unique key entries but they 
// refer to the same url. 
var dupUrlMap = {};

function createItem(ops) {
    //If item contains any spaces, make it a R item 
    //for the corresponding A, otherwise, add it as a A item
    var currentType = 'article';
    var newUrl = URL_BASE + ops.URL;
    if (!origMap[ops.name]){
         if (ops.name.indexOf(' ') > -1) {
            var redirect = ops.name.replace(' ', '.');
            currentType = 'redirect';
            items.push([
                ops.name,
                'R',
                redirect,
                '','','','','','','','','',
                newUrl
            ].join('\t'));
        } else {
            items.push([
                ops.name,
                'A',
                '','','','','','','','','',
                ops.abstract,
                newUrl
            ].join('\t'));
        }
        // Adding index location in array for the key
        origMap[ops.name] = {
                    loc: items.length - 1,
                    type: currentType,
                    url: newUrl
                };
    }
    // If orig Map already contains the key, add the current entry to a dup Entry array for postProcessing
    // We process only for 'article' types and for unique url dups
    else if (origMap[ops.name].type == 'article' && origMap[ops.name].url != newUrl){
        if(!dupEntries[ops.name]){
            dupEntries[ops.name] = [];
        }
        if(!dupUrlMap[newUrl]){
            dupEntries[ops.name].push([
                    ops.name,
                    'A',
                    '','','','','','','','','',
                    ops.abstract,
                    newUrl
            ]);
            dupUrlMap[newUrl] = true;
        }
    }
}

function getAbstract(obj) {
    var summarizedDesc = obj.desc ? preProcessDescription(obj.desc) : '';
    var a =  summarizedDesc,
        code = '',
        stability = '';
    // strip <a> HTML tags but keep others.
    a = a.replace(/<\s*a[^>]*>/ig, '');
    a = a.replace(/<\s*\/\s*a>/ig, '');
    if (obj.type && obj.type === 'method') {
        code = '<pre><code>' + obj.textRaw + '</code></pre>';
    }

    if (obj.stability) {
        stability = '<p  id="obj.stability">Stability: ' + obj.stability + ' - ' + obj.stabilityText + '</p>';
    }

    a = '<section class="prog__container">' + a + code + stability + '</section>';
    // there's some non-utf8 stuff that postgres doesn't like:
    a = a.replace(/0x00|\\0|\\u0000/g,'');
    
    // would be nice to be able to link these, but the links seem to get stripped out:
    a = a.replace(/\[(.*)\]\[\]/g,'$1');
    
    // there's line breaks and tabs in the code blocks that we want to preserve, but
    // will mess up the process script if you don't escape them:
    a = a.replace(/\n/g,'\\n');
    a = a.replace(/\t/g,'  ');
    return a;
}

// Method used for extracting only the relevant description content.
// The below logic always gets the first <p> tag for a field Name
// Since examples are what users are looking for in a fathead, rest
// of the abstract content just fills in all the <pre> tags (which are)
// example section in nodejs documentation.
// For keeping some contextual info, adding a previous <p> tag for each
// example <pre> tag in the final content, so as not to break the 
// documentation continuity

function preProcessDescription(descContent) {
    var $ = cheerio.load(descContent);
    var sumamrizedContent = "";
    // get the first <p> tag always
    sumamrizedContent = "<p id='obj.desc'>" + $('p').eq(0).html() + "</p>";
    // for each <pre> tag get the prev <p> and then content of <pre>
    $('pre').each(function(i, elem) {
        sumamrizedContent+= "<p>" +  $(this).prev('p').html() + "</p>";
        sumamrizedContent+= "<pre>" + $(this).html() + "</pre>";
    });
    return sumamrizedContent;
}


function getURLId(arr, sigs) {
    var str = arr.map(function(i) { return i.toLowerCase(); }).join('_'),
        params = sigs && sigs[0] && sigs[0].params;

    params && params.forEach(function(p) {
        if (p.name && p.name !== '...') {
            str += '_' + p.name.toLowerCase();
        }
    });

    return str;
}

// Parsing the JSON:

// top level:
//   globals
//   vars
//   methods
//   modules

var sectionKeys = ['globals','vars','modules'],
    childKeys = ['methods','properties','classes','events','classMethods'];

function processNode(node, nodes) {

    // there's some nodes we want to skip because they're just not helpeful and better
    // defined elsewhere:
    if (node.name == 'console' || node.name === 'process' || node.name === 'buffer') {
        if (!node.methods && !node.events && !node.properties) {
            return;
        }
    }

    // if it's a class, start over as the root node:
    if (node && node.type === 'class') {
        nodes = [node];
    } else {
        nodes.push(node);
    }
    
    var names = nodes.map(function(n){ return n.name }),
        abstract = getAbstract(node);

    //Create the plain . item
    //EX: vm.Script.runInThisContext 
    createItem({
        name: names.join('.'),
        abstract: abstract,
        URL: getURLId(names, node.signatures)
    });

    // IF: If names array is len of 1 and contains a .
    // remove dot from item and create said item
    // EX: http.Agent -> http Agent
    //
    // ELSE IF: if names array is greater than one and the first item
    // has a ., we will first add the items as they are spaced
    // EX: [http.Agent, something] -> http.Agent something
    // afterwards, we remove the dot from the first item then add it
    // EX: [http.Agent, something] -> http Agent something
    // 
    // ELSE: If there are multiple items but none have dots
    // EX: [vm, isContext] -> vm isContext
    if (names.length === 1 && names[0].indexOf('.') > -1) {
        names[0] = names[0].replace('.', ' ');

        createItem({
            name: names[0],
            abstract: abstract,
            URL: getURLId(names, node.signatures)
        });
    } else if(names.length > 1 && names[0].indexOf('.')  > -1) {
        createItem({
            name: names.join(' '),
            abstract: abstract,
            URL: getURLId(names, node.signatures)
        });
        
        names[0] = names[0].replace('.', ' ');
        
        createItem({
            name: names.join(' '),
            abstract: abstract,
            URL: getURLId(names, node.signatures)
        });
    } else {
         createItem({
            name: names.join(' '),
            abstract: abstract,
            URL: getURLId(names, node.signatures)
        });
    }
  
    childKeys.forEach(function(childKey) {
        if (node[childKey]) {
            node[childKey].forEach(function(child) {
                processNode(child, [].concat(nodes));
            });
        }
    });
}

sectionKeys.forEach(function(sectionKey) {
    json[sectionKey].forEach(function(node) {
        processNode(node, []);
    });
});

// Post processing for disambiguation
postProcessForDisAmbiguation();

function uniq(items) {
   return Array.from(new Set(items));
}
// save to file:
fs.writeFileSync('output.txt', uniq(items).join('\n') + '\n');

/**
Main method for performing disambiguation.
The basic idea is:
1. Process all dup entries for given key name, 
2. Create 'A' entries for each with key name appended with url fragment, ex: KeyName(fragment)
3. Create a new 'A' for original entry's name with the same above format ex.
4. Update the original entry which we added as 'A' to a 'D'
5. Update the 'D' entry's abstract to point to the newly added 'A''s
**/
function postProcessForDisAmbiguation(){
    for (var dupName in dupEntries) {
        var titleArray = [];
        for (var i = 0; i < dupEntries[dupName].length; i++) {
            var dupEntry = dupEntries[dupName][i];
            var abstract = dupEntry[11];
            var url = dupEntry[12];
            var description = getFirst_P_Element(abstract);
            var finalName = generateDisAmbName(dupName, url)
            var dEntryString = '[[' + finalName + ']] ' + description;
            titleArray.push(dEntryString);
            items.push([
                finalName,
                'A',
                '','','','','','','','','',
                abstract,
                url
            ].join('\t'));
        }
        // Steps 3 to 5 from above description
        processAndGenerateDisAmbiguationEntry(dupName, titleArray);
    }
}


function processAndGenerateDisAmbiguationEntry(dupName, titleArray){
    var originalLocation = origMap[dupName]['loc'];
    var originalEntry = items[originalLocation];
    var origEntries = originalEntry.split('\t');
    var origName = origEntries[0];
    var origAbstract = origEntries[11];
    var origUrl = origEntries[12];
    var finalName = generateDisAmbName(origName, origUrl);
    titleArray.push('[[' + finalName + ']] ' + getFirst_P_Element(origAbstract));

    //Step 4
    insertOrigEntryWithUpdatedKey(finalName, origAbstract, origUrl);

    //Step 5
    convertOrigEntryToA_D_Entry(origEntries, titleArray, originalLocation)
   
}

// Creates a new 'A' for original entry's name with the same above format ex.
function insertOrigEntryWithUpdatedKey(finalName, origAbstract, origUrl){
    items.push([
        finalName,
        'A',
        '','','','','','','','','',
        origAbstract,
        origUrl
    ].join('\t'));
}

//Update the original entry which we added as 'A' to a 'D'
//Update the 'D' entry's abstract to point to the newly added 'A''s
function convertOrigEntryToA_D_Entry(origEntries, titleArray, originalLocation){

    origEntries[1] = 'D'
    origEntries[9] = "*" + titleArray.join('\\n*');
    origEntries[11] = '';
    origEntries[12] = '';
    items[originalLocation] = origEntries.join('\t');
}

// Helper for removing code tags from descriptions for D entries
function getFirst_P_Element(content){
    var $ = cheerio.load(content);
    var localDescription = $('p').eq(0);
    localDescription.find('code').each(function(i, elem) {
        $(this).replaceWith($(this).html());
    });
    return localDescription.html().replace(/\\n|\(|\)/g, ' ');
}

// Helper for generating the 'D' names
function generateDisAmbName(currentName, currentUrl){
    var metaInfo = currentName + '(' + urlUtil.parse(currentUrl).hash.replace('#', '') + ')';
    return metaInfo;
}
