var fs = require('fs')
    json = require('./download/all.json'),

    URL_BASE = 'http://nodejs.org/api/all.html#all_',

    items = [];

function createItem(ops) {
    //If item contains any spaces, make it a R item 
    //for the corresponding A, otherwise, add it as a A item 
    if (ops.name.indexOf(' ') > -1) {
        var redirect = ops.name.replace(' ', '.');

        items.push([
            ops.name,
            'R',
            redirect,
            '','','','','','','','','',
            URL_BASE + ops.URL
        ].join('\t'));
    } else {
        items.push([
            ops.name,
            'A',
            '','','','','','','','','',
            ops.abstract,
            URL_BASE + ops.URL
        ].join('\t'));
    }     
}

function getAbstract(obj) {
    var a = obj.desc ? '<p id="obj.desc">' + obj.desc + '</p>' : '',
        code = '',
        a = a.replace(/<\s*p*>/ig, ''),
        stability = '';
    // strip <a> and <p> HTML tags but keep others.
    a = a.replace(/<\s*\/\s*p>/ig, '');
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

function uniq(items) {
   return Array.from(new Set(items));
}
// save to file:
fs.writeFileSync('output.txt', uniq(items).join('\n') + '\n');
