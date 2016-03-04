var fs = require('fs')
    json = require('./download/all.json'),

    URL_BASE = 'http://nodejs.org/api/all.html#all_',

    items = [];

function createItem(ops) {
    items.push([
        ops.name,
        'A',
        '','','','','','','','','',
        ops.abstract,
        URL_BASE + ops.URL
    ].join('\t'));
}

function getAbstract(obj) {
    var a = obj.desc || '',
        code = '',
        stability = '';

    if (obj.type && obj.type === 'method') {
        code = '<pre><code>' + obj.textRaw + '</code></pre>';
    }

    if (obj.stability) {
        stability = '<p>Stability: ' + obj.stability + ' - ' + obj.stabilityText + '</p>';
    }

    a = code + a + stability;

    // there's some non-utf8 stuff that postgres doesn't like:
    a = a.replace(/0x00|\\0|\\u0000/g,' ');

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

var sectionKeys = ['globals','vars','methods','modules'],
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

    createItem({
        name: names.join('.'),
        abstract: abstract,
        URL: getURLId(names, node.signatures)
    });

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

// save to file:
fs.writeFileSync('output.txt', items.join('\n') + '\n');
