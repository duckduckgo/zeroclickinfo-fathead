#!/usr/bin/env node
(function() {
    'use strict';

    const debug = false;
    
    const fs = require('graceful-fs');
    const jsdom = require('jsdom');
    const chalk = require('chalk');

    const DOWNLOAD_DIR = 'download';
    const DOC_DIR = DOWNLOAD_DIR + '/phpdoc';

    const BASE_URL = 'http://php.net/manual/en/';

    const OUTPUT_FILE = 'output.txt'

    // mapping required for repeated functions
    var mapping = [];
    
    // remove output file before parse
    try {fs.unlinkSync(OUTPUT_FILE)} catch(e) {};

    function addItem(item) {

        const defaultItem = {
            name: '',
            type: 'A',
            redirect: '',
            ignore1: '',
            categories: '',
            ignore2: '',
            related: '',
            ignore3: '',
            external_links: '',
            disambiguation: '',
            image: '',
            abstract: '',
            url: ''
        }

        for (let key in defaultItem) {
            item[key] = item[key] || defaultItem[key]
        }

        const line = [
            item.name,
            item.type,
            item.redirect,
            item.ignore1,
            item.categories,
            item.ignore2,
            item.related,
            item.ignore3,
            item.external_links,
            item.disambiguation,
            item.image,
            item.abstract,
            item.url
        ].join("\t") + "\n";

        // using sync as async caused too many files open
        fs.appendFileSync(OUTPUT_FILE, line);
    }

    // function which read all files from phpdoc and call it parser
    function main() {
        var files = fs.readdirSync(DOC_DIR);
        files.forEach(fileIterator);
    }

    function fileIterator(file) {
    
        if(debug) console.log('File:'+chalk.grey(file))
        
        const pattern = /(^[^\.]+)\.(.+)\.html$/;
        const matches = file.match(pattern);

        if (matches == null) { 
            if(debug) console.log( chalk.cyan("unmatched - " + file) ); 
            return; 
        }

        const type = matches[1] || '';
        const name = matches[2] || '';

        // parse documentation
        parserFunction(type, name, file);	    
    }

    // function to parse all php functions
    function parserFunction(className, key, fileName) {
        if(debug) console.log('parsing: '+chalk.blue(className)+' '+chalk.cyan(key)+' '+chalk.green(fileName))
        
	    // checking if function already exists, if not also add name without class
	    if(className == 'function') {
            parserAbstractFunction(key, fileName);
            parserAbstractFunction('function.'+key, fileName);
        }
        else if(className != 'class') {
            parserAbstractFunction(className + '.' + key, fileName);
        }
        
        if( typeof mapping[key] == 'undefined' && !fs.existsSync(DOC_DIR + '/function.' + key + '.html') ) {
            mapping[key] = 1;
            parserAbstractFunction(key, fileName); 
        }
        
    }

    function parserAbstractFunction(key, fileName) {

        var fileContent = fs.readFileSync(DOC_DIR + '/' + fileName, {encoding: "utf-8"});

        jsdom.env(fileContent, (err, window) => {

            if (err) {
                console.error(err)
                return;
            }
            
            // finding if a function exists on page or not
            const methods = window.document.getElementsByClassName('methodsynopsis');
            const classes = window.document.getElementsByClassName('classsynopsis');
            
            if( methods.length == 0 || (methods.length > 0 && classes.length > 0) ) {
                if(debug) console.log('[F] Skipping : '+chalk.blue(fileName));
                return window.close()
            }
            
            const none = {textContent : ''}

            const name = window.document.querySelector('.refname') || none;
            const version = window.document.querySelector('.verinfo') || none;
            const title = window.document.querySelector('.dc-title') || none;
            const description = window.document.querySelector('.description .dc-description') || none;

            let abstract = '';

            abstract += '<section class="prog__container">';
            abstract +=   '<p>' + title.textContent.trim() + '</p>';
            abstract +=   '<pre><code>' + description.textContent.trim() + '</code></pre>';
            abstract +=   '<span class="prog__sub">PHP Versions</span>';
            abstract +=   '<p>' + version.textContent.trim() + '</p>';
            abstract += '</section>';

            const itemName = name.textContent.trim();
            if(debug) console.log(chalk.grey(fileName)+' > '+chalk.blue(key) + ' : ' + chalk.magenta(itemName));
            
            const item = {
                name: key,
                abstract: abstract.replace(/\n/g, '').replace(/\\x00/g, '\\\\x00'),
                url: BASE_URL + fileName.replace(/html$/g, 'php')
            };

            addItem(item);

            window.close();
        });
    }

    // CODE INITIALIZAITION
    main();
    //
})();
