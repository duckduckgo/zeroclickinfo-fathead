#!/usr/bin/env node
(function() {
    'use strict';

    const debug = false;
    
    const fs = require('graceful-fs');
    const jsdom = require('jsdom');
    const chalk = require('chalk');

    const DOWNLOAD_DIR = 'download';
    const DOC_DIR = DOWNLOAD_DIR;

    const BASE_URL = 'http://docs.meteor.com/';

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
        
        const pattern = /(^[^\.]+)\_(.+)\.html$/;
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
    function parserFunction(type, key, fileName) {
        if(type == 'api') {
            if(debug) console.log('parsing: '+chalk.blue(type)+' '+chalk.cyan(key)+' '+chalk.green(fileName));
            parserAbstractFunction(type, fileName);
        }
    };

    function parserAbstractFunction(type, fileName) {

        var fileContent = fs.readFileSync(DOC_DIR + '/' + fileName, {encoding: "utf-8"});

        jsdom.env(fileContent, (err, window) => {

            if (err) {
                console.error(err)
                return;
            }
            
            const title = window.document.querySelector('.title-page');
            const methods = window.document.querySelectorAll('.api');
            
            if(debug) console.log(chalk.cyan(title.textContent), methods.length);
            const none = {textContent : ''}
            
            for(var i=0;i<methods.length;i++){
                let method = methods[i];
                let name = method.querySelector('.api-heading .title-api a.primary') || none;
                let desc = method.querySelector('.api-body .desc') || none;
                let code = method.querySelector('.api-heading .subtext-api .code') || none;
                let usage = method.querySelector('.api-heading .locus') || none;
                
                let mask = name.href.replace('about:blank#','').split('-').join(' ');
                if(debug) console.log(chalk.magenta(mask));
                
                // add a debugger here
                let abstract = '';
                abstract += '<section class="prog__container">';
                abstract +=   '<p>' + desc.textContent.trim().replace(/[\r\n]+/g,'\\n') + '</p>';
                abstract +=   '<pre><code>';
                abstract +=     name.textContent.replace(/\s+/g,' ').trim().replace(/[\r\n]+/g,'\\\\n');
                abstract +=    '</code></pre>';
                abstract +=   '<span class="prog__sub">Usage: ' + usage.textContent.trim().replace(/[\r\n]+/g,'\\n') + '</span>';
                abstract +=   '<p>Found in ' + title.textContent.trim().replace(/[\r\n]+/g,'\\n') + '</p>';
                abstract += '</section>';   
                
                addItem({
                    name: mask.trim().replace(/[\r\n]+/g,'\\n'),
                    abstract: abstract,
                    url: getURL(type, fileName, name)
                });
            }
            
            window.close();
        });
    }

    function getURL(type, fileName, name){

        let url = BASE_URL;
        let regex = new RegExp('^'+type+'_', 'g');

        url += type;
        url += '/';
        url += fileName.replace(regex,'');
        url += name.href.replace('about:blank','');

        return url;

    }

    // CODE INITIALIZAITION
    main();
    //
})();

