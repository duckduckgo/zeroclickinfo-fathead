#!/usr/bin/env node
(function() {
    'use strict';

    const fs = require('fs');
    const gfs = require('graceful-fs');
    gfs.gracefulify(fs);
    const jsdom = require('jsdom');

    const DOWNLOAD_DIR = 'download';
    const DOC_DIR = DOWNLOAD_DIR + '/phpdoc';

    const BASE_URL = 'http://php.net/manual/en/';

    const OUTPUT_FILE = 'output.txt'

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
            item.url,
            "\n"
        ].join("\t")

        fs.appendFile(OUTPUT_FILE, line, (err) => {
            if (err) console.error('OUTPUT:', err);
        });
    }

    // function which read all files from phpdoc and call it parser
    function main() {

        function fileIterator(file) {

            if (ignoredFiles.indexOf(file) > -1) return;

            const pattern = /(^.*)\.(.*)\.html$/;
            const matches = file.match(pattern);
            const type = matches[1] || '';
            const name = matches[2] || '';

            // parse documentation
            if (parsers[type]) {
                parsers[type](name, file);
            }
        }

        fs.readdir(DOC_DIR, (err, files) => {

            if (err) {
                console.error('DIRECTORY LIST: ', err);
                return;
            }

            files.forEach(fileIterator);
        });
    }

    // function to parse all php functions
    function parserFunction(key, fileName) {

        jsdom.env(DOC_DIR + '/' + fileName, (err, window) => {

            if (err) {
                console.error(err)
                return;
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

            const item = {
                name: name.textContent.trim() || key.replace(/\-/g, '_'),
                abstract: abstract.replace(/\n/g, '').replace(/[\x00]/g, '\\x00'),
                url: BASE_URL + 'function.' + key + '.php'
            };

            addItem(item);
        });
    }

    // CODE INITIALIZAITION
    main();
    //

    // object mapping all parsers available
    const parsers = {
        "function": parserFunction
    }

//============================================================================//
//                                OTHER STUFFS                                //
//============================================================================//

    const ignoredFiles = [
        'images',
        'about.html',
        'aliases.html',
        'appendices.html',
        'configuration.html',
        'configure.html',
        'context.html',
        'copyright.html',
        'debugger-about.html',
        'debugger.html',
        'extensions.html',
        'faq.html',
        'features.html',
        'filters.html',
        'funcref.html',
        'getting-started.html',
        'gupnp-service-proxy-send-action.html',
        'history.html',
        'index.html',
        'indexes.html',
        'ini.html',
        'install.html',
        'internals2.html',
        'intro-whatcando.html',
        'intro-whatis.html',
        'introduction.html',
        'langref.html',
        'manual.html',
        'migration5.html',
        'migration51.html',
        'migration52.html',
        'migration53.html',
        'migration54.html',
        'migration55.html',
        'migration56.html',
        'migration70.html',
        'migration71.html',
        'mysql.html',
        'oop4.html',
        'preface.html',
        'reserved.html',
        'resource.html',
        'search-description.json',
        'search-index.json',
        'security.html',
        'timezones.html',
        'tokens.html',
        'transports.html',
        'tutorial.html',
        'userlandnaming.html',
        'wrappers.html'
    ]

})();
