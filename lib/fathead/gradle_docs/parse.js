#!/usr/bin/env node
(function() {
    'use strict';

    const fs = require('fs');
    const gfs = require('graceful-fs');
    gfs.gracefulify(fs);
    const cheerio = require("cheerio");
    const request = require("request");
    const async = require("async");
    const Promise = require("promise");
    const DOWNLOAD_DIR = 'download';
    const DOC_BASE_FILE = DOWNLOAD_DIR + '/gradle_doc_overview.html';

    const BASE_URL = 'https://docs.gradle.org/current/javadoc/';
    const ABSTRACT_WRAPPER_DOM = cheerio('<section class="prog__container"></section>')
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

    // function which read all urls from overview, then fetch and call parser
    function main() {
        function urlFetcher(allUrls) {
            console.log(allUrls.length);
            async.map(allUrls, function(url, callback) {
                  requestWrapper(BASE_URL + url, false).then(function (data) {
                        console.log(url);
                        parserFunction(url, data);
                        callback(null, url);
                        },function (err) {
                            console.error("%s; %s", err.message, url);
                            console.log("%j", err.res.statusCode);
                            callback(err, url);
                        });
                },function(err, results) {
                    //TODO add proper completed message and logs here
                    console.log("I am done");
                });
        }

        let $ = cheerio.load(fs.readFileSync(DOC_BASE_FILE));
        let allUrls = [];
        $('.overviewSummary td.colFirst > a').each(function(i, elem) {
            allUrls[i] = $(this).attr("href");
        });
        console.log(allUrls.length);
        urlFetcher(allUrls.splice(1,1))
    }

    function requestWrapper(url, json) {
        json = json || false;
        return new Promise(function (resolve, reject) {
            request({url:url, json:json}, function (err, res, body) {
                if (err) {
                    return reject(err);
                } else if (res.statusCode !== 200) {
                    err = new Error("Unexpected status code: " + res.statusCode);
                    err.res = res;
                    return reject(err);
                }
                resolve(body);
                });
            });
    }

    // function to parse all php functions
    function parserFunction(url, htmlData) {

        let $ = cheerio.load(htmlData);
        $('li.blockList > table').each(function(i, elem) {
            //console.log($(this).html());
            let children = $(this).children();
            let category = children.eq(0).children().eq(0).text().replace('Summary', '').trim();
            children.eq(2).children().each(function(i, elem) {

                let detailNode = "<section class=\"prog__container\">" 
                                    + $(this).children().eq(1).children().eq(0).html()
                                    + "</section>"

                const item = {
                    name: $(this).children().eq(0).text(),
                    abstract: detailNode.replace(/\n/g, ' ').replace(/\\x00/g, '\\\\x00').replace("/..\/..\/..\//g", BASE_URL),
                    categories: category,
                    url: BASE_URL + url
                }
                console.log(item);
            });
        });


        // jsdom.env(DOC_DIR + '/' + fileName, (err, window) => {

        //     if (err) {
        //         console.error(err)
        //         return;
        //     }

        //     const none = {textContent : ''}

        //     const name = window.document.querySelector('.refname') || none;
        //     const version = window.document.querySelector('.verinfo') || none;
        //     const title = window.document.querySelector('.dc-title') || none;
        //     const description = window.document.querySelector('.description .dc-description') || none;

        //     let abstract = '';

        //     abstract += '<section class="prog__container">';
        //     abstract +=   '<p>' + title.textContent.trim() + '</p>';
        //     abstract +=   '<pre><code>' + description.textContent.trim() + '</code></pre>';
        //     abstract +=   '<span class="prog__sub">PHP Versions</span>';
        //     abstract +=   '<p>' + version.textContent.trim() + '</p>';
        //     abstract += '</section>';

        //     const item = {
        //         name: name.textContent.trim() || key.replace(/\-/g, '_'),
        //         abstract: abstract.replace(/\n/g, '').replace(/\\x00/g, '\\\\x00'),
        //         url: BASE_URL + 'function.' + key + '.php'
        //     };

        //     addItem(item);
        // });
    }

    // CODE INITIALIZAITION
    main();
    //

})();
