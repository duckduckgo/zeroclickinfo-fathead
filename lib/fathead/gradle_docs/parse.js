#!/usr/bin/env node
(function() {
    'use strict';

    const fs = require('fs');
    const gfs = require('graceful-fs');
    gfs.gracefulify(fs);
    const cheerio = require("cheerio");
    const async = require("async");
    const urlUtil = require("url");
    const parse_util = require('./parse_util.js');

    const OUTPUT_FILE = 'output.txt'
    const DOWNLOAD_DIR = 'download';
    const DOC_BASE_FILE = DOWNLOAD_DIR + '/gradle_doc_overview.html';
    const BASE_URL = 'https://docs.gradle.org/current/javadoc/';
    const P_DOM_WRAPPER = cheerio('<p></p>');
    const PRE_DOM_WRAPPER = cheerio('<pre></pre>');

    // remove output file before parse
    try {fs.unlinkSync(OUTPUT_FILE)} catch(e) {};

    
     const dupMap = {};
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
            
        ].join("\t")

        fs.appendFile(OUTPUT_FILE, line + "\n", (err) => {
            if (err) console.error('OUTPUT:', err);
        });
    }

    // function which read all urls from overview, then fetch and call parser
    function main() {

        function urlFetcher(allUrls) {
            // limit simultaneous scrape calls to 8.
            async.mapLimit(allUrls, 8, function(url, callback) {
                  parse_util.requestWrapper(BASE_URL + url, false).then(function (data) {
                        console.log("Processing: " + url);
                        parserFunction(url, data);
                        callback(null, url);
                        },function (err) {
                            console.error("%s; %s", err.message, url);
                            console.log("%j", err.res.statusCode);
                            callback(err, url);
                        });
                },function(err, results) {
                    if(err){
                        console.error(err);
                        console.err("Gradle Fathead Processing Errored out..." + new Date());
                        return;
                    }
                    console.log("Processed total urls: " + results.length);
                    console.log("** Gradle Fathead Processing Completed at: " + new Date());
                    
                });
        }

        console.log("** Gradle Fathead Processing Started at: " + new Date());
        let $ = cheerio.load(fs.readFileSync(DOC_BASE_FILE));
        let allUrls = [];
        $('.overviewSummary td.colFirst > a').each(function(i, elem) {
            allUrls[i] = $(this).attr("href");
        });
        console.log("Total urls to process: " + allUrls.length);
        urlFetcher(allUrls);
    }

    // function to parse all php functions
    function parserFunction(url, htmlData) {
        let $ = cheerio.load(htmlData);
        $('li.blockList > table').each(function(i, elem) {
            //console.log($(this).html());
            let children = $(this).children();
            // extract category
            let category = children.eq(0).children().eq(0).text().replace(/Summary|Types/g, '').trim();
            // Process each row of the Category type
            children.eq(2).children().each(function(i, elem) {
                
                // Process Type name and cleanup <T>'sand remove "."
                let nameRowElement = $(this).children().eq(0);
                let nameRowElementText = nameRowElement.text().replace(/<.*>/g, '');
                nameRowElementText = nameRowElementText.split(".").join(" ");
                // extract detail url for the current type
                let nameRowElementShowMoreUrl = nameRowElement.find('a').attr('href');
                nameRowElementShowMoreUrl = BASE_URL + nameRowElementShowMoreUrl.replace(/^(?:\.\.\/)+/, "")
                
                // Removing dup with same class and interface name
                if (dupMap[nameRowElementText]) {
                    nameRowElementText+= url.split("/").slice(0, -1).join(' ');
                }else{
                    dupMap[nameRowElementText] = category;
                }
                // Process details of the category
                let detailRowElement = $(this).children().eq(1).children().eq(0);

                // Code element wrapper
                detailRowElement.find('code').wrap(PRE_DOM_WRAPPER);

                // Update anchor tag with absolute urls
                detailRowElement.find('a').each(function(i, elem) {
                    let hrefVal = $(this).attr('href');
                    if(hrefVal.indexOf('http://') < 0 && hrefVal.indexOf('https://') < 0){
                        hrefVal = hrefVal.replace(/^(?:\.\.\/)+/, "")
                        $(this).attr('href', BASE_URL+hrefVal);
                    }
                });

                //Wrap text nodes with <p>
                detailRowElement.contents().filter(parse_util.isTextNode).wrap(P_DOM_WRAPPER).end();

                let detailNode = "<section class=\"prog__container\">" 
                                 + detailRowElement.html()
                                + "</section>"

                const item = {
                    name: nameRowElementText,
                    abstract: detailNode.replace(/\n/g, ' ').replace(/\\x00/g, '\\\\x00'),
                    categories: category,
                    url: nameRowElementShowMoreUrl
                }
                addItem(item);

            });
        });

    }

    // CODE INITIALIZAITION
    main();
})();
