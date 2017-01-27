#!/usr/bin/env node

/*
 * This scripts parse one file taken as the first argument (e.g: ./parse.js FILE_TO_PARSE_NAME).
 * It outputs the parsed data to the stdout.
 * */

"use strict";
const dataSource = process.argv[2];
const jQuery = require("jquery");

require("jsdom").env({
    file: dataSource,
    url: generateTheOriginalUrl(dataSource),
    done: function (err, window) {
        if (err) {
            console.error(err);
            return;
        }

        let $ = jQuery(window);
        let results = [];
        $('h2.header code').parents('h2.header').map((index, titleElement) => {
            let $titleElement = $(titleElement);
            let title = extractTitle($titleElement);
            let url = extractUrl($titleElement);
            let abstractText = extractAbstract($titleElement);

            let abstract = new Abstract(title, url, abstractText);
            results.push(abstract);
        });


        let output = "";

        results = results.map((abstract) => {
            output += abstract.formatForOutput();
        });

        console.log(output);
    }
});

function generateTheOriginalUrl(dataSource) {
    const webpackSiteUrl = 'https://webpack.js.org/configuration/';
    let result = dataSource.replace(/\.\/downloads\//, webpackSiteUrl);
    return result;
}

function extractTitle($codeTitle) {
    return $codeTitle.find('code').first().text();
}

function extractUrl($codeTitle) {
    return $codeTitle.find('a')[0].href;
}


/*
*  grabbing all the elements that are either 'p' or 'pre' that are the next sibling of the title
* */
function extractAbstract(titleElement) {
    let result = [];
    let $temp = titleElement.next();
    let tempTag = $temp[0].tagName;
    while (tempTag === 'P' || tempTag === 'PRE') {
        if (tempTag === 'P') {
            escapeLinksInsideTheParagraph($temp);
        }
        result.push($temp[0].outerHTML);
        $temp = $temp.next();
        if ($temp.length > 0) {
            tempTag = $temp[0].tagName;
        } else {
            break;
        }
    }
    return convertAbstractToHtml(result);
}

function escapeLinksInsideTheParagraph($p) {
    let links = $p.find('a');
    links.map((index, element) => {
        /*
        * the html has a relative path, where as the property .href has the full path
        * (the one injected to the document by jsdom combined with the relative path)
        * this line of code makes the full path to be presented on the html and than
        * when parsing it, the link will be the right one (and not duckduckgo/relative_path)
        */
        element.href = element.href;
    });
}

function convertAbstractToHtml(arrayOfTags) {
    let result = `<section class="prog__container">`;
    arrayOfTags.map((tag) => {
        result += tag;
    });
    result += `</section>`;
    return result
}


// A class that represent the Abstract (a line in the output.txt)
class Abstract {
    constructor(title, url, abstract, type = 'A') {
        this.title = title;
        this.type = type;
        this.categories = "";
        this.related = "";
        this.externalLinks = "";
        this.seeAlso = "";
        this.disambiguation = "";
        this.image = "";
        this.abstract = abstract;
        this.url = url;
    }

    formatForOutput() {
        this._escapingStrings();
        return `${this.title}\t${this.type}\t\t\t${this.categories}\t\t${this.seeAlso}\t\t${this.externalLinks}\t${this.disambiguation}\t${this.image}\t${this.abstract}\t${this.url}\n`;
    }

    /*
    *  sending all the properties if this class to the escapeString method
    * */
    _escapingStrings() {
        for (let value in this) {
            this[value] = escapeString(this[value]);
        }
    }
}

function escapeString(string) {
    //the hex 'OA' is the value of Windows new line character that for some reason was in the data source.
    let patten1 = /\x0A/g;
    let patten2 = /s\\n/g;

    let result = string.replace(patten1, '\\n');
    result = result.replace(patten2, '\\\\n');

    return result;
}

