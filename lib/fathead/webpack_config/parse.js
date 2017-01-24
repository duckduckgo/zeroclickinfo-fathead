#!/usr/bin/env node

/*
* This scripts parse one file taken from the first argument.
* It outputs the parsed data to the stdout.
* */

"use strict";
const dataSource = process.argv[2];
const jQuery=require("jquery");

require("jsdom").env({
    file:dataSource,
    done: function(err, window) {
        if (err) {
            console.error(err);
            return;
        }

        let $ = jQuery(window);
        let results = [];
        $('h2.header code').parents('h2.header').map((i,d)=>{
            let $d = $(d);
            let title = extractTitle($d);
            let url = extractUrl($d);
            let abstractText = extractAbstract($d);

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

// A class that represent the Abstract (a line in the output.txt)
class Abstract {
    constructor(title, url, abstract, image, disambiguation, externalLinks, related, categories, type = 'A') {
        this.title = title || "";
        this.type = type || "";
        this.categories = categories || "";
        this.related = related || "";
        this.externalLinks = externalLinks || "";
        this.seeAlso="" || "";
        this.disambiguation = disambiguation || "";
        this.image = image || "";
        this.abstract = abstract || "";
        this.url = url || "";
    }

    formatForOutput() {
        this._escapingStrings();
        return `${this.title}\t${this.type}\t\t\t${this.categories}\t\t${this.seeAlso}\t\t${this.externalLinks}\t${this.disambiguation}\t${this.image}\t${this.abstract}\t${this.url}\n`;
    }

    _escapingStrings(){
        for (let value in this) {
            this[value] = escapeString(this[value]);
        }
    }
}

function escapeString(string) {
    let patten1 = /\x0A/g;
    let patten2 = /s\\n/g;

    let result = string.replace(patten1, '\\n');
    result = result.replace(patten2, '\\\\n');

    return result;
}

function extractAbstract($codeTitle) {
    let result = [];
    let $temp = $codeTitle.next();
    let tempTag = $temp[0].tagName;
    while (tempTag === 'P' || tempTag === 'PRE') {
        result.push($temp[0].outerHTML);
        $temp = $temp.next();
        if ($temp.length > 0) {
            tempTag = $temp[0].tagName;
        } else {
            tempTag = 'stop_the_loop_there_are_no_more_tags'
        }
    }
    return convertAbstractToHtml(result);
}

function convertAbstractToHtml(arrayOfTags) {
    let result = `<section class="prog__container">`;
    arrayOfTags.map((tag) => {
        result += tag;
    });
    result += `</section>`;
    return result
}

function extractTitle($codeTitle) {
    return $codeTitle.find('code')[0].innerHTML;
}

function extractUrl($codeTitle) {
    //TODO: extracts with file://
    return $codeTitle.find('a')[0].href;
}