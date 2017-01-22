// #!/usr/bin/env node
"use strict";
const fs = require('fs');
const dataSource = './downloads/module';

function extractAbstract($codeTitle) {
    let result = [];
    let $temp = $codeTitle.next();
    let tempTag = $temp[0].tagName;
    while (tempTag === 'P' || tempTag === 'PRE') {
        result.push($temp[0].outerHTML);
        $temp = $temp.next();
        tempTag = $temp[0].tagName;
    }
    return convertAbstrctToHtml(result);
}
function convertAbstrctToHtml(arrayOfTags) {
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

require("jsdom").env({
    file:dataSource,
    done: function(err, window) {
        if (err) {
            console.error(err);
            return;
        }

        let $ = require("jquery")(window);
        let results = [];
        $('h2.header code').parents('h2.header').map((i,d)=>{
            let $d = $(d);
            let title = extractTitle($d);
            let url = extractUrl($d);
            let abstractText = extractAbstract($d);

            let abstract = new Abstract(title, url, abstractText);
            results.push(abstract);
        });

        console.log(results);

        results = results.map((abstrct) => {
            return abstrct.formatForOutput();
        });

        let writer = new outputWriter();
        results.map((outputLine) => {
            writer.write(outputLine);
        });
    }
});

// A class that represent the Abstract (a line in the output.txt)
class Abstract {
    constructor(title, url, abstract, image, disambiguation, externalLinks, related, categories, type = 'A') {
        this.title = title;
        this.type = type;
        this.categories = categories;
        this.related = related;
        this.externalLinks = externalLinks;
        this.seeAlso="";
        this.disambiguation = disambiguation;
        this.image = image;
        this.abstract = abstract;
        this.url = url;
    }

    formatForOutput() {
        return `${this.title}\t${this.type}\t\t\t${this.categories}\t\t${this.seeAlso}\t\t${this.externalLinks}\t${this.disambiguation}\t${this.images}\t${this.abstract}\t${this.url}\n`;
    }
}

class outputWriter {
    constructor(path = './output.txt') {
        this.path = path
    }

    write(data) {
        fs.appendFileSync(this.path,data,{encoding: 'utf8'})
    }
}