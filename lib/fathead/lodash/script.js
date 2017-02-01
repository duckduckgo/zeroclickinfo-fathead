"use strict";

const cheerio = require("cheerio");
const fs = require("fs");
const formattedData = [];
let $;

fs.readFile("./download/lodash.html", "utf-8", (err, lodashDoc) => {
    if(err) throw err;
    $ = cheerio.load(lodashDoc);
    let $docContainer = $(".doc-container"); //This contains all the docs
    for(let i = 0; i < $docContainer.children("div").length - 2; i+=1) {
        let $currElem = $docContainer.children("div").eq(i); //current children of the doc
        $currElem.children("div").map(index => formatDataAndPush($currElem.children("div").eq(index))); //Every children in the current children which is a method
    }
    fs.writeFileSync("./output.txt", formattedData.join(''));
});

function formatDataAndPush($elem) {
    let {article, url, abstract} = getDetails($elem);
    formattedData.push(
        `._${article}()\tA\t\t\t\t\t\t\t\t\thttps://www.drupal.org/files/project-images/logo%20(2).png\t${abstract}\t${url}\n`,
        `${article}\tR\t._${article}()\t\t\t\t\t\t\t\t\t\t\n`,
        `._${article}\tR\t._${article}()\t\t\t\t\t\t\t\t\t\t\n`
    );
}

function formatAbstract($elem) {
    $elem.children(".highlight").addClass("pandasarestupid"); //To find the div below
    let example = "<pre><code>";
    $(`.pandasarestupid div`).each((i, $div) => {
        for(let i = 0; i < $div.children.length; i+=1) {
            $div.children[i].name === "span" && $div.children[i].children[0] ? example += $div.children[i].children[0].data : example += $div.children[i].data;
        }
        example += "\n"
    });
    example += "</code>\n</pre>";
    $elem.children(".highlight").removeClass("pandasarestupid");
    return example;
}

function getAnchorTitle(anchorText) {
    let startInd = anchorText.indexOf("<code>");
    let endInd = anchorText.indexOf("</code>");
    return anchorText.slice(startInd + 6, endInd);
}

function formatSummary(summary) {
    while(summary.indexOf("<a") !== -1) { //For all <a> occurences
        let startInd = summary.indexOf("<a");
        let endInd = summary.indexOf("</a>");
        let titleForA = getAnchorTitle(summary.slice(startInd, endInd));
        summary = `${summary.slice(0, startInd - 1)} ${titleForA} ${summary.slice(endInd + 5, summary.length)}`;
    }
    return summary;
}

function getDetails($elem) {
    let article = $elem.children("h3").attr("id");
    let url = `https://lodash.com/docs/4.17.4#${article}`;
    let abstractData = {
        title: $elem.children("h3").children("code"),
        summary: formatSummary($elem.children("p").eq(1).html()),
        args: $elem.children("ol"),
        returns: $elem.children("p").eq(3),
        example: formatAbstract($elem)
    }
    let abstract = `<section class="prog__container"><pre>${abstractData.title}</pre><p>${abstractData.summary}</p><span class="prog__sub">Arguments</span><pre>${abstractData.args}</pre><span class="prog__sub">Returns</span><pre>${abstractData.returns}</pre><span class="prog__sub">Example</span>${abstractData.example}</section>`.replace(/\t/g,  '    ').replace(/\n/g, "\\n");
    return {article, url, abstract};
}
