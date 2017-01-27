"use strict";

const cheerio = require("cheerio");
const fs = require("fs");
const formattedData = [];

fs.readFile("./download/lodash.html", "utf-8", (err, lodashDoc) => {
    if(err) throw err;
    const $ = cheerio.load(lodashDoc);
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

function getDetails($elem) {
    let article = $elem.children("h3").attr("id");
    let url = `https://lodash.com/docs/4.17.4#${article}`;
    let abstractData = {
        title: $elem.children("h3").children("code"),
        summary: $elem.children("p").eq(1).text(),
        args: $elem.children("ol"),
        returns: $elem.children("p").eq(3),
        example: $elem.children(".highlight").html()
    }
    let abstract = `<section class="prog__container"><pre>${abstractData.title}</pre><p>${abstractData.summary}</p><span class="prog__sub">Arguments</span><pre>${abstractData.args}</pre><span class="prog__sub">Returns</span><pre>${abstractData.returns}</pre><span class="prog__sub">Example</span>${abstractData.example}</section>`.replace(/\t/g,  '    ').replace(/\n/g, "\\n");
    return {article, url, abstract};
}
