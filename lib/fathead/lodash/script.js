"use strict";

const cheerio = require("cheerio");
const fs = require("fs");
const formattedData = [];

fs.readFile("./download/lodash.html", "utf-8", (err, data) => {
    if(err) throw err;
    const $ = cheerio.load(data);
    let $docContainer = $(".doc-container");
    for(let i = 0; i < $docContainer.children("div").length - 2; i+=1) {
        let $currElem = $docContainer.children("div").eq(i);
        for(let k = 0; k < $currElem.children("div").length; k+=1) {
            let $innerCurrElem = $currElem.children("div").eq(k);
            let {article, url, abstract} = getDetails($innerCurrElem);
            formattedData.push(`._${article}()\tA\t\t\t\t\t\t\t\thttps://www.drupal.org/files/project-images/logo%20(2).png\t${abstract}\t${url}\n`);
            formattedData.push(`${article}\tR\t._${article}()\t\t\t\t\t\t\t\t\t\n`);
        }
    }
    let joinedString = "";
    formattedData.map(currElem => joinedString += currElem);
    fs.writeFile("./output.txt", joinedString, err => {
        if(err) throw err;
    });
});

function getDetails($element) {
    let article = $element.children("h3").attr("id");
    let url = `https://lodash.com/docs/4.17.4#${article}`;
    let summary = $element.children("p").eq(1).text();
    let abstract = `<section class="prog__container">
        <p>${summary}</p>
        <pre>${$element.children("h3").children("code")}</pre>
        <span class="prog__sub">Arguments</span>
        <pre>${$element.children("ol")}</pre>
        <span class="prog__sub">Returns</span>
        <pre>${$element.children("p").eq(3)}</pre>
        <span class="prog__sub">Example</span>
        ${$element.children(".highlight").html()}
    </section>`.replace(/\t/g,  '    ').replace(/\n/g, "\\n");
    return {article, url, abstract};
}
