"use strict";

const cheerio = require("cheerio");
const fs = require("fs");
const formattedData = [];

fs.readFile("./download/lodash.html", "utf-8", (err, data) => {
    if(err) throw err;
    const $ = cheerio.load(data);
    let $docContainer = $(".doc-container");
    for(let i = 0; i < 1; i+=1) {
        let $currElem = $docContainer.children("div").eq(i);
        for(let k = 0; k < 1; k+=1) {
            let $innerCurrElem = $currElem.children("div").eq(k);
            let {article, url, abstract} = getDetails($innerCurrElem);
            formattedData.push(`${article}\tA\t\t\t\t\t\t\t\t\t${abstract}\t${url}`)
        }
    }
    let joinedString = "";
    formattedData.map(currElem => joinedString += currElem);
    fs.writeFile("./output.txt", joinedString);
});

function getDetails($element) {
    let article = $element.children("h3").attr("id");
    let url = `https://lodash.com/docs/4.17.4#${article}`;
    let summary = $element.children("p").eq(1).text();
    let abstract =
    `<section class="prog__container">
        <p>${summary}</p>
        <pre>${$element.children("h3").children("code")}</pre>
        <span class="prog__sub">Arguments</span>
        <pre>${$element.children("ol")}</pre>
        <span class="prog__sub">Returns</span>
        <pre>${$element.children("p").eq(3)}</pre>
        <span class="prog__sub">Example</span>
        ${$element.children(".highlight")}
    </section>`.replace("s/\\n/\\\\n/g");
    return {article, url, abstract};
}
