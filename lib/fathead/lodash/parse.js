"use strict";

var cheerio = require("cheerio");
var fs = require("fs");
var formattedData = [];

fs.readFile("./download/lodash.html", "utf-8", function (err, data) {
    if (err) throw err;
    var $ = cheerio.load(data);
    var $docContainer = $(".doc-container");
    for (var i = 0; i < 1; i += 1) {
        var $currElem = $docContainer.children("div").eq(i);
        for (var k = 0; k < 1; k += 1) {
            var $innerCurrElem = $currElem.children("div").eq(k);

            var _getDetails = getDetails($innerCurrElem),
                article = _getDetails.article,
                url = _getDetails.url,
                abstract = _getDetails.abstract;

            formattedData.push(article + "\tA\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"\t\"\"\t" + abstract + "\t" + url);
        }
    }
    var joinedString = "";
    formattedData.map(function (currElem) {
        return joinedString += currElem;
    });
    fs.writeFile("./output.txt", joinedString);
});

function getDetails($element) {
    var article = $element.children("h3").attr("id");
    var url = "https://lodash.com/docs/4.17.4#" + article;
    var summary = $element.children("p").eq(1).text();
    var abstract = ("\n    <section class=\"prog__container\">\n        <p>" + summary + "</p>\n        <pre>" + $element.children("h3").children("code") + "</pre>\n        <span class=\"prog__sub\">Arguments</span>\n        <pre>" + $element.children("ol") + "</pre>\n        <span class=\"prog__sub\">Returns</span>\n        <pre>" + $element.children("p").eq(3) + "</pre>\n        <span class=\"prog__sub\">Example</span>\n        " + $element.children(".highlight") + "\n    </section>").replace("s/\\n/\\\\n/g");
    return { article: article, url: url, abstract: abstract };
}
