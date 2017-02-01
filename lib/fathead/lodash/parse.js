"use strict";

var cheerio = require("cheerio");
var fs = require("fs");
var formattedData = [];
var $ = void 0;

fs.readFile("./download/lodash.html", "utf-8", function (err, lodashDoc) {
    if (err) throw err;
    $ = cheerio.load(lodashDoc);
    var $docContainer = $(".doc-container"); //This contains all the docs

    var _loop = function _loop(i) {
        var $currElem = $docContainer.children("div").eq(i); //current children of the doc
        $currElem.children("div").map(function (index) {
            return formatDataAndPush($currElem.children("div").eq(index));
        }); //Every children in the current children which is a method
    };

    for (var i = 0; i < $docContainer.children("div").length - 2; i += 1) {
        _loop(i);
    }
    fs.writeFileSync("./output.txt", formattedData.join(''));
});

function formatDataAndPush($elem) {
    var _getDetails = getDetails($elem),
        article = _getDetails.article,
        url = _getDetails.url,
        abstract = _getDetails.abstract;

    formattedData.push("._" + article + "()\tA\t\t\t\t\t\t\t\t\thttps://www.drupal.org/files/project-images/logo%20(2).png\t" + abstract + "\t" + url + "\n", article + "\tR\t._" + article + "()\t\t\t\t\t\t\t\t\t\t\n", "._" + article + "\tR\t._" + article + "()\t\t\t\t\t\t\t\t\t\t\n");
}

function formatAbstract($elem) {
    $elem.children(".highlight").addClass("pandasarestupid"); //To find the div below
    var example = "<pre><code>";
    $(".pandasarestupid div").each(function (i, $div) {
        for (var _i = 0; _i < $div.children.length; _i += 1) {
            $div.children[_i].name === "span" && $div.children[_i].children[0] ? example += $div.children[_i].children[0].data : example += $div.children[_i].data;
        }
        example += "\n";
    });
    example += "</code>\n</pre>";
    $elem.children(".highlight").removeClass("pandasarestupid");
    return example;
}

function getDetails($elem) {
    var article = $elem.children("h3").attr("id");
    var url = "https://lodash.com/docs/4.17.4#" + article;
    var abstractData = {
        title: $elem.children("h3").children("code"),
        summary: $elem.children("p").eq(1).text(),
        args: $elem.children("ol"),
        returns: $elem.children("p").eq(3),
        example: formatAbstract($elem)
    };
    var abstract = ("<section class=\"prog__container\"><pre>" + abstractData.title + "</pre><p>" + abstractData.summary + "</p><span class=\"prog__sub\">Arguments</span><pre>" + abstractData.args + "</pre><span class=\"prog__sub\">Returns</span><pre>" + abstractData.returns + "</pre><span class=\"prog__sub\">Example</span>" + abstractData.example + "</section>").replace(/\t/g, '    ').replace(/\n/g, "\\n");
    return { article: article, url: url, abstract: abstract };
}
