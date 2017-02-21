"use strict";

var cheerio = require("cheerio");
var fs = require("fs");
<<<<<<< HEAD

(function organize() {
	var lodashDoc = fs.readFileSync("./download/lodash.html");
	var outputData = getDocs(lodashDoc).map(function ($elem) {
		return getDetails($elem);
	}).map(function (doc) {
		return makeOutput(doc);
	}).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function getDocs(lodashDoc) {
	var docs = [];
	var $ = cheerio.load(lodashDoc);
	var $docContainer = $(".doc-container").children("div");

	var _loop = function _loop(i) {
		var $elem = $docContainer.eq(i);
		var $divs = $elem.children("div");
		var category = $elem.children("h2").text();
		$divs.map(function (index) {
			return docs.push({ category: category, $elem: $divs.eq(index) });
		}); //Every children in the current children which is a method
	};

	for (var i = 0; i < $docContainer.length - 2; i += 1) {
		_loop(i);
	}
	return docs;
}

function makeOutput(_ref) {
	var article = _ref.article,
	    url = _ref.url,
	    abstract = _ref.abstract,
	    category = _ref.category;

	return ["._" + article + "\tA\t\t\t" + category + "\t\t\t\t\t\thttps://www.drupal.org/files/project-images/logo%20(2).png\t" + abstract + "\t" + url + "\n", article + "\tR\t._" + article + "()\t\t\t\t\t\t\t\t\t\t\n"].join("");
}

function getDetails(_ref2) {
	var category = _ref2.category,
	    $elem = _ref2.$elem;

	var article = $elem.children("h3").attr("id");
	var url = "https://lodash.com/docs/4.17.4#" + article;
	var abstract = getAbstract($elem);
	category = category.replace(/”|“/g, "");
	return { article: article, url: url, abstract: abstract, category: category };
}

function formatDom(doc) {
	return doc.replace(/<\s*a[^>]*>/ig, "").replace(/<\s*\/\s*a>/ig, "");
}

function getAbstract($elem) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		title: "<pre>" + $elem.children("h3").children("code") + "</pre>",
		summary: "<p>" + $elem.children("p").eq(1).html() + "</p>",
		args: "<span class=\"prog__sub\">Arguments</span>" + $elem.children("ol"),
		returns: "<span class=\"prog__sub\">Returns</span>" + $elem.children("p").eq(3),
		example: formatExample($elem),
		endDom: "</section>"
	};
	return Object.keys(abstractData).map(function (prop) {
		return abstractData[prop] ? formatDom(abstractData[prop]) : "";
	}).join("").replace(/\t/g, '    ').replace(/\n/g, "\\n");
}

function formatExample($elem) {
	//Some of the methods don't contain an sexample
	var $divs = $elem.children(".highlight").find("div");
	if (!$divs.html()) return null;
	var example = ["<span class=\"prog__sub\">Example</span><pre><code>"];
	$divs.map(function (i) {
		return example.push($divs.eq(i).text());
	});
	return example.join("\n") + "</code></pre>";
=======
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

function getAnchorTitle(anchorText) {
    var startInd = anchorText.indexOf("<code>");
    var endInd = anchorText.indexOf("</code>");
    return anchorText.slice(startInd, endInd + 7);
}

function formatSummary(summary) {
    while (summary.indexOf("<a") !== -1) {
        //For all <a> occurences
        var startInd = summary.indexOf("<a");
        var endInd = summary.indexOf("</a>");
        var titleForA = getAnchorTitle(summary.slice(startInd, endInd));
        summary = summary.slice(0, startInd - 1) + " " + titleForA + " " + summary.slice(endInd + 5, summary.length);
    }
    return summary;
}

function getDetails($elem) {
    var article = $elem.children("h3").attr("id");
    var url = "https://lodash.com/docs/4.17.4#" + article;
    var abstractData = {
        title: $elem.children("h3").children("code"),
        summary: formatSummary($elem.children("p").eq(1).html()),
        args: $elem.children("ol"),
        returns: $elem.children("p").eq(3),
        example: formatAbstract($elem)
    };
    var abstract = "<section class=\"prog__container\">" + ("<pre>" + abstractData.title + "</pre>") + ("<p>" + abstractData.summary + "</p>") + ("<span class=\"prog__sub\">Arguments</span>" + abstractData.args) + ("<span class=\"prog__sub\">Returns</span><pre>" + abstractData.returns + "</pre>") + ("<span class=\"prog__sub\">Example</span>" + abstractData.example + "</section>").replace(/\t/g, '    ').replace(/\n/g, "\\n");
    return { article: article, url: url, abstract: abstract };
>>>>>>> master
}
