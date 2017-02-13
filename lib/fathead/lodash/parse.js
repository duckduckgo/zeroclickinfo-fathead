"use strict";

var cheerio = require("cheerio");
var fs = require("fs");

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

	return ["._" + article + "()\tA\t\t\t\t" + category + "\t\t\t\t\thttps://www.drupal.org/files/project-images/logo%20(2).png\t" + abstract + "\t" + url + "\n", article + "\tR\t._" + article + "()\t\t\t\t\t\t\t\t\t\t\n", "._" + article + "\tR\t._" + article + "()\t\t\t\t\t\t\t\t\t\t\n"].join("");
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
	return doc.replace(/<\s*a[^>]*>/ig, "").replace(/<\s*a[^>]*>/ig, "");
}

function getAbstract($elem) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		title: "<pre>" + $elem.children("h3").children("code") + "</pre>",
		summary: "<p>" + $elem.children("p").eq(1).html().replace(/<\s*a[^>]*>/ig, "").replace(/<\s*\/\s*a>/ig, "") + "</p>",
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
	//Some of the methods don't contain example
	var $divs = $elem.children(".highlight").find("div");
	if (!$divs) return null;
	var example = ["<span class=\"prog__sub\">Example</span><pre><code>"];
	$divs.map(function (i) {
		return example.push($divs.eq(i).text());
	});
	return example.join("\n") + "</code></pre>";
}
