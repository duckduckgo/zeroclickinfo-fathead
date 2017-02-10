"use strict";

var fs = require("fs");
var cheerio = require("cheerio");

(function organize() {
	var mongooseData = fs.readFileSync("./download/mongoosejs.html");
	var outputData = getDocs(mongooseData).map(function ($elem) {
		return getDetails($elem);
	}).filter(function (doc) {
		if (doc) return doc;
	}).map(function (doc) {
		return makeOutput(doc);
	}).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput(_ref) {
	var article = _ref.article,
	    url = _ref.url,
	    abstract = _ref.abstract;

	return [article + "\tA\t\t\t\t\t\t\t\t\thttp://mongodb-tools.com/img/mongoose.png\t" + abstract + "\t" + url + "\n"].concat(getRedirects(article)).join("");
}

function getRedirects(article) {
	var boilerPlate = "\tR\t" + article + "\t\t\t\t\t\t\t\t\t\t\n";
	var redirects = [];
	if (article.indexOf("-") !== -1) {
		redirects.push(article.replace(/-/g, "."), article.replace(/-/g, " "));
	} else if (article.indexOf(".") !== -1) {
		redirects.push(article.replace(/\./g, " "));
	}
	return redirects.map(function (current) {
		return current + boilerPlate;
	});
}

function getDocs(mongooseData) {
	var methods = [];
	var $ = cheerio.load(mongooseData);
	var $docs = $("#content ul .module"); //All the docs

	var _loop = function _loop(i) {
		var $elem = $docs.eq(i).children(".item.public"); //div containing the methods
		$elem.map(function (index) {
			return methods.push($elem.eq(index));
		}); //method
	};

	for (var i = 0; i < $docs.length; i += 1) {
		_loop(i);
	}
	return methods;
}

function getDetails($elem) {
	var article = $elem.children("h3").attr("id");
	if (!article || article.indexOf("%20") !== -1) return null;
	article = article.slice(article.lastIndexOf("_") + 1, article.length).replace(/%24/g, "$");
	var url = "http://mongoosejs.com/docs/api.html#" + $elem.children("h3").attr("id");
	var abstract = makeAbstract($elem);
	return { article: article, url: url, abstract: abstract };
}

function formatDom(desc) {
	//Replaces all the <h4> tag and removes <a> tag replacing it with it's title
	if (!desc) return;
	return desc.replace(/<\s*a[^>]*>/ig, "").replace(/<\s*\/\s*a>/ig, "").replace(/<h4>/g, "<span class=\"prog__sub\">").replace(/<\/h4>/g, "</span>");
}

function formatTitle(title) {
	return title.replace(/<code>|<\/code>/g, "");
}

function formatDesc($elem) {
	var $desc = $elem.children(".description");
	if (!$desc.text()) return null;
	var descCode = $desc.children("pre").text().replace(/\n/g, "\\\\n");
	$desc.children("pre").remove();
	var descBody = $desc.html();
	return descBody + ("<pre><code>" + descCode + "</code></pre>");
}

function formatArg($elem, className) {
	var arg = $elem.children(className);
	if (!arg.text()) return null;
	var title = "<span class=\"prog__sub\">" + arg.children("h4").text() + "</span>";
	var body = "<ul>";
	for (var i = 0; i < arg.children("ul").children("li").length; i += 1) {
		body += "<li>" + arg.children("ul").children("li").eq(i).text() + "</li>\n";
	}
	return "" + title + body + "</ul>";
}

function makeAbstract($elem) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		title: "<pre><code>" + formatTitle($elem.children("h3").html()) + "</code></pre>",
		summary: "<p>" + $elem.children("p").eq(0).html() + "</p>",
		description: formatDesc($elem),
		events: $elem.children(".events").html(),
		args: formatArg($elem, ".params"),
		returns: formatArg($elem, ".returns"),
		endDom: "</section>"
	};
	//Abstract is not consistent, so sometimes abastractData has empty properties
	return Object.keys(abstractData).map(function (prop) {
		return abstractData[prop] ? formatDom(abstractData[prop]) : "";
	}).join("").replace(/\t/g, '    ').replace(/\n/g, "\\n");
}
