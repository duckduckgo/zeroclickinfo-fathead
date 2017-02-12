"use strict";

var fs = require("fs");
var cheerio = require("cheerio");

(function organize() {
	var mongooseData = fs.readFileSync("./download/mongoosejs.html");
	var outputData = getDocs(mongooseData).map(function ($elem) {
		return getDetails($elem);
	}).filter(function (doc) {
		if (doc) return doc;
	});
	var disambiguations = getDisambiguations(outputData);
	outputData = outputData.map(function (doc) {
		return makeOutput(doc);
	}).join("");
	fs.writeFileSync("./output.txt", outputData + disambiguations);
})();

function getDisambiguations(docs) {
	var similar = [];
	var alreadyWritten = []; //For duplicates
	docs.map(function (_ref) {
		var article = _ref.article;

		var findThis = article.slice(article.lastIndexOf("-") + 1, article.length); //Extract article (Mongoose-Aggeragate -> Aggeragate)
		similar.push(findDisambiguations(docs, findThis, alreadyWritten));
		alreadyWritten.push(findThis); //for duplicates
	});
	return similar.map(function (_ref2) {
		var userQuery = _ref2.userQuery,
		    queries = _ref2.queries;
		return userQuery + "\tD\t\t\t\t\t\t\t\t" + queries + "\t\t\t\n";
	}).join("");
}

function findDisambiguations(docs, findThis, alreadyWritten) {
	var tempSimilar = { userQuery: findThis, queries: "*" }; //To get all the queries for the current article
	docs.map(function (_ref3) {
		var article = _ref3.article,
		    disambiguation = _ref3.disambiguation;

		var slicedArt = void 0;
		var reg = new RegExp("^" + findThis + "$", "g");
		if (article.lastIndexOf("-") !== -1) {
			slicedArt = article.slice(article.lastIndexOf("-") + 1, article.length);
		} else if (article.lastIndexOf(".") !== -1) {
			slicedArt = article.slice(article.lastIndexOf(".") + 1, article.length);
		} else {
			slicedArt = article.slice(article.lastIndexOf("=") + 1, article.length);
		}
		if (reg.test(slicedArt) && alreadyWritten.indexOf(findThis) === -1) tempSimilar.queries += "[[" + article + "]], " + disambiguation + "\\n*";
	});
	tempSimilar.queries = tempSimilar.queries.slice(0, tempSimilar.queries.length - 3); //remove last \\n*
	return tempSimilar;
}

function makeOutput(_ref4) {
	var article = _ref4.article,
	    url = _ref4.url,
	    abstract = _ref4.abstract;

	return [article + "\tA\t\t\t\t\t\t\t\t\thttp://mongodb-tools.com/img/mongoose.png\t" + abstract + "\t" + url + "\n"];
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
	if (!article) return null;
	var url = "http://mongoosejs.com/docs/api.html#" + $elem.children("h3").attr("id");
	var abstract = makeAbstract($elem);
	var disambiguation = $elem.children("p").eq(0).text();
	if (!/\.|-/gi.test(article)) article = "=" + article; //for disambiguations
	return { article: article, url: url, abstract: abstract, disambiguation: disambiguation };
}

function formatDom(desc) {
	//Replaces all the <h4> tag and removes <a> tag replacing it with it's title
	if (!desc) return;
	return desc.replace(/<\s*a[^>]*>/ig, "").replace(/<\s*\/\s*a>/ig, "").replace(/<h4>/g, "<span class=\"prog__sub\">").replace(/<\/h4>/g, "</span>");
}

function formatArg($elem, className) {
	var arg = $elem.children(className);
	if (!arg.text()) return null;
	var title = "<span class=\"prog__sub\">" + arg.children("h4").text() + "</span>";
	var body = "<ul>";
	var $argLi = arg.children("ul").children("li");

	var _loop2 = function _loop2(i) {
		var _ref5 = [$argLi.eq(i).children("span"), $argLi.eq(i).children("code")],
		    $span = _ref5[0],
		    $code = _ref5[1];

		body += "<li><p><code>" + $code.text() + "</code>";
		$span.map(function (index) {
			return body += formatArgHelper($span.eq(index).text());
		});
		body += "</p></li>";
	};

	for (var i = 0; i < $argLi.length; i += 1) {
		_loop2(i);
	}
	return "" + title + body + "</ul>";
}

function formatArgHelper(str) {
	//I couldn't figure out a regexp for this
	str = str.split("");
	str[str.indexOf("<")] = "<code>";
	str[str.indexOf(">")] = "</code>";
	return str.join("");
}

function formatSummary($elem) {
	//For methods without any classes, just randomly in the div
	var summary = [];
	for (var i = 0; i < $elem.length; i += 1) {
		//This is kind of ugly
		var ifFound = /^pre|^p|^h4|^ul/g.exec($elem.eq(i)[0].name);
		if (!ifFound) continue;
		if (ifFound[0] === "p") {
			summary.push("<p>" + $elem.eq(i).text() + "</p>");
		} else if (ifFound[0] === "pre") {
			summary.push("<pre><code>" + $elem.eq(i).text() + "</code></pre>");
		} else if (ifFound[0] === "h4") {
			summary.push("<span class=\"prog__sub\">" + $elem.eq(i).text() + "</span>");
		} else {
			summary.push($elem.eq(i).html());
		}
	}
	return summary.join("");
}

function makeAbstract($elem) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		title: "<pre><code>" + $elem.children("h3").text() + "</code></pre>",
		summary: formatSummary($elem.children()),
		events: $elem.children(".events").html(),
		args: formatArg($elem, ".params"),
		returns: formatArg($elem, ".returns"),
		description: formatSummary($elem.children(".description").children()),
		endDom: "</section>"
	};
	//Abstract is not consistent, so sometimes abastractData has empty properties
	return Object.keys(abstractData).map(function (prop) {
		return abstractData[prop] ? formatDom(abstractData[prop]) : "";
	}).join("").replace(/\t/g, '    ').replace(/\n/g, "\\n");
}
