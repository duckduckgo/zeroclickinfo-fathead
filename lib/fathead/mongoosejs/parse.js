"use strict";

var _extends2 = require("babel-runtime/helpers/extends");

var _extends3 = _interopRequireDefault(_extends2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var fs = require("fs");
var cheerio = require("cheerio");

(function organize() {
	var mongooseData = fs.readFileSync("./download/mongoosejs.html");
	var outputData = getDocs(mongooseData).map(function ($elem) {
		return getDetails($elem);
	}).filter(function (doc) {
		if (doc) return doc;
	});
	var disambiguations = getDisambiguationsAndRedirects(outputData);
	outputData = outputData.map(function (doc) {
		return makeOutput(doc);
	}).join("");
	fs.writeFileSync("./output.txt", outputData + disambiguations);
})();

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
	var category = getCategory(article);
	if (!/\.|-/gi.test(article)) article = "=" + article; //for disambiguations
	return { article: article, url: url, abstract: abstract, disambiguation: disambiguation, category: category };
}

function getCategory(article) {
	var exec = /-|\./g.exec(article);
	return exec ? article.slice(0, exec.index) : article;
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

function makeOutput(_ref) {
	var article = _ref.article,
	    url = _ref.url,
	    abstract = _ref.abstract,
	    category = _ref.category;

	return [article + "\tA\t\t\t" + category + "\t\t\t\t\t\thttp://mongodb-tools.com/img/mongoose.png\t" + abstract + "\t" + url + "\n"];
}

function getDisambiguationsAndRedirects(docs) {
	var similar = [];
	var alreadyWritten = []; //For duplicates
	var redirects = [];
	docs.map(function (_ref2) {
		var article = _ref2.article;

		var exec = /\.|-|=/g.exec(article);
		var findThis = article.slice(exec.index + 1, article.length); //Extract article (Mongoose-Aggeragate -> Aggeragate)
		var disambiguations = findDisambiguations(docs, findThis, alreadyWritten);
		if (disambiguations) {
			/\\n\*/g.test(disambiguations.queries) ? similar.push(disambiguations) : redirects.push({
				userQuery: findThis,
				redirect: disambiguations.queries.slice(3, disambiguations.queries.indexOf("]"))
			});
		}
		alreadyWritten.push(findThis); //for duplicates
	});
	return [similar.map(function (_ref3) {
		var userQuery = _ref3.userQuery,
		    queries = _ref3.queries;
		return userQuery + "\tD\t\t\t\t\t\t\t\t" + queries + "\t\t\t\n";
	}).join(""), redirects.map(function (_ref4) {
		var userQuery = _ref4.userQuery,
		    redirect = _ref4.redirect;
		return userQuery + "\tR\t" + redirect + "\t\t\t\t\t\t\t\t\t\t\n";
	}).join("")].join("");
}

function findDisambiguations(docs, findThis, alreadyWritten) {
	var similar = { userQuery: findThis, queries: "*" }; //To get all the queries for the current article
	docs.map(function (_ref5) {
		var article = _ref5.article,
		    disambiguation = _ref5.disambiguation;

		var reg = new RegExp("^" + findThis + "$", "g");
		var exec = /\.|-|=/g.exec(article);
		var slicedArt = article.slice(exec.index + 1, article.length);
		if (reg.test(slicedArt) && alreadyWritten.indexOf(findThis) === -1) similar.queries += "[[" + article + "]], " + disambiguation + "\\n*";
	});
	return similar.queries.length === 1 ? null : (0, _extends3.default)({}, similar, { queries: similar.queries.slice(0, similar.queries.length - 3) });
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
	var body = "<ul class=\"prog__ul\">";
	var $argLi = arg.children("ul").children("li");

	var _loop2 = function _loop2(i) {
		var _ref6 = [$argLi.eq(i).children("span"), $argLi.eq(i).children("code")],
		    $span = _ref6[0],
		    $code = _ref6[1];

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
		} else if (ifFound[0] === "ul") {
			summary.push("<ul class=\"prog__ul>\"" + $elem.eq(i).html() + "</ul>");
		} else {
			summary.push($elem.eq(i).html());
		}
	}
	return summary.join("");
}
