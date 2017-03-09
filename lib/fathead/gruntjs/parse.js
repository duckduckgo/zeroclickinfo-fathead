"use strict";

var _typeof2 = require("babel-runtime/helpers/typeof");

var _typeof3 = _interopRequireDefault(_typeof2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var fs = require("fs");
var cheerio = require("cheerio");

(function organize() {
	var outputData = getFiles().map(function (file) {
		return getDocs(file);
	}).map(function (docs) {
		return getDetails(docs);
	}).map(function (docs) {
		return makeOutput(docs);
	}).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput(docs) {
	return docs.map(function (_ref) {
		var category = _ref.category,
		    url = _ref.url,
		    abstract = _ref.abstract,
		    article = _ref.article;

		return [article + "\tA\t\t\t" + category + "\t\t\t\t\t\t\t" + abstract + "\t" + url + "\n"].concat(getRedirects(article)).join("");
	}).join("");
}

function getRedirects(article) {
	var newArticle = article.replace(/grunt\./g, "");
	return [newArticle + "\tR\t" + article + "\t\t\t\t\t\t\t\t\t\t\n"];
}

function getFiles() {
	var dir = fs.readdirSync("./download");
	return dir.map(function (file) {
		return fs.readFileSync("./download/" + file, "utf-8");
	});
}

function getDocs(file) {
	var methods = [];
	var $ = cheerio.load(file);
	var $docs = $(".hero-unit");
	var baseURL = "http://gruntjs.com/api/" + $docs.find("h1").text();
	var category = $docs.find("h1").text().split(".").map(function (name) {
		return name = name[0].toUpperCase() + name.slice(1);
	}).join(" ") + " Functions";
	for (var i = 3; i < $docs.children().length; i += 1) {
		var $elem = $docs.children().eq(i);
		if (!/h3/g.test($elem[0].name)) continue;
		methods.push({ category: category, $elems: getMethod($docs.children(), i), baseURL: baseURL });
	}
	return methods;
}

//Gets the start(/h3/) of the method -> Outputs the HTML elements between the start and the end(/h3|h2/)
function getMethod($docs, methodStart) {
	var method = [];

	var _loop = function _loop(i) {
		var $elem = $docs.eq(i);
		if (!/h3|h2|div/g.test($elem[0].name)) return "continue";
		$docs.map(function (index) {
			return index >= methodStart && index < i ? method.push($docs.eq(index)) : "";
		});
		return {
			v: method
		};
	};

	for (var i = methodStart + 1; i < $docs.length; i += 1) {
		var _ret = _loop(i);

		switch (_ret) {
			case "continue":
				continue;

			default:
				if ((typeof _ret === "undefined" ? "undefined" : (0, _typeof3.default)(_ret)) === "object") return _ret.v;
		}
	}
}

function getDetails(docs) {
	return docs.map(function (_ref2) {
		var category = _ref2.category,
		    $elems = _ref2.$elems,
		    baseURL = _ref2.baseURL;

		var $anchor = $elems[0].children("a");
		var url = baseURL + $anchor.attr("href");
		var article = $anchor.attr("name");
		var abstract = makeAbstract($elems.splice(1, $elems.length));
		return { category: category, url: url, article: article, abstract: abstract };
	});
}

function formatSummary($elems) {
	var summary = [];
	for (var i = 0; i < $elems.length; i += 1) {
		var $elem = $elems[i];
		var name = $elem[0].name;
		if (name === "p") /example/gi.test($elem.text()) ? summary.push("<span class=\"prog__sub\">" + $elem.text() + "</span>") : summary.push("<p>" + $elem.text() + "</p>");
		if (name === "pre") summary.push("<pre><code>" + $elem.text() + "</code></pre>");
	}
	return summary.join("");
}

function makeAbstract($elems) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		summary: formatSummary($elems),
		endDom: "</section>"
	};
	return Object.keys(abstractData).map(function (prop) {
		return abstractData[prop];
	}).join("").replace(/\t/g, '    ').replace(/\n/g, "\\n");
}
