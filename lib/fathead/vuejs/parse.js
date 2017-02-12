"use strict";

var _typeof2 = require("babel-runtime/helpers/typeof");

var _typeof3 = _interopRequireDefault(_typeof2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var fs = require("fs");
var cheerio = require("cheerio");

(function organize() {
	var vueJsData = fs.readFileSync("./download/vuejs.html");
	var outputData = getDocs(vueJsData).map(function (doc) {
		return getDetails(doc);
	}).map(function (doc) {
		return makeOutput(doc);
	}).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput(_ref) {
	var category = _ref.category,
	    article = _ref.article,
	    url = _ref.url,
	    abstract = _ref.abstract;

	return [article + "\tA\t\t\t" + category + "\t\t\t\t\t\thttps://vuejs.org/images/logo.png\t" + abstract + "\t" + url + "\n"].concat(getRedirects(article)).join("");
}

function getRedirects(article) {
	var redirects = [];
	var boilerPlate = "\tR\t\t\t\t\t\t\t\t\t\t\t\n";
	var _ref2 = [article.lastIndexOf("-"), article.lastIndexOf(".")],
	    slash = _ref2[0],
	    dot = _ref2[1];

	if (slash === -1 && dot === -1) return [];
	slash !== -1 ? redirects.push(article.replace(/-/g, " ")) : redirects.push(article.replace(/\./g, " "));
	return redirects.map(function (current) {
		return current + boilerPlate;
	});
}

function getDocs(vueJsData) {
	var methods = [];
	var $ = cheerio.load(vueJsData);
	var $docs = $(".content.api.with-sidebar").children();
	var category = "Global Config";
	for (var i = 3; i < $docs.length - 24; i += 1) {
		if ($docs.eq(i)[0].name === "h2") {
			category = $docs.eq(i).text();
			continue;
		}
		if ($docs.eq(i)[0].name === "h3") methods.push({ category: category, $artElem: $docs.eq(i), $elem: $docs.eq(i + 1) });
	}
	return methods;
}

function getDetails(_ref3) {
	var category = _ref3.category,
	    $artElem = _ref3.$artElem,
	    $elem = _ref3.$elem;

	var article = $artElem.text().replace(/\((.)+\)/g, "");
	var url = "https://vuejs.org/v2/api/#" + article;
	var abstract = makeAbstract($artElem, $elem);
	return { article: article, url: url, abstract: abstract, category: category };
}

function formatDom(doc) {
	if (!doc) return;
	return doc.replace(/<\s*a[^>]*>/ig, "").replace(/<\s*\/\s*a>/ig, "").replace(/<strong>/gi, "<span class=\"prog__sub\">").replace(/<\/strong>/gi, "</span>");
}

function formatSummary($elem) {
	var $li = $elem.children();
	var summary = [];
	$li.map(function (i) {
		var $elem = $li.eq(i);
		var $code = $elem.children(".highlight");
		if ($code.html()) {
			var _ret = function () {
				var $p = $elem.children("p");
				summary.push($p.eq(0).html());
				summary.push("<pre><code>" + $code.text() + "</code></pre>");
				$p.map(function (index) {
					if (index > 0) summary.push($p.eq(index).html());
				});
				return {
					v: void 0
				};
			}();

			if ((typeof _ret === "undefined" ? "undefined" : (0, _typeof3.default)(_ret)) === "object") return _ret.v;
		}
		summary.push($elem.html());
	});
	return summary.map(function (doc) {
		return formatDom(doc);
	}).join("");
}

function makeAbstract($artElem, $elem) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		title: $artElem.text(),
		summary: formatSummary($elem),
		endDom: "</section>"
	};
	return Object.keys(abstractData).map(function (prop) {
		return abstractData[prop];
	}).join("").replace(/\t/g, '    ').replace(/\n/g, "\\n");
}
