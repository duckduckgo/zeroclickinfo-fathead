"use strict";

var fs = require("fs");
var cheerio = require("cheerio");

(function organize() {
	var celeryData = fs.readFileSync("./download/celery.html");
	var outputData = getDocs(celeryData).map(function (doc) {
		return getDetails(doc);
	}).map(function (docs) {
		return makeOutput(docs);
	}).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput(docs) {
	return docs.map(function (_ref) {
		var article = _ref.article,
		    category = _ref.category,
		    abstract = _ref.abstract,
		    url = _ref.url;

		return [article + "\tA\t\t\t" + category + "\t\t\t\t\t\t\t" + abstract + "\t" + url + "\n"].concat(getRedirects(article)).join("");
	}).join("");
}

function getRedirects(article) {
	if (!/\./g.test(article)) return "";
	var redirs = [];
	var newArt = article.slice(article.lastIndexOf(".") + 1, article.length);
	/_/g.test(article) ? redirs.push(newArt.replace(/_/g, " ")) : redirs.push(newArt);
	return redirs.map(function (art) {
		return art + "\tR\t" + article + "\t\t\t\t\t\t\t\t\t\t\n";
	});
}

function getDocs(celeryData) {
	var methods = [];
	var $ = cheerio.load(celeryData);
	var $docs = $("#api-reference").children(".toctree-wrapper").children(".section");
	for (var i = 3; i < $docs.length; i += 1) {
		var $elem = $docs.eq(i);
		if (!$elem.children(".class").html()) continue;
		var id = $elem.attr("id").replace(/-/g, ".");
		methods.push({
			category: id + " Functions",
			baseURL: id + ".html",
			$methods: $elem.children(".class").children("dd").children()
		});
	}
	return methods;
}

function getDetails(_ref2) {
	var category = _ref2.category,
	    baseURL = _ref2.baseURL,
	    $methods = _ref2.$methods;

	var details = [];
	baseURL = "http://docs.celeryproject.org/en/stable/reference/" + baseURL;
	for (var i = 0; i < $methods.length; i += 1) {
		var $elem = $methods.eq(i);
		if (!/method/g.test($elem.attr("class"))) continue;
		var article = $elem.find(".descclassname").text() + $elem.find(".descname").text();
		var url = baseURL + $elem.find(".headerlink").attr("href");
		var abstract = makeAbstract($elem);
		details.push({ article: article, category: category, url: url, abstract: abstract });
	}
	return details;
}

function formatDescription($elem) {
	if (!$elem.html()) return "";
	var desc = [];
	$elem.map(function (i) {
		var attr = $elem.eq(i).attr("class");
		if (attr === "rubric") return desc.push("<span class=\"prog__sub\">" + $elem.eq(i).text() + "</span>");
		desc.push("<p>" + $elem.eq(i).text() + "</p>");
	});
	return desc.join("");
}

function summaryHelper($tr) {
	//If the current row has a list or not
	var $td = $tr.children("td");
	var $ul = $td.find("ul");
	if (!/<\/ul>/.test($tr.html())) return $td.text() ? "<p>" + $td.text() + "</p>" : "";
	return '<ul class="prog__ul">' + $ul.text().split("\n").filter(function (curr) {
		return curr ? curr : null;
	}).map(function (curr) {
		return "<li>" + curr + "</li>";
	}).join("") + "</ul>";
}

function formatSummary($elem) {
	var summary = [];
	for (var i = 0; i < $elem.length; i += 1) {
		//For all the rows in the table body
		var $tr = $elem.eq(i);
		var $th = $tr.children("th");
		var name = !$th.text() ? "" : "<span class=\"prog__sub\">" + $th.text().replace(/:/g, "") + "</span>";
		var body = summaryHelper($tr);
		summary.push(name + "\n" + body);
	}
	return summary.join("");
}

function formatDom(desc) {
	return desc.replace(/<\s*a[^>]*>/ig, "").replace(/<\s*\/\s*a>/ig, "").replace(/\[source\]¶/g, "").replace(/¶/, "").replace(/\n/g, "");
}

function formatCode($elem) {
	if (!$elem.find(".highlight pre").text()) return "";
	return ("<pre><code>" + $elem.find(".highlight pre").text() + "</code></pre>").split("...").join("\n");
}

function makeAbstract($elem) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		title: "<pre><code>" + $elem.find("dt").text() + "</code></pre>",
		description: formatDescription($elem.children("dd").children("p")),
		example: formatCode($elem),
		summary: formatSummary($elem.find("table tbody").children("tr")),
		endDom: "</section>"
	};
	return Object.keys(abstractData).map(function (prop) {
		return abstractData[prop] ? formatDom(abstractData[prop]) : "";
	}).join("").replace(/\t/g, '    ').replace(/\n/g, "\\n");
}
