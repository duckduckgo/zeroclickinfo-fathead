"use strict";

var fs = require("fs");
var cheerio = require("cheerio");

(function organize() {
	var lessData = fs.readFileSync("./download/less.html");
	var outputData = getDocs(lessData).map(function (docs) {
		return getDetails(docs);
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

		return [article + "\tA\t\t\t" + category + "\t\t\t\t\t\thttp://lesscss.org/public/img/logo.png\t" + abstract + "\t" + url + "\n"];
	}).join("");
}

function getDocs(lessData) {
	var methods = [];
	var $ = cheerio.load(lessData);
	var $docs = $(".panel.docs-content").children();
	for (var i = 1; i < $docs.length; i += 1) {
		var $elem = $docs.eq(i).children(".section-content").html();
		var category = $docs.eq(i).find(".page-header h1").text().toLowerCase().replace("\n", "");
		methods.push({ category: category, docs: $elem.split("<h3 class=\"docs-heading\">") });
	}
	return methods;
}

function getDetails(_ref2) {
	var category = _ref2.category,
	    docs = _ref2.docs;

	var details = [];
	docs.forEach(function (doc, index) {
		if (index === 0) return;
		doc = doc.split("\n");
		var article = doc[0].slice(0, doc[0].indexOf("<"));
		var url = "http://lesscss.org/functions/#" + category.replace(" ", "-") + "-" + article;
		var abstract = makeAbstract(doc);
		details.push({ category: category, article: article, url: url, abstract: abstract });
	});
	return details;
}

function formatDom(desc) {
	if (!desc) return;
	return desc.replace(/<\s*a[^>]*>/ig, "").replace(/<\s*\/\s*a>/ig, "");
}

function makeAbstract(doc) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		description: doc[3],
		details: "",
		endDom: "</section>"
	};
	for (var i = 5; i < doc.length; i += 1) {
		if (/<br>|^\s+$|img/g.test(doc[i])) continue;
		if (/:/g.test(doc[i]) && /<\/p>/g.test(doc[i])) {
			doc[i] = doc[i].replace("<p>", "<span class=\"prog__sub\">").replace(":", "</span><p>");
		}
		if (/<ul/g.test(doc[i])) doc[i] = doc[i].replace("<ul", "<ul class=\"prog__ul\"");
		if (/<pre>/g.test(doc[i])) {
			var _formatCode = formatCode(doc, i),
			    code = _formatCode.code,
			    loc = _formatCode.loc; //gets the code and the end location of code in array(doc)


			abstractData.details += code; //add code
			/*
   	Two statements below are used to make the loop skip the code in array because they've been added above.
   	This is done by jumping the loop ahead by changing the value of i
   */
			i = loc - 1; //make loop go to the location just before the end of the code
			doc[i] = ""; //this makes the current value to "" or it will add the last line of code again
		}
		abstractData.details += doc[i];
	}
	return Object.keys(abstractData).map(function (doc) {
		return formatDom(abstractData[doc]);
	}).join("").replace(/\t/g, '    ').replace(/\n/g, "\\n");
}

function formatCode(doc, i) {
	//Extracting code and formmating it and then giving the location of the end of the code in the array
	var preLoc = [i];
	doc.forEach(function (current, index) {
		return (/<\/pre>/g.test(current) && !preLoc[1] && index > i ? preLoc[1] = index : ""
		);
	});
	var code = doc.slice(preLoc[0], preLoc[1]).join("\n").replace(/<span[^>]*>/ig, "").replace(/<*\/*span>/ig, "");
	return { code: code, loc: preLoc[1] };
}
