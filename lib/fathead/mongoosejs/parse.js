"use strict";

var _slicedToArray2 = require("babel-runtime/helpers/slicedToArray");

var _slicedToArray3 = _interopRequireDefault(_slicedToArray2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

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
	return replaceAnchorWithTitle(desc).replace(/<h4>/g, "<span class=\"prog__sub\">").replace(/<\/h4>/g, "</span>");
}

function makeAbstract($elem) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		title: "<pre><code>" + $elem.children("h3").html() + "</code></pre>",
		summary: "<p>" + $elem.children("p").eq(0).html() + "</p>",
		description: $elem.children(".description").html(),
		events: $elem.children(".events").html(),
		args: $elem.children(".params").html(),
		returns: formatReturns($elem),
		endDom: "</section>"
	};
	//Abstract is not consistent, so sometimes abastractData has empty properties
	return Object.keys(abstractData).map(function (prop) {
		return abstractData[prop] ? formatDom(abstractData[prop]) : "";
	}).join("").replace(/\t/g, '    ').replace(/\n/g, "\\n");
}

function formatReturns($elem) {
	return $elem.children(".returns").html() ? "<pre>" + $elem.children(".returns").html() + "</pre>" : null;
}

function getAnchorTitle(anchorData, _ref2) {
	var _ref3 = (0, _slicedToArray3.default)(_ref2, 2),
	    startInd = _ref3[0],
	    endInd = _ref3[1];

	var onlyAnchorText = anchorData.slice(startInd, endInd);
	var tagEnd = onlyAnchorText.indexOf("\">") + 2; //just to make sure
	return onlyAnchorText.slice(tagEnd, endInd);
}

function replaceAnchorWithTitle(anchorData) {
	//Removing <a> tag while taking out the text between the <a> tag.
	while (anchorData.indexOf("<a") !== -1) {
		var _ref4 = [anchorData.indexOf("<a"), anchorData.indexOf("</a>")],
		    startInd = _ref4[0],
		    endInd = _ref4[1];

		var anchorTitle = getAnchorTitle(anchorData, [startInd, endInd]);
		anchorData = anchorData.slice(0, startInd) + anchorTitle + anchorData.slice(endInd + 4, anchorData.length);
	}
	return anchorData;
}
