"use strict";

var _typeof2 = require("babel-runtime/helpers/typeof");

var _typeof3 = _interopRequireDefault(_typeof2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var fs = require("fs");
var cheerio = require("cheerio");

(function organize() {
	var outputData = getFiles().map(function (file) {
		return getDocs(file);
	}).map(function (doc) {
		return getDetails(doc);
	}).map(function (doc) {
		return makeOutput(doc);
	}).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput(docs) {
	return docs.map(function (_ref) {
		var article = _ref.article,
		    url = _ref.url,
		    abstract = _ref.abstract,
		    _ref$category = _ref.category,
		    category = _ref$category === undefined ? "" : _ref$category;
		return [article + "\tA\t\t\t" + category + "\t\t\t\t\t\t\t" + abstract + "\t" + url + "\n"].concat(getRedirects(article)).join("");
	}).join("");
}

function getRedirects(article) {
	var newArticle = article.split(/(?=[A-Z])/g).join(" ");
	return !/\s/g.test(newArticle) ? "" : [newArticle + "\tR\t" + article + "\t\t\t\t\t\t\t\t\t\t\n"];
}

function getFiles() {
	var dir = fs.readdirSync("./download");
	return dir.map(function (file) {
		return { name: file, file: fs.readFileSync("./download/" + file, "utf-8") };
	});
}

function getDocs(_ref2) {
	var name = _ref2.name,
	    file = _ref2.file;

	var methods = [];
	var $ = cheerio.load(file);
	var $docs = $(".normal.markdown-section").children();
	var start = name === "Store.html" ? 7 : 0;
	for (var i = start; i < $docs.length; i += 1) {
		var $elem = $docs.eq(i);
		if (!/h3|h1/g.test($elem[0].name)) continue;
		methods.push({ name: name, $elems: getMethod($docs, i) });
	}
	return methods;
}

function getMethod($docs, methodStart) {
	var _loop = function _loop(i) {
		var $elem = $docs.eq(i);
		if (!/h3|h2|div/g.test($elem[0].name) && i !== $docs.length - 1 && $elem.attr("id") !== "tips") return "continue";
		var methods = [];
		$docs.map(function (index) {
			return index >= methodStart && index < i ? methods.push($docs.eq(index)) : "";
		});
		return {
			v: methods
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

function getDetails($docs) {
	return $docs.map(function (_ref3) {
		var name = _ref3.name,
		    $elems = _ref3.$elems;

		var $zero = $elems[0];
		return {
			article: $zero.text().slice(0, $zero.text().indexOf("(")),
			url: "http://redux.js.org/docs/api/" + name + "#" + $zero.attr("id"),
			abstract: makeAbstract($elems, name),
			category: name === "Store.html" ? "Redux Store Functions" : ""
		};
	});
}

function formatDom(doc) {
	if (!doc) return;
	return doc.replace(/<\s*a[^>]*>/ig, "").replace(/<\s*\/\s*a>/ig, "").replace(/<\s*h4[^>]*>/ig, "<span class=\"prog__sub\">").replace(/<\s*\/\s*h4>/ig, "</span>").replace(/<\s*ul[^>]*>/ig, "<span class=\"prog__ul\">").replace(/<\s*\/\s*ul>/ig, "</span>");
}
//Iterate over li and if li contains code, then get it's text else get it's html
function formatList($ul) {
	var list = [];
	for (var i = 0; i < $ul.length; i += 1) {
		var $li = $ul.eq(i);
		list.push("<li>");
		if (/<\/pre>/g.test($li.html())) {
			(function () {
				var $child = $li.children();
				$child.map(function (i) {
					return (/pre/g.test($child.eq(i)) ? list.push("<pre><code>" + $child.eq(i).text() + "</code></pre>") : list.push("<p>" + $child.eq(i).html() + "</p>")
					);
				});
			})();
		} else list.push($li.html());
		list.push("</li>");
	}
	return list.join("");
}

function helpSummary($elem, name) {
	if (/ul|ol/g.test(name)) return formatList($elem.children());
	return name === "pre" ? "<code>" + $elem.text() + "</code>" : name === "blockquote" ? "" : $elem.html();
}

function formatSummary($elems) {
	var summary = [];
	for (var i = 0; i < $elems.length; i += 1) {
		var $elem = $elems[i];
		var name = /pre|p|h4|h3|blockquote|ul|ol/g.exec($elem[0].name);
		if (!name || $elem.attr("id") === "tips") continue;
		summary.push("<" + name[0] + ">" + helpSummary($elem, name[0]) + "</" + name[0] + ">");
	}
	return summary.join("");
}

function makeAbstract($elems, name) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		title: name === "Store.html" ? "" : "<pre><code>" + $elems[0].text() + "</code></pre>",
		summary: formatSummary($elems),
		endDom: "</section>"
	};
	return Object.keys(abstractData).map(function (prop) {
		return formatDom(abstractData[prop]);
	}).join("").replace(/\t/g, '    ').replace(/\n/g, "\\n");
}
