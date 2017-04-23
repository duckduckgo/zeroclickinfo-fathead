"use strict";

var _regenerator = require("babel-runtime/regenerator");

var _regenerator2 = _interopRequireDefault(_regenerator);

var _asyncToGenerator2 = require("babel-runtime/helpers/asyncToGenerator");

var _asyncToGenerator3 = _interopRequireDefault(_asyncToGenerator2);

var getDocs = function () {
	var _ref3 = (0, _asyncToGenerator3.default)(_regenerator2.default.mark(function _callee2(sitemap) {
		var $, $loc, threads, i, url, docHtml;
		return _regenerator2.default.wrap(function _callee2$(_context2) {
			while (1) {
				switch (_context2.prev = _context2.next) {
					case 0:
						_context2.prev = 0;
						$ = cheerio.load(sitemap, { xmlMode: true });
						$loc = $("loc");
						threads = [];
						i = 170;

					case 5:
						if (!(i < $loc.length)) {
							_context2.next = 14;
							break;
						}

						url = $loc.eq(i).html();
						_context2.next = 9;
						return getThread(url);

					case 9:
						docHtml = _context2.sent;

						threads.push({ url: url, docHtml: docHtml });

					case 11:
						i += 1;
						_context2.next = 5;
						break;

					case 14:
						_context2.next = 16;
						return Promise.all(threads);

					case 16:
						return _context2.abrupt("return", threads.filter(function (thread) {
							//Checks if the thread has an accepted Answer
							var $ = cheerio.load(thread.docHtml);
							if (/Solved/g.test($("span.marker-text").text())) return thread;
						}));

					case 19:
						_context2.prev = 19;
						_context2.t0 = _context2["catch"](0);
						throw _context2.t0;

					case 22:
					case "end":
						return _context2.stop();
				}
			}
		}, _callee2, this, [[0, 19]]);
	}));

	return function getDocs(_x) {
		return _ref3.apply(this, arguments);
	};
}();

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var fs = require("fs");
var cheerio = require("cheerio");
var https = require("https");

(function () {
	var _ref = (0, _asyncToGenerator3.default)(_regenerator2.default.mark(function _callee() {
		var sitemapData, outputData;
		return _regenerator2.default.wrap(function _callee$(_context) {
			while (1) {
				switch (_context.prev = _context.next) {
					case 0:
						_context.prev = 0;
						sitemapData = fs.readFileSync("./download/sitemap.xml");
						_context.next = 4;
						return getDocs(sitemapData);

					case 4:
						outputData = _context.sent;

						outputData = outputData.map(function (doc) {
							return getDetails(doc);
						}).map(function (doc) {
							return makeOutput(doc);
						}).join("");
						fs.writeFileSync("./output.txt", outputData);
						_context.next = 12;
						break;

					case 9:
						_context.prev = 9;
						_context.t0 = _context["catch"](0);
						throw _context.t0;

					case 12:
					case "end":
						return _context.stop();
				}
			}
		}, _callee, this, [[0, 9]]);
	}));

	function organize() {
		return _ref.apply(this, arguments);
	}

	return organize;
})()();

function makeOutput(_ref2) {
	var article = _ref2.article,
	    url = _ref2.url,
	    abstract = _ref2.abstract;

	return [article + "\tA\t\t\t\t\t\t\t\t\t\t" + abstract + "\t" + url + "\n"].join("");
}

function getThread(url) {
	try {
		return new Promise(function (resolve, reject) {
			setTimeout(function () {
				var options = {
					host: "discussions.apple.com",
					path: url,
					headers: {
						"Accept-Encoding": "gzip;q=0;deflate;q=0",
						"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3076.0 Safari/537.36"
					}
				};
				https.get(options, function (res) {
					res.setEncoding("utf8");
					var output = "";
					res.on("data", function (chunk) {
						return output += chunk;
					});
					res.on("end", function () {
						return resolve(output);
					});
					res.on("error", function (e) {
						return reject(e);
					});
				});
			}, 5500);
		});
	} catch (e) {
		throw e;
	}
}

function getDetails(_ref4) {
	var url = _ref4.url,
	    docHtml = _ref4.docHtml;

	var $ = cheerio.load(docHtml);
	var article = $(".j-thread-post-wrapper h1").text().replace(/Q:/, "").trim();
	var linkToAnswer = "#" + $("[itemprop=acceptedAnswer]").attr("id");
	if (linkToAnswer) url += linkToAnswer;
	var abstract = makeAbstract($);
	return { article: article, url: url, abstract: abstract };
}

function formatDom(dom) {
	if (!dom) return;
	return dom.replace(/<\s*a[^>]*>/ig, "").replace(/<\s*\/\s*a>/ig, "").replace(/<\s*p[^>]*>/ig, "<p>").replace(/<\s*div[^>]*>/ig, "<div>").replace(/<\s*div[^<\/div>]*>/ig, "") //Empty div
	.replace(/<\/\s*[^<\/div>]*div>/gi, "") //Empty div
	.replace(/<\s*h4[^>]*>/ig, "<span class=\"prog__sub\">").replace(/<\s*\/\s*h4>/ig, "</span>").replace(/<\s*ul[^>]*>/ig, "<span class=\"prog__ul\">").replace(/<\s*\/\s*ul>/ig, "</span>");
}

function makeAbstract($) {
	var abstractData = {
		startDom: "<section class=\"prog__container\">",
		answer: $(".j-inline-correct-answer section .jive-rendered-content").html(),
		endDom: "</section>"
	};
	return Object.keys(abstractData).map(function (prop) {
		return formatDom(abstractData[prop]);
	}).join("").replace(/\t/g, "    ").replace(/\n/g, "\\n");
}
