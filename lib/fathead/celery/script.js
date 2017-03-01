"use strict";

const fs = require("fs");
const cheerio = require("cheerio");

(function organize() {
	let celeryData = fs.readFileSync("./download/celery.html");
	let outputData = getDocs(celeryData)
		.map(doc => getDetails(doc))
		.map(docs => makeOutput(docs)).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput(docs) {
	return docs.map(({article, category, abstract, url}) => {
		return [
			`${article}\tA\t\t\t${category}\t\t\t\t\t\t\t${abstract}\t${url}\n`
		].concat(getRedirects(article)).join("");
	}).join("");
}

function getRedirects(article) {
	if(!/\./g.test(article)) return "";
	let redirs = [];
	let newArt = article.slice(article.lastIndexOf(".") + 1, article.length);
	/_/g.test(article) ? redirs.push(newArt.replace(/_/g, " ")) : redirs.push(newArt);
	return redirs.map(art => `${art}\tR\t${article}\t\t\t\t\t\t\t\t\t\t\n`);
}

function getDocs(celeryData) {
	let methods = [];
	let $ = cheerio.load(celeryData);
	let $docs = $("#api-reference").children(".toctree-wrapper").children(".section");
	for(let i = 3; i < $docs.length; i+=1) {
		let $elem = $docs.eq(i);
		if(!$elem.children(".class").html()) continue;
		let id = $elem.attr("id").replace(/-/g, ".");
		methods.push({
			category: `${id} Functions`,
			baseURL: `${id}.html`,
			$methods: $elem.children(".class").children("dd").children()
		});
	}
	return methods;
}

function getDetails({category, baseURL, $methods}) {
	let details = [];
	baseURL = `http://docs.celeryproject.org/en/stable/reference/${baseURL}`;
	for(let i = 0; i < $methods.length; i+=1) {
		let $elem = $methods.eq(i);
		if(!/method/g.test($elem.attr("class"))) continue;
		let article = $elem.find(".descclassname").text() + $elem.find(".descname").text();
		let url = baseURL + $elem.find(".headerlink").attr("href");
		let abstract = makeAbstract($elem);
		details.push({article, category, url, abstract});
	}
	return details;
}

function formatDescription($elem) {
	if(!$elem.html()) return "";
	let desc = [];
	$elem.map(i => {
		let attr = $elem.eq(i).attr("class");
		if(attr === "rubric") return desc.push(`<span class="prog__sub">${$elem.eq(i).text()}</span>`);
		desc.push(`<p>${$elem.eq(i).text()}</p>`);
	});
	return desc.join("");
}

function summaryHelper($tr) { //If the current row has a list or not
	let $td = $tr.children("td");
	let $ul = $td.find("ul");
	if(!/<\/ul>/.test($tr.html())) return $td.text() ? `<p>${$td.text()}</p>` : "";
	return '<ul class="prog__ul">' + $ul.text().split("\n")
			.filter(curr => curr ? curr : null)
			.map(curr => `<li>${curr}</li>`).join("") + "</ul>";
}

function formatSummary($elem) {
	let summary = [];
	for(let i = 0; i < $elem.length; i+=1) { //For all the rows in the table body
		let $tr = $elem.eq(i);
		let $th = $tr.children("th");
		let name = !$th.text() ? "" : `<span class="prog__sub">${$th.text().replace(/:/g, "")}</span>`;
		let body = summaryHelper($tr);
		summary.push(`${name}\n${body}`);
	}
	return summary.join("");
}

function formatDom(desc) {
	return desc.replace(/<\s*a[^>]*>/ig, "")
		.replace(/<\s*\/\s*a>/ig, "")
		.replace(/\[source\]¶/g, "")
		.replace(/¶/, "")
		.replace(/\n/g, "");
}

function formatCode($elem) {
	if(!$elem.find(".highlight pre").text()) return "";
	return `<pre><code>${$elem.find(".highlight pre").text()}</code></pre>`.split("...").join("\n");
}

function makeAbstract($elem) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		title: `<pre><code>${$elem.find("dt").text()}</code></pre>`,
		description: formatDescription($elem.children("dd").children("p")),
		example: formatCode($elem),
		summary: formatSummary($elem.find("table tbody").children("tr")),
		endDom: `</section>`
	};
	return Object.keys(abstractData)
		.map(prop => abstractData[prop] ? formatDom(abstractData[prop]) : "")
		.join("")
		.replace(/\t/g,  '    ')
		.replace(/\n/g, "\\n");
}
