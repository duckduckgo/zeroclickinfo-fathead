"use strict";

const fs = require("fs");
const cheerio = require("cheerio");

(function organize() {
	let outputData = getFiles()
		.map(file => getDocs(file))
		.map(docs => getDetails(docs))
		.map(docs => makeOutput(docs)).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput(docs) {
	return docs.map(({category, url, abstract, article}) => {
		return [
			`${article}\tA\t\t\t${category}\t\t\t\t\t\t\t${abstract}\t${url}\n`
		].concat(getRedirects(article)).join("");
	}).join("");
}

function getRedirects(article) {
	let newArticle = article.replace(/grunt\./g, "");
	return  [
		`${newArticle}\tR\t${article}\t\t\t\t\t\t\t\t\t\t\n`
	];
}

function getFiles() {
	let dir = fs.readdirSync("./download");
	return dir.map(file => fs.readFileSync(`./download/${file}`, "utf-8"));
}

function getDocs(file) {
	let methods = [];
	let $ = cheerio.load(file);
	let $docs = $(".hero-unit");
	let baseURL = `http://gruntjs.com/api/${$docs.find("h1").text()}`;
	let category = $docs.find("h1").text().split(".")
		.map(name => name = name[0].toUpperCase() + name.slice(1)).join(" ") + " Functions";
	for(let i = 3; i < $docs.children().length; i+=1) {
		let $elem = $docs.children().eq(i);
		if(!/h3/g.test($elem[0].name)) continue;
		methods.push({category, $elems: getMethod($docs.children(), i), baseURL});
	}
	return methods;
}

//Gets the start(/h3/) of the method -> Outputs the HTML elements between the start and the end(/h3|h2/)
function getMethod($docs, methodStart) {
	let method = [];
	for(let i = methodStart + 1; i < $docs.length; i+=1) {
		let $elem = $docs.eq(i);
		if(!/h3|h2|div/g.test($elem[0].name)) continue;
		$docs.map(index => index >= methodStart && index < i ? method.push($docs.eq(index)) : "");
		return method;
	}
}

function getDetails(docs) {
	return docs.map(({category, $elems, baseURL}) => {
		let $anchor = $elems[0].children("a");
		let url = baseURL + $anchor.attr("href");
		let article = $anchor.attr("name");
		let abstract = makeAbstract($elems.splice(1, $elems.length));
		return {category, url, article, abstract};
	});
}

function formatSummary($elems) {
	let summary = [];
	for(let i = 0; i < $elems.length; i+=1) {
		let $elem = $elems[i];
		let name = $elem[0].name;
		if(name === "p") /example/gi.test($elem.text()) ?
			summary.push(`<span class="prog__sub">${$elem.text()}</span>`) :
			summary.push(`<p>${$elem.text()}</p>`);
		if(name === "pre") summary.push(`<pre><code>${$elem.text()}</code></pre>`);
	}
	return summary.join("");
}

function makeAbstract($elems) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		summary: formatSummary($elems),
		endDom: `</section>`
	};
	return Object.keys(abstractData)
		.map(prop => abstractData[prop])
		.join("")
		.replace(/\t/g,  '    ')
		.replace(/\n/g, "\\n");
}
