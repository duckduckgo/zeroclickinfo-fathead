"use strict";

const fs = require("fs");
const cheerio = require("cheerio");

(function organize() {
	let mongooseData = fs.readFileSync("./download/mongoosejs.html");
	let outputData = getDocs(mongooseData)
		.map($elem => getDetails($elem))
		.filter(doc => { if(doc) return doc; })
		.map(doc => makeOutput(doc)).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput({article, url, abstract}) {
	return [
		`${article}\tA\t\t\t\t\t\t\t\t\thttp://mongodb-tools.com/img/mongoose.png\t${abstract}\t${url}\n`
	].concat(getRedirects(article)).join("");
}

function getRedirects(article) {
	let boilerPlate = `\tR\t${article}\t\t\t\t\t\t\t\t\t\t\n`;
	let redirects = [];
	if(article.indexOf("-") !== -1) {
		redirects.push(
			article.replace(/-/g, "."),
			article.replace(/-/g, " ")
		);
	} else if(article.indexOf(".") !== -1) {
		redirects.push(article.replace(/\./g, " "));
	}
	return redirects.map(current => current + boilerPlate);
}

function getDocs(mongooseData) {
	let methods = [];
	let $ = cheerio.load(mongooseData);
	let $docs = $("#content ul .module"); //All the docs
	for(let i = 0; i < $docs.length; i+=1) {
		let $elem = $docs.eq(i).children(".item.public"); //div containing the methods
		$elem.map(index => methods.push($elem.eq(index))); //method
	}
	return methods;
}

function getDetails($elem) {
	let article = $elem.children("h3").attr("id");
	if(!article || article.indexOf("%20") !== -1) return null;
	article = article.slice(article.lastIndexOf("_") + 1, article.length).replace(/%24/g, "$");
	if(!article) return null;
	let url = `http://mongoosejs.com/docs/api.html#${$elem.children("h3").attr("id")}`;
	let abstract = makeAbstract($elem);
	return {article, url, abstract};
}

function formatDom(desc) { //Replaces all the <h4> tag and removes <a> tag replacing it with it's title
	if(!desc) return;
	return desc.replace(/<\s*a[^>]*>/ig, "")
		.replace(/<\s*\/\s*a>/ig, "")
		.replace(/<h4>/g, `<span class="prog__sub">`)
		.replace(/<\/h4>/g, `</span>`);
}

function formatArg($elem, className) {
	let arg = $elem.children(className);
	if(!arg.text()) return null;
	let title = `<span class="prog__sub">${arg.children("h4").text()}</span>`;
	let body = "<ul>";
	let $argLi = arg.children("ul").children("li");
	for(let i = 0; i < $argLi.length; i+=1) {
		let $currElem = $argLi.eq(i);
		body += `<li><p><code>${$currElem.children("code").text()}</code>`;
		$currElem.children("span")
			.map(index =>
				body += formatArgHelper($currElem.children("span").eq(index).text())
			);
		body += "</p></li>";
	}
	return `${title}${body}</ul>`;
}

function formatArgHelper(str) { //I couldn't figure out a regexp for this
	str = str.split("");
	str[str.indexOf("<")] = "<code>";
	str[str.indexOf(">")] = "</code>";
	return str.join("");
}

function formatSummary($elem) {//For methods without any classes, just randomly in the div
	let summary = [];
	for(let i = 0; i < $elem.length; i+=1) { //This is kind of ugly
		let ifFound = /^pre|^p|^h4|^ul/g.exec($elem.eq(i)[0].name);
		if(!ifFound) continue;
		if(ifFound[0] === "p") {
			summary.push(`<p>${$elem.eq(i).text()}</p>`);
		} else if(ifFound[0] === "pre") {
			summary.push(`<pre><code>${$elem.eq(i).text()}</code></pre>`);
		} else if(ifFound[0] === "h4") {
			summary.push(`<span class="prog__sub">${$elem.eq(i).text()}</span>`);
		} else {
			summary.push($elem.eq(i).html());
		}
	}
	return summary.join("");
}

function makeAbstract($elem) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		title: `<pre><code>${$elem.children("h3").text()}</code></pre>`,
		summary: formatSummary($elem.children()),
		events: $elem.children(".events").html(),
		args: formatArg($elem, ".params"),
		returns: formatArg($elem, ".returns"),
		description: formatSummary($elem.children(".description").children()),
		endDom: `</section>`
	};
	//Abstract is not consistent, so sometimes abastractData has empty properties
	return Object.keys(abstractData)
		.map(prop => abstractData[prop] ? formatDom(abstractData[prop]) : "")
		.join("")
		.replace(/\t/g,  '    ')
		.replace(/\n/g, "\\n");
}