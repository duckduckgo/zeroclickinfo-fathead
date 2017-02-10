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

function formatTitle(title) {
	return title.replace(/<code>|<\/code>/g, "");
}

function formatDesc($elem) {
	let descCode = `<pre><code>${$elem.children(".description").children("pre").text()}</code></pre>`;
	$elem.children(".description").children("pre").remove();
	let descWithoutCode = $elem.children(".description").html();
	return descWithoutCode + descCode;
}

function makeAbstract($elem) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		title: `<pre><code>${formatTitle($elem.children("h3").html())}</code></pre>`,
		summary: `<p>${$elem.children("p").eq(0).html()}</p>`,
		description: formatDesc($elem),
		events: $elem.children(".events").html(),
		args: $elem.children(".params").html(),
		returns: formatReturns($elem),
		endDom: `</section>`
	};
	//Abstract is not consistent, so sometimes abastractData has empty properties
	return Object.keys(abstractData)
		.map(prop => abstractData[prop] ? formatDom(abstractData[prop]) : "")
		.join("")
		.replace(/\t/g,  '    ')
		.replace(/\n/g, "\\n");
}

function formatReturns($elem) {
	return $elem.children(".returns").html() ? `<pre>${$elem.children(".returns").html()}</pre>` : null;
}
