"use strict";

const fs = require("fs");
const cheerio = require("cheerio");

(function organize() {
	let vueJsData = fs.readFileSync("./download/vuejs.html");
	let outputData = getDocs(vueJsData)
		.map(doc => getDetails(doc))
		.map(doc => makeOutput(doc)).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput({category, article, url, abstract}) {
	return [
		`${article}\tA\t\t\t${category}\t\t\t\t\t\thttps://vuejs.org/images/logo.png\t${abstract}\t${url}\n`
	].concat(getRedirects(article)).join("");
}

function getRedirects(article) {
	let redirects = [];
	let boilerPlate = `\tR\t${article}\t\t\t\t\t\t\t\t\t\t\n`;
	let vue = article.lastIndexOf("Vue");
	if(vue !== -1) redirects.push(article.slice(vue + 4, article.length));
	return redirects.map(current => current + boilerPlate);
}

function getDocs(vueJsData) {
	let methods = [];
	let $ = cheerio.load(vueJsData);
	let $docs = $(".content.api.with-sidebar").children();
	let category = "Global Config";
	for(let i = 3; i < $docs.length - 24; i+=1) { //-24 is to ignore Special attributes and Build-In Components)
		if($docs.eq(i)[0].name === "h2")  {
			category = $docs.eq(i).text();
			continue;
		}
		if($docs.eq(i)[0].name === "h3") methods.push({category, $artElem: $docs.eq(i), $elem: $docs.eq(i + 1) });
	}
	return methods;
}

function getDetails({category, $artElem, $elem}) {
	let article = $artElem.text().replace(/\((.)+\)/g, "");
	let url = `https://vuejs.org/v2/api/#${article}`;
	let abstract = makeAbstract($artElem, $elem);
	return {article, url, abstract, category};
}

function formatDom(doc) {
	if(!doc) return;
	return doc.replace(/<\s*a[^>]*>/ig, "")
		.replace(/<\s*\/\s*a>/ig, "")
		.replace(/<strong>/gi, `<span class="prog__sub">`)
		.replace(/<\/strong>/gi, `</span>`);
}

function formatSummary($elem) {
	let $li = $elem.children();
	let summary = [];
	$li.map(i => {
		let $elem = $li.eq(i);
		let $code = $elem.children(".highlight");
		if($code.html()) {
			let $p = $elem.children("p");
			$p.map(index => summary.push(`<p>${$p.eq(index).html()}</p>`));
			let $codeDivs = $code.find("pre").children("div");
			summary.push("<pre><code>");
			$codeDivs.map(index => summary.push($codeDivs.eq(index).text() + "\n"));
			summary.push("</code></pre>");
			return;
		}
		summary.push($elem.html());
	});
	return summary.map(doc => formatDom(doc)).join("");
}

function makeAbstract($artElem, $elem) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		title: `<pre><code>${$artElem.text()}</code></pre>`,
		summary: formatSummary($elem),
		endDom: `</section>`
	};
	return Object.keys(abstractData)
		.map(prop => abstractData[prop])
		.join("")
		.replace(/\t/g,  '    ')
		.replace(/\n/g, "\\n");
}
