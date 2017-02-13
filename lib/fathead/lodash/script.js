"use strict";

const cheerio = require("cheerio");
const fs = require("fs");

(function organize() {
	let lodashDoc = fs.readFileSync("./download/lodash.html");
	let outputData = getDocs(lodashDoc)
		.map($elem => getDetails($elem))
		.map(doc => makeOutput(doc)).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function getDocs(lodashDoc) {
	let docs = [];
	let $ = cheerio.load(lodashDoc);
	let $docContainer = $(".doc-container").children("div");
	for(let i = 0; i < $docContainer.length - 2; i+=1) {
		let $elem = $docContainer.eq(i);
		let $divs = $elem.children("div");
		let category = $elem.children("h2").text();
		$divs.map(index => docs.push({category, $elem: $divs.eq(index)})); //Every children in the current children which is a method
	}
	return docs;
}

function makeOutput({article, url, abstract, category}) {
	return [
		`._${article}()\tA\t\t\t\t${category}\t\t\t\t\thttps://www.drupal.org/files/project-images/logo%20(2).png\t${abstract}\t${url}\n`,
		`${article}\tR\t._${article}()\t\t\t\t\t\t\t\t\t\t\n`,
		`._${article}\tR\t._${article}()\t\t\t\t\t\t\t\t\t\t\n`
	].join("");
}

function getDetails({category, $elem}) {
	let article = $elem.children("h3").attr("id");
	let url = `https://lodash.com/docs/4.17.4#${article}`;
	let abstract = getAbstract($elem);
	category = category.replace(/”|“/g, "");
	return {article, url, abstract, category};
}

function formatDom(doc) {
	return doc
		.replace(/<\s*a[^>]*>/ig, "")
		.replace(/<\s*a[^>]*>/ig, "");
}

function getAbstract($elem) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		title: `<pre>${$elem.children("h3").children("code")}</pre>`,
		summary: "<p>" + $elem.children("p").eq(1).html()
			.replace(/<\s*a[^>]*>/ig, "")
			.replace(/<\s*\/\s*a>/ig, "") + "</p>",
		args: `<span class="prog__sub">Arguments</span>${$elem.children("ol")}`,
		returns: `<span class="prog__sub">Returns</span>${$elem.children("p").eq(3)}`,
		example: formatExample($elem),
		endDom: `</section>`
	};
	return Object.keys(abstractData)
		.map(prop => abstractData[prop] ? formatDom(abstractData[prop]) : "")
		.join("")
		.replace(/\t/g,  '    ')
		.replace(/\n/g, "\\n");
}

function formatExample($elem) { //Some of the methods don't contain example
	let $divs = $elem.children(".highlight").find("div");
	if(!$divs) return null;
	let example = [`<span class="prog__sub">Example</span><pre><code>`];
	$divs.map(i => example.push($divs.eq(i).text()));
	return `${example.join("\n")}</code></pre>`;
}
