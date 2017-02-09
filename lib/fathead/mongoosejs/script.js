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
	if(article.indexOf("#") !== -1) {
		redirects.push(
			article.replace(/#/g, "."),
			article.replace(/#/g, " "),
			removeBrack(article.replace(/#/g, ".")),
			removeBrack(article.replace(/#/g, " "))
		);
	}
	return redirects.map(current => current + boilerPlate);
}

function removeBrack(text) {
	return text.slice(0, text.indexOf("("));
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
	let article = formatDom($elem.children("h3").html()).replace(/<span>|<\/span>/g, "");
	let url = `http://mongoosejs.com/docs/api.html#${$elem.children("h3").attr("id")}`;
	let abstract = makeAbstract($elem);
	return {article, url, abstract};
}

function formatDom(desc) { //Replaces all the <h4> tag and removes <a> tag replacing it with it's title
	if(!desc) return;
	return replaceAnchorWithTitle(desc)
	.replace(/<h4>/g, `<span class="prog__sub">`)
	.replace(/<\/h4>/g, `</span>`);
}

function makeAbstract($elem) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		title: `<pre><code>${$elem.children("h3").html()}</code></pre>`,
		summary: `<p>${$elem.children("p").eq(0).html()}</p>`,
		description: $elem.children(".description").html(),
		events: $elem.children(".events").html(),
		args: $elem.children(".params").html(),
		returns: formatReturns($elem),
		endDom: `</section>`
	};
	//Abstract is not consistent, so sometimes abastractData has empty properties
	return Object.keys(abstractData)
		.map(prop => abstractData[prop] ? formatDom(abstractData[prop]) : "")
		.join("")
		.replace(/\t/g,  '    ').replace(/\n/g, "\\n");
}

function formatReturns($elem) {
	return $elem.children(".returns").html() ? `<pre>${$elem.children(".returns").html()}</pre>` : null;
}

function getAnchorTitle(anchorData, [startInd, endInd]) {
	let onlyAnchorText = anchorData.slice(startInd, endInd);
	let tagEnd = onlyAnchorText.indexOf(`">`) + 2; //just to make sure
	return onlyAnchorText.slice(tagEnd, endInd);
}

function replaceAnchorWithTitle(anchorData) { //Removing <a> tag while taking out the text between the <a> tag.
	while(anchorData.indexOf("<a") !== -1) {
		let [startInd, endInd] = [anchorData.indexOf("<a"), anchorData.indexOf("</a>")];
		let anchorTitle = getAnchorTitle(anchorData, [startInd, endInd]);
		anchorData = anchorData.slice(0, startInd) + anchorTitle + anchorData.slice(endInd + 4, anchorData.length);
	}
	return anchorData;
}
