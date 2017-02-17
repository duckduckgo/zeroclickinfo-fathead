"use strict";

const fs = require("fs");
const cheerio = require("cheerio");

(function organize () {
	let lessData = fs.readFileSync("./download/less.html");
	let outputData = getDocs(lessData)
		.map(docs => getDetails(docs))
		.map(docs => makeOutput(docs)).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput(docs) {
	return docs.map(({article, category, abstract, url}) => {
		return [
			`${article}\tA\t\t\t${category}\t\t\t\t\t\thttp://lesscss.org/public/img/logo.png\t${abstract}\t${url}\n`
		];
	}).join("");
}

function getDocs(lessData) {
	let methods = [];
	let $ = cheerio.load(lessData);
	let $docs = $(".panel.docs-content").children();
	for(let i = 1; i < $docs.length; i+=1) {
		let $elem = $docs.eq(i).children(".section-content").html();
		let category = $docs.eq(i).find(".page-header h1").text().toLowerCase().replace("\n", "");
		methods.push({category, docs: $elem.split(`<h3 class="docs-heading">`)});
	}
	return methods;
}

function getDetails({category, docs}) {
	let details = [];
	docs.forEach((doc, index) => {
		if(index === 0) return;
		doc = doc.split("\n");
		let article = doc[0].slice(0, doc[0].indexOf("<"));
		let url = `http://lesscss.org/functions/#${category.replace(" ", "-")}-${article}`;
		let abstract = makeAbstract(doc);
		details.push({category, article, url, abstract});
	});
	return details;
}

function formatDom(desc) {
	if(!desc) return;
	return desc.replace(/<\s*a[^>]*>/ig, "")
		.replace(/<\s*\/\s*a>/ig, "");
}

function makeAbstract(doc) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		description: doc[3],
		details: "",
		endDom: `</section>`
	};
	for(let i = 5; i < doc.length; i+=1) {
		if(/<br>|^\s+$|img/g.test(doc[i])) continue;
		if(/:/g.test(doc[i]) && /<\/p>/g.test(doc[i])) {
			doc[i] = doc[i].replace("<p>", `<span class="prog__sub">`).replace(":", `</span><p>`);
		}
		if(/<ul/g.test(doc[i])) doc[i] = doc[i].replace("<ul", `<ul class="prog__ul"`);
		if(/<pre>/g.test(doc[i])) {
			let {code, loc} = formatCode(doc, i); //gets the code and the end location of code in array(doc)
			abstractData.details += code; //add code
			/*
				Two statements below are used to make the loop skip the code in array because they've been added above.
				This is done by jumping the loop ahead by changing the value of i
			*/
			i = loc - 1; //make loop go to the location just before the end of the code
			doc[i] = ""; //this makes the current value to "" or it will add the last line of code again
		}
		abstractData.details += doc[i];
	}
	return Object.keys(abstractData)
		.map(doc => formatDom(abstractData[doc]))
		.join("")
		.replace(/\t/g,  '    ')
		.replace(/\n/g, "\\n");
}

function formatCode(doc, i) { //Extracting code and formmating it and then giving the location of the end of the code in the array
	let preLoc = [i];
	doc.forEach((current, index) => /<\/pre>/g.test(current) && !preLoc[1] && index > i ? preLoc[1] = index : "");
	let code = doc.slice(preLoc[0], preLoc[1])
		.join("\n")
		.replace(/<span[^>]*>/ig, "")
		.replace(/<*\/*span>/ig, "");
	return {code, loc: preLoc[1]};
}
