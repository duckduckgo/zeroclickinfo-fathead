"use strict";

const fs = require("fs");
const cheerio = require("cheerio");

(function organize() {
	let mongooseData = fs.readFileSync("./download/mongoosejs.html");
	let outputData = getDocs(mongooseData)
		.map($elem => getDetails($elem))
		.filter(doc => { if(doc) return doc; });
	let disambiguations = getDisambiguationsAndRedirects(outputData);
	outputData = outputData.map(doc => makeOutput(doc)).join("");
	fs.writeFileSync("./output.txt", outputData + disambiguations);
})();

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
	let disambiguation = $elem.children("p").eq(0).text();
	let category = getCategory(article);
	if(!/\.|-/gi.test(article)) article = `=${article}`; //for disambiguations
	return {article, url, abstract, disambiguation, category};
}

function getCategory(article) {
	let exec = /-|\./g.exec(article);
	return exec ? article.slice(0, exec.index) : article;
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

function makeOutput({article, url, abstract, category}) {
	return [
		`${article}\tA\t\t\t${category}\t\t\t\t\t\thttp://mongodb-tools.com/img/mongoose.png\t${abstract}\t${url}\n`
	];
}

function getDisambiguationsAndRedirects(docs) {
	let similar = [];
	let alreadyWritten = []; //For duplicates
	let redirects = [];
	docs.map(({article}) => {
		let exec = /\.|-|=/g.exec(article);
		let findThis = article.slice(exec.index + 1, article.length); //Extract article (Mongoose-Aggeragate -> Aggeragate)
		let disambiguations = findDisambiguations(docs, findThis, alreadyWritten);
		if(disambiguations) {
			/\\n\*/g.test(disambiguations.queries) ? similar.push(disambiguations) :
			redirects.push({
				userQuery: findThis,
				redirect: disambiguations.queries.slice(3, disambiguations.queries.indexOf("]"))
			});
		}
		alreadyWritten.push(findThis); //for duplicates
	});
	return [
		similar.map(({userQuery, queries}) => `${userQuery}\tD\t\t\t\t\t\t\t\t${queries}\t\t\t\n`).join(""),
		redirects.map(({userQuery, redirect}) => `${userQuery}\tR\t${redirect}\t\t\t\t\t\t\t\t\t\t\n`).join("")
	].join("");
}

function findDisambiguations(docs, findThis, alreadyWritten) {
	let similar = {userQuery: findThis, queries: "*"}; //To get all the queries for the current article
	docs.map(({article, disambiguation}) => {
		let reg = new RegExp("^" + findThis + "$", "g");
		let exec = /\.|-|=/g.exec(article);
		let slicedArt = article.slice(exec.index + 1, article.length);
		if(reg.test(slicedArt) && alreadyWritten.indexOf(findThis) === -1) similar.queries += `[[${article}]], ${disambiguation}\\n*`;
	});
	return similar.queries.length === 1 ? null :
		{ ...similar, queries: similar.queries.slice(0, similar.queries.length - 3)};
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
	let body = `<ul class="prog__ul">`;
	let $argLi = arg.children("ul").children("li");
	for(let i = 0; i < $argLi.length; i+=1) {
		let [$span, $code] = [$argLi.eq(i).children("span"), $argLi.eq(i).children("code")];
		body += `<li><p><code>${$code.text()}</code>`;
		$span.map(index => body += formatArgHelper($span.eq(index).text()) );
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
		} else if(ifFound[0] === "ul") {
			summary.push(`<ul class="prog__ul>"${$elem.eq(i).html()}</ul>`);
		} else {
			summary.push($elem.eq(i).html());
		}
	}
	return summary.join("");
}
