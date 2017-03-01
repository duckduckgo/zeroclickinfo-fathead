"use strict";

const fs = require("fs");
const cheerio = require("cheerio");

(function organize() {
	let outputData = getFiles()
		.map(file => getDocs(file))
		.map(doc => getDetails(doc))
		.map(doc => makeOutput(doc)).join("");
	fs.writeFileSync("./output.txt", outputData);
})();

function makeOutput(docs) {
	return docs.map(({article, url, abstract, category = ""}) => [
		`${article}\tA\t\t\t${category}\t\t\t\t\t\t\t${abstract}\t${url}\n`
	].concat(getRedirects(article)).join("")).join("");
}

function getRedirects(article) {
	let newArticle = article.split(/(?=[A-Z])/g).join(" ");
	return !/\s/g.test(newArticle) ? "" : [
		`${newArticle}\tR\t${article}\t\t\t\t\t\t\t\t\t\t\n`
	];
}

function getFiles() {
	let dir = fs.readdirSync("./download");
	return dir.map(file => ({name: file, file: fs.readFileSync(`./download/${file}`, "utf-8")}) );
}

function getDocs({name, file}) {
	let methods = [];
	let $ = cheerio.load(file);
	let $docs = $(".normal.markdown-section").children();
	let start = (name === "Store.html" ? 7 : 0);
	for(let i = start; i < $docs.length; i+=1) {
		let $elem = $docs.eq(i);
		if(!/h3|h1/g.test($elem[0].name)) continue;
		methods.push({name, $elems: getMethod($docs, i)});
	}
	return methods;
}

function getMethod($docs, methodStart) {
	for(let i = methodStart + 1; i < $docs.length; i+=1) {
		let $elem = $docs.eq(i);
		if(!/h3|h2|div/g.test($elem[0].name) && i !== $docs.length - 1 && $elem.attr("id") !== "tips") continue;
		let methods = [];
		$docs.map(index => index >= methodStart && index < i ? methods.push($docs.eq(index)) : "");
		return methods;
	}
}

function getDetails($docs) {
	return $docs.map(({name, $elems}) => {
		let $zero = $elems[0];
		return {
			article: $zero.text().slice(0, $zero.text().indexOf("(")),
			url: `http://redux.js.org/docs/api/${name}#${$zero.attr("id")}`,
			abstract: makeAbstract($elems, name),
			category: name === "Store.html" ? "Redux Store Functions" : ""
		};
	});
}

function formatDom(doc) {
	if(!doc) return;
	return doc.replace(/<\s*a[^>]*>/ig, "")
		.replace(/<\s*\/\s*a>/ig, "")
		.replace(/<\s*h4[^>]*>/ig, `<span class="prog__sub">`)
		.replace(/<\s*\/\s*h4>/ig, "</span>")
		.replace(/<\s*ul[^>]*>/ig, `<span class="prog__ul">`)
		.replace(/<\s*\/\s*ul>/ig, "</span>");
}
//Iterate over li and if li contains code, then get it's text else get it's html
function formatList($ul) {
	let list = [];
	for(let i = 0; i < $ul.length; i+=1) {
		let $li = $ul.eq(i);
		list.push(`<li>`);
		if(/<\/pre>/g.test($li.html())) {
			let $child = $li.children();
			$child.map(i => /pre/g.test($child.eq(i)) ?
				list.push(`<pre><code>${$child.eq(i).text()}</code></pre>`) :
				list.push(`<p>${$child.eq(i).html()}</p>`)
			);
		} else list.push($li.html());
		list.push(`</li>`);
	}
	return list.join("");
}

function helpSummary($elem, name) {
	if(/ul|ol/g.test(name)) return formatList($elem.children());
	return name === "pre" ? `<code>${$elem.text()}</code>` : (
		name === "blockquote" ? "" : $elem.html()
	);
}

function formatSummary($elems) {
	let summary = [];
	for(let i = 0; i < $elems.length; i+=1) {
		let $elem = $elems[i];
		let name = /pre|p|h4|h3|blockquote|ul|ol/g.exec($elem[0].name);
		if(!name || $elem.attr("id") === "tips") continue;
		summary.push(`<${name[0]}>${helpSummary($elem, name[0])}</${name[0]}>`);
	}
	return summary.join("");
}

function makeAbstract($elems, name) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		title: name === "Store.html" ? "" : `<pre><code>${$elems[0].text()}</code></pre>`,
		summary: formatSummary($elems),
		endDom: `</section>`
	};
	return Object.keys(abstractData)
		.map(prop => formatDom(abstractData[prop]))
		.join("")
		.replace(/\t/g,  '    ')
		.replace(/\n/g, "\\n");
}
