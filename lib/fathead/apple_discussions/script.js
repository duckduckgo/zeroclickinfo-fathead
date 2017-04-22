"use strict";

const fs = require("fs");
const cheerio = require("cheerio");
const https = require("https");

(async function organize() {
	try {
		let sitemapData = fs.readFileSync("./download/sitemap.xml");
		let outputData = await getDocs(sitemapData);
		outputData = outputData.map(doc => getDetails(doc))
			.map(doc => makeOutput(doc)).join("");
		fs.writeFileSync("./output.txt", outputData);
	} catch(e) { throw e; }
})();

function makeOutput({article, url, abstract}) {
	return [
		`${article}\tA\t\t\t\t\t\t\t\t\t\t${abstract}\t${url}\n`
	].join("");
}

async function getDocs(sitemap) {
	try {
		let $ = cheerio.load(sitemap, { xmlMode: true });
		let $loc = $("loc");
		let threads = [];
		for(let i = 170; i < $loc.length; i+=1) {
			let url = $loc.eq(i).html();
			let docHtml = await getThread(url);
			threads.push({url, docHtml});
		}
		await Promise.all(threads);
		return threads.filter(thread => {
			let $ = cheerio.load(thread.docHtml);
			if(/Solved/g.test($("span.marker-text").text())) return thread;
		});
	} catch(e) { throw e; }
}

function getThread(url) {
	try {
		return new Promise((resolve, reject) => {
			setTimeout(() => {
				let options = {
					host: "discussions.apple.com",
					path: url,
					headers: {
						"Accept-Encoding": "gzip;q=0;deflate;q=0",
						"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3076.0 Safari/537.36"
					}
				};
				https.get(options, res => {
					res.setEncoding("utf8");
					let output = "";
					res.on("data", chunk => output += chunk);
					res.on("end", () => resolve(output));
					res.on("error", e => reject(e));
				});
			}, 2000);
		});
	} catch(e) { throw e; }
}

function getDetails({url, docHtml}) {
	let $ = cheerio.load(docHtml);
	let article = $(".j-thread-post-wrapper h1").text().replace(/Q:/, "").trim();
	url += `#${$("[itemprop=acceptedAnswer]").attr("id")}`; //Link to acceptedAnswer
	let abstract = makeAbstract($);
	return {article, url, abstract};
}

function formatDom(dom) {
	if(!dom) return;
	return dom.replace(/<\s*a[^>]*>/ig, "")
		.replace(/<\s*\/\s*a>/ig, "")
		.replace(/<\s*p[^>]*>/ig, "<p>")
		.replace(/<\s*div[^>]*>/ig, "<div>")
		.replace(/<\s*div[^<\/div>]*>/ig, "") //Empty div
		.replace(/<\/\s*[^<\/div>]*div>/gi, "") //Empty div
		.replace(/<\s*h4[^>]*>/ig, `<span class="prog__sub">`)
		.replace(/<\s*\/\s*h4>/ig, "</span>")
		.replace(/<\s*ul[^>]*>/ig, `<span class="prog__ul">`)
		.replace(/<\s*\/\s*ul>/ig, "</span>");
}

function makeAbstract($) {
	let abstractData = {
		startDom: `<section class="prog__container">`,
		answer: $(".j-inline-correct-answer section .jive-rendered-content").html(),
		endDom: `</section>`
	};
	return Object.keys(abstractData)
		.map(prop => formatDom(abstractData[prop])).join("")
		.replace(/\t/g,  "    ")
		.replace(/\n/g, "\\n");
}
