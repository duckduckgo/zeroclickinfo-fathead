fs = require('fs');

var rootUrl = 'https://www.cia.gov/library/publications/the-world-factbook/';
var tableId = 'table#fieldListing';
var countryClass = 'td.country';
var abstractClass = 'td.fieldData';

fs.readFile('download/page.txt', 'utf-8', function(err, data) {
	if(err) throw err;

	var cheerio = require('cheerio'), $ = cheerio.load(data);

	fs.writeFile('output.txt', scrape($), function (err) {
		if(err) throw err;
	});
});

function scrape($) {
	var output = '';

	var titles = $(countryClass).map(function(index, element) {
		return $(element).children().first().text().trim();
	}).get();

	//Take the first line of every abstract
	var abstracts = $(abstractClass).map(function(index, element) {
		return $(element).text().trim().split('\n')[0];
	});

	//strip "../" part from every url
	var urls = $(countryClass).map(function(index, element) {
		return rootUrl + $(element).children().first().attr('href').substr(3);
	}).get();

	if(titles.length != abstracts.length || abstracts.length != urls.length) throw "Scraped data invalid!";

	for(var i = 0; i < titles.length; ++i) {
		output += generateFatheadLine(titles[i], 'A', '', abstracts[i], urls[i]);
	}

	//now look for languages with variations, such as "Gambia, The", "The Gambia" and "Gambia"
	titles.forEach(function (element, index) {
		if(element.indexOf(', The') != -1) {
			//The Gambia
			output += generateFatheadLine('The ' + element.split(',')[0], 'R', element, abstracts[index], urls[index]);
			//Gambia
			output += generateFatheadLine(element.split(',')[0], 'R', element, abstracts[index], urls[index]);
		}
	});

	return output;
}

function generateFatheadLine(title, type, redirect, abstract, url) {
	return title + '\t' + type + '\t' + redirect + '\t\t\t\t\t\t\t\t\t' + abstract + '\t' + url + '\n';
}
