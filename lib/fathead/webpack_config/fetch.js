#!/usr/bin/env node
"use strict";
/*
* This is a small JS script created to parse the dataSource and extract all the href values of a tags with class code-link
*/
const dataSource = 'https://webpack.js.org/configuration/';

require("jsdom").env(dataSource, function(err, window) {
    if (err) {
        console.error(err);
        return;
    }
 
    let $ = require("jquery")(window);
    let links = $("a.code-link");
    links.map( (link) => {
	console.log(links[link].href);
    });

});
