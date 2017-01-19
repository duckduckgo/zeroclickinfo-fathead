#!/usr/bin/env node
"use strict";

//const request = require('sync-request');


const dataSource = 'https://webpack.js.org/configuration/';

//let res = request('GET', dataSource);
//let html = res.getBody().toString('utf8');

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
