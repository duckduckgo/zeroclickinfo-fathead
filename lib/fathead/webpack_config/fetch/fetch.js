#!/usr/bin/env node
"use strict";

const request = require('sync-request');


const dataSource = 'https://webpack.js.org/configuration/';

let res = request('GET', dataSource);
let html = res.getBody().toString('utf8');

console.log(html);

