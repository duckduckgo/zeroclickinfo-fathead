#!/usr/bin/env node 
(function(){
    'use strict';
    
    const BASE_URL = 'http://docs.meteor.com/';
    const DOWNLOAD_DIR = 'download/';
     
    const debug = false;
    
    const fs = require('graceful-fs');
    const jsdom = require('jsdom');
    const chalk = require('chalk');
    const http = require('http');
    
    function downloadByURL(url, cb){
       
       var uri = url.replace(BASE_URL, "").split("/").join("_") || 'index.html';
       if(debug) console.log(chalk.cyan('Fetching => ') + url);
       
	http.get(url, function(res){
          var chunk = '';
           
          res.on('data', function(body){
              fs.appendFileSync(DOWNLOAD_DIR + uri, body);
              chunk += body;
          });
                 
          res.on('end', function(){
              if(debug) console.log(chalk.green('Written to File : ') + DOWNLOAD_DIR + uri);
              
              if(typeof cb == 'function') {
                  var html = chunk.toString('utf-8');
                  cb(html);
              }
          });
           
       });
    };
    
    function main(){
       
        downloadByURL(BASE_URL, function(fileContent){
           jsdom.env(fileContent, function(err, window){
               
               if (err) {
                    console.error(err)
                    return;
                }

               var toc = window.document.querySelectorAll('.item-toc');
               
               for(var i=0;i<toc.length; i++){
                   var selector = toc[i].firstElementChild;
                   var href = selector.href;
                   if(debug) console.log(chalk.red('Downloading ==> ') + href);
                   
                   if(href.startsWith('api/')){
                       downloadByURL(BASE_URL + href);   
                   }
               }
               
               window.close();
           });
       });  
       
    }
    
    main();
    
})();
