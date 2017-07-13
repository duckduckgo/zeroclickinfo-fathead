"use strict";

const fs = require('fs');
const request = require('request');
const wait = require('wait-for-stuff');

const dir = "apple_data/";
const delay = 100;

// slurps the url list
function get_urls() {
  let urls = fs.readFileSync('./urls.txt', 'utf8');
  return urls.split('\n');
}

/**
 * Loops through list of URLs
 */
let urls = get_urls();
let i = 0;

let resetInterval = setInterval(function(){
  if(urls[i] === undefined) { clearInterval(resetInterval) };

  // makes the file name from the url
  const file_name = urls[i].split("/").pop() + ".html";

  // request the html
  var req = request(urls[i], function(error, response, body){
    if(!error){
      fs.writeFile(dir + file_name, body, (err) => {
        if (err) {
          console.log(err);
          throw err;
        }
        console.log(`${file_name} has been saved to ${dir}`);
      });
    } else {
      console.log(error);
    }
  })

  // if request has returned, we'll block
  if (req) {
    wait.for.time(2);
  }

  i++;
}, delay);

