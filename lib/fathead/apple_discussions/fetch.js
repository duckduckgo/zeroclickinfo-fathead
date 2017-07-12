
const fs = require('fs');
const request = require('request');
const wait = require('wait-for-stuff');

const dir = "apple_data/";
const delay = 100; // the interval to scrape site. default: .5second

// slurps the url list
function get_urls() {
  let urls = fs.readFileSync('./urls.txt', 'utf8');
  return urls.split('\n');
}

/**
 * Loops through list of URLs
 */
urls = get_urls();
let i = 0;

setInterval(function(){
  if(urls[i] === undefined) { return };
  const file_name = urls[i].split("/").pop() + ".html";
  get_html(urls[i], dir, file_name);
  i++;
}, delay);

/**
 * get_html
 *
 * Gets the html by making a request to the url and saves it to a
 * file with it's unique URI id.
 */
function get_html(url, dir, file_name) {
  wait.for.time(3);
  request(url, function(error, response, body){
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
}

