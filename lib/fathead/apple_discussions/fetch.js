
const fs = require('fs');
const request = require('request');

// reads the url list
function get_urls() {
  let urls = fs.readFileSync('./urls.txt', 'utf8');
  return urls.split('\n');
}

urls = get_urls();

urls.forEach(function(url) {
  const dir = "apple_data/";
  const file_name = url.split("/").pop() + ".html";

  // get's the html from the urls
  request(url, function(error, response, body){
    if(!error){
      fs.writeFile(dir + file_name, body, (err) => {
        if (err) throw err;
        console.log(`${file_name} has been saved to ${dir}`);
      });
    } else {
      console.log(error);
    }
  })
})
