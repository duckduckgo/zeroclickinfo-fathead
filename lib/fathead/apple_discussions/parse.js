
const fs = require('fs');
const cheerio = require('cheerio');
const colors = require('colors');

let urls = []; // the urls for scraping
let result_data = {}; // the end product. an array of js objects

const datadir = "apple_data/";

fs.readdirSync(datadir).forEach(function(file) {

  console.log(`Scraping ${file}`)
  scrape_information(
    load_files(datadir + file), file
  )
});

// loads the html files
function load_files(f) {

  try {
    let contents = fs.readFileSync(f, 'utf8');
    return contents;
  } catch(e) { throw e; }
};

// parses the html documents
async function scrape_information(data, id) {

  $ = cheerio.load(data, {
    ignoreWhiteSpace: true,
    xmlMode: true
  });

  let local_obj = {};
  const apple_id = id.split('.').shift();
  local_obj.url = 'https://discussions.apple.com/thread/' + apple_id;

  // Outputs the title
  const title = $('h1').text().trim().replace(/^Q:\s/, '');
  console.log('Title: ' + title);
  local_obj.title = title;

  // Outputs if Apple Recommended
  if ($('.marker-text').eq(0).text().trim() == 'Apple recommended') {
    console.log(colors.yellow('- Apple Recommended'));
    local_obj.recommended = true;
  } else {
    console.log('- Not Apple Recommended');
    local_obj.recommended = false;
  }

  // Outputs if Helpful
  if ($('.marker-text').eq(0).text().trim() == 'Helpful') {
    console.log(colors.magenta('- Helpful'));
    local_obj.helpful = true;
  } else {
    console.log('- Not Helpful');
    local_obj.helpful = false;
  }

  // outputs if solved
  if ($('.marker-text').eq(0).text().trim() == 'Solved') {
    console.log(colors.green('- Solved'));
    local_obj.solved = true;
  } else {
    console.log('- Not Solved');
    local_obj.solved = false;
  }

  result_data[apple_id] = local_obj;
}

fs.writeFile('output.json', JSON.stringify(result_data), (err) => {
  if (err) throw err;
  console.log(`output.json has been saved`);
});
