// Introduction:
// -------------
// This is used to crawl swiftdoc.org to create a Fathead output
// that's specified in http://docs.duckduckhack.com/resources/fathead-overview.html
//
// Page Structure:
// ---------------
// Crawling swiftdoc.org is simple. All the links can be found in the root domain (index.html)
// so all we have to do is to collect all those links and send them off to processing.
//
// Pipeline:
// ---------
// Fetch -> Scrape / Process -> Output
//
// TODO:
// - Create disambiguations
//
// Updates:
// --------
// July 27, 2016: Initial version. Working with Swift 2.2. Articles and redirects working.

const req = require('request');
const cheerio = require('cheerio');
const sanitizeHtml = require('sanitize-html');
const fs = require('fs');

// This is the Swift version number that we'll be parsing.
const version = "v2.2";
const domain = "http://swiftdoc.org";
const rootURL = `${domain}/${version}/`;

// These are the selectors that we'll be using to scrape the needed data.
const selectors = {
  indexLinks: ".container-fluid.home-block a",
  codeSnippet: ".intro-declaration",
  description: ".discussion.comment",
  func: "div[id*='func-'].declaration",
  propDescription: ".comment",
  instanceVars: "div[id*='static-'].declaration",
  vars: "div[id*='var-'].declaration"
}

// Find all the articles.
//
// The homepage contains all of the links to each of the articles.
// However, not all links on index.html link to an article so we
// specify `.container-fluid.home-block a` to get a list of all
// the links to the article.
const fetch = (callback) => {
  request(rootURL, (body) => {
      const $ = cheerio.load(body);

      // Get all the links on the front-page then pass
      // the links that we get to the callback function for scraping.
      let links = [];
      $(selectors.indexLinks).each((_, el) => {
        let link = $(el).attr("href");
        links.push(link);
      });

      callback(links);
  });
};

// Scrape each article to get the data.
//
// Most articles have a snippet of code and a comment (specifically the types and protocols).
// We can easily select those sections of the page and parse out the unnecessary
// tags. We only keep <pre> and <code> tags.
fetch(links => {
  // We make a request for each link that we have so that
  // we can scrape each one.
  const scrape = link => {
    request(`${domain}${link}`, (body) => {

      // We create a Fathead object which contains all of the articles
      // and redirects that we have in the page. We will later process this to create the
      // output file.
      let Fathead = {
        articles: [],
        redirects: [],
      }

      const $ = cheerio.load(body);

      // ===================================================================
      // Scraping: First Pass 🔨
      // ===================================================================
      //
      // We go through the all the links that we got and we create entries for each
      // of them (provided they have any content in them that we can use).
      let article = {
        title: $("header").text().trim(),
        url: `${domain}${link}`,
        description: ''
      };

      // Sections "Protocols" and "Types" would usually fall into this block.
      [selectors.codeSnippet, selectors.description].forEach((selector, index) => {
        let html = sanitize($(selector)) || '';
        // The first entry is always the code snippet. We wrap that in a <pre> tag.
        if(index === 0 && html) { html = preWrap(html); }
        article.description += html;
      });

      // Sections "Operators" and "Globals" usually fall in here since most of them
      // lack a description.
      if(!article.description) {
        let description = sanitize($("code").eq(0));
        if(/operator/.test(description)) {
          article.description = preWrap(`<code>${description}</code>`);
        }
      }

      // Don't save it if it really doesn't have a description.
      if(article.description) {
        Fathead.articles.push(article);
      }

      // ===================================================================
      // Scraping: Second Pass ⚒
      // ===================================================================
      //
      // Now each of the pages can have functions and variables. These can
      // be made into individual articles as well. For example, searching for
      // "swift forEach" should return "SequenceType.forEach" as the article.
      let options = {
        parentArticle: article.title,
        $: $,
        link: link,
      };

      // Get the functions
      [options.selector, options.re] = [selectors.func, /func ([a-zA-Z]+)/];
      Fathead.articles = Fathead.articles.concat(findProperties(options));

      // Get the static variables
      [options.selector, options.re] = [selectors.instanceVars, /static var ([a-zA-Z]+)/];
      Fathead.articles = Fathead.articles.concat(findProperties(options));

      // Get the variables
      [options.selector, options.re] = [selectors.vars, /var ([a-zA-Z]+)/];
      Fathead.articles = Fathead.articles.concat(findProperties(options));

      // ===================================================================
      // Processing: Making Redirects ⚙
      // ===================================================================
      //
      // Finally let's make redirects. For example, "forEach" should redirect to "SequenceType.forEach"
      Fathead.articles.forEach(a => {
        if(/\./.test(a.title)) {
          let prop = a.title.match(/\.([a-zA-Z]+)$/);
          if(prop && prop.length > 1) {
            Fathead.redirects.push({
              title: prop[1],
              redirect: a.title,
            });
          }
        }
      });

      // ===================================================================
      // Processing: Making Disambiguations (TODO) ⚙
      // ===================================================================
      //
      // ???


      // ===================================================================
      // Write the file 📜
      // ===================================================================
      //
      // Finally, we write the contents to output.txt
      Fathead.articles.forEach(a => {
        fs.appendFileSync("output.txt", `${a.title}\tA\t\t\t\t\t\t\t\t\t\t${a.description}\t${a.url}\n`);
      });

      Fathead.redirects.forEach(r => {
        fs.appendFileSync("output.txt", `${r.title}\tR\t${r.redirect}\t\t\t\t\t\t\t\t\t\t\n`);
      });
    });
  }

  links.forEach(scrape);
});

// ===================================================================
// Utility Functions 🔧
// ===================================================================
//
// We declare these with the old function declaration so that they get hoisted up.

// This removes unnecessary tags.
// The function removes all the other tags in the HTML
// except for the <code> and <pre> tags. This also escapes newlines.
function sanitize(html) {
  return sanitizeHtml(html.html() || '', {
    allowedTags: [ 'code', 'pre', 'strong' ],
    allowedAttributes: {}
  }).trim().replace(/\n+/g, "\\n");
};

// This is simply a helper function which wraps HTML in a <pre> tag.
function preWrap(html) {
  return `<pre>${html}</pre>`;
}

// This is an abstraction to the `request` module.
// All we need to know is if we got it successfully or not.
function request(url, success) {
  req(url, (_, resp, body) => {
    if(resp.statusCode === 200) {
      success(body);
    } else {
      console.log(`Failed to load ${url}. Exiting.`);
      process.exit(1);
    }
  });
}

// This is a function for abstracting away the details that we need to
// get all the functions and variable declarations on the page that we're scraping.
function findProperties(options) {
  let articleList = [];

  options.$(options.selector).each((_, el) => {
    let strippedName = options.$(".toggle-link", el).text().match(options.re);
    let id = options.$(el).attr("id");

    // We make sure that we found a match with our regexp.
    if(strippedName && strippedName.length > 1) {
      let article = {
        title: `${options.parentArticle}.${strippedName[1]}`,
        url: `${domain}${options.link}#${id}`,
        description: sanitize(options.$(selectors.propDescription, el)),
      };

      // We don't want to duplicate entries that have already been decleared
      // somewhere else.
      if(!/Declared In/.test(article.description)) {
        articleList.push(article);
      }
    }
  });

  return articleList;
}
