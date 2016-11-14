var cheerio = require('cheerio'),
    htmlclean = require('htmlclean');;

module.exports = {
    getAbstract: getAbstract,
    getCategory: getCategory
}

function getAbstract(data) {
    var $ = cheerio.load(data);
    var descriptionElement = $('.api-profile-description');
    if (descriptionElement.children().length === 0) return '';
    
    var abstract = '<p>' + descriptionElement.children().first().html().trim() + '</p>';
    abstract+= getUsage();
    return '<section class="prog__container">' + htmlclean(abstract).replace(/\r?\n+/g, '\\n') + '</section>';
    
    function getUsage() {
        var usageElement = $('.usage');
        if (usageElement.length === 0) {
            return '<pre><code>' + $('#usage').siblings('p').find('code').html() + '</pre></code>';
        }

        return '<p>Usage</p>' + usageElement.html();
    }
}

function getCategory(data) {
    var $ = cheerio.load(data);
    return $('.api-profile-header-structure > li').last().text()
        .trim()
        .substring(2);
}