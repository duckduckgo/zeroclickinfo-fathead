var cheerio = require('cheerio'),
    htmlclean = require('htmlclean');;

module.exports = {
    getAbstract: getAbstract,
    getCategory: getCategory
}

function getAbstract(data) {
    var $ = cheerio.load(data);
    // Remove links
    $("a").each(function(){
        $(this).replaceWith($(this).text());
    });
    
    var descriptionElement = $('.api-profile-description');
    if (descriptionElement.children().length === 0) return '';
    
    var abstract = '<p>' + descriptionElement.children('p').html().trim() + '</p>';
    abstract+= getUsage();
    return '<section class="prog__container">' + htmlclean(abstract).replace(/\r?\n+/g, '\\n') + '</section>';
    
    function getUsage() {
        var usageElement = $('.usage');
        if (usageElement.length !== 0)
            return '<h2>Usage</h2>' + usageElement.html();
        
        usageElement = $('#usage');
        if (usageElement.length !== 0)
            return '<h2>Usage</h2>' + '<pre><code>' + usageElement.siblings('p').find('code').html() + '</code></pre>';
        
        usageElement = $('h2:contains("Usage")');
        if (usageElement.length !== 0) {
            var text = usageElement.nextUntil('section.api-section').toArray()
                .reduce(function(acc, element) {
                    acc += $.html(element);
                    return acc;
                }, '');
            
            return text? '<h2>Usage</h2>' + text : '';
                
        }
        return '';
    }
}



function getCategory(data) {
    var $ = cheerio.load(data);
    return $('.api-profile-header-structure > li').last().text()
        .trim()
        .substring(2);
}