package DDG::Fathead::HTTPHeaders;
# ABSTRACT: Information about HTTP headers

use DDG::Fathead;
use utf8;

primary_example_queries "http header content-type";

secondary_example_queries
    "if-modified-since http",
    "user-agent http header";

description "Information about HTTP headers";

name "HTTPHeaders";

icon_url "/i/www.wikipedia.com.ico";

source "Wikipedia";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/http_headers";

topics "geek", "programming";

category "programming";

attribution
    twitter => ['https://twitter.com/andrewalker', 'andrewalker'],
    github => ['https://github.com/andrewalker', 'andrewalker'],
    web => ['https://andrewalker.net', 'André Walker'];

1;
