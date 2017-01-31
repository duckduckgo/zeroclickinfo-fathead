package DDG::Fathead::HTTPStatusCodes;

use DDG::Fathead;

primary_example_queries "HTTP 200";

secondary_example_queries
    "200 HTTP",
    "HTTP code 200",
    "HTTP status code 200",

description "Obtain information about the HTTP status codes";

name "HTTPStatusCodes";

icon_url "";

source "Wikipedia";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/http_status_codes";

topics "geek", "sysadmin", "programming";

category "computing_info";

attribution
    github => ['https://github.com/BaltoRouberol', 'BaltoRouberol'],
    twitter => ['https://twitter.com/brouberol', 'brouberol'];

1;
