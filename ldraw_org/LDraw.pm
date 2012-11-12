package DDG::Fathead::LDraw;

use DDG::Fathead;

primary_example_queries "3815c63.dat";

secondary_example_queries
    "ldraw 2545",
    "2594";

description "LDraw LEGO part tracker";

name "LDraw";

icon_url "http://www.ldraw.org/favicon.ico";

source "LDraw Parts Tracker";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/ldraw_org";

topics "special_interest";

category "reference";

attribution
    github => ['https://github.com/anoved', 'anoved'],
    twitter => ['https://twitter.com/anoved', 'anoved'],
    web => ['http://anoved.net', 'Jim DeVona'];

1;
