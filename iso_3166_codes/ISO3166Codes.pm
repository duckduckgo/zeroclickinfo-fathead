package DDG::Fathead::ISO3166Codes;

use DDG::Fathead;

primary_example_queries "iso tk";

secondary_example_queries
    "us iso";

description "ISO 3166 country codes";

name "ISO3166Codes";

icon_url "/i/www.iso.org.ico";

source "ISO";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/iso_3166_codes";

topics "geography", "travel", "special_interest";

category "geography";

attribution
    github => ['https://github.com/cjfarrar', 'cjfarrar'],
    web => ['http://www.arrdor.com', 'Chris Farrar'];

1;
