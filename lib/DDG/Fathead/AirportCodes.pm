package DDG::Fathead::AirportCodes;

use DDG::Fathead;

primary_example_queries "NRT"

secondary_example_queries
	"PAD",
	"JFK";

description "IATA Airport Codes";

name "AirportCodes";

icon_url "/i/www.github.com.ico";

source "GitHub";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/airport_codes";

topics "travel", "geography";

category "ids", "geography";

attribution
	github => ['https://github.com/ponzellus', 'ponzellus'];

1;
