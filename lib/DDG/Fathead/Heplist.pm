package DDG::Fathead::Heplist;

use DDG::Fathead;

primary_example_queries "electron";

secondary_example_queries
    "top quark",
	"B0",
	"mcnumber 443";

description "Informations of particles known in the Standard Model of High Energy Physics";

name "Heplist";

icon_url "";

source "PDG";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/heplist";

topics "science", "special_interest";

category "physical_properties";

attribution
    github => ["https://github.com/ponzellus", "ponzellus"];

1;
