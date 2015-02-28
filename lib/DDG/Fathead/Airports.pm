package DDG::Fathead::Airports;

use DDG::Fathead;

primary_example_queries "iata vie";
secondary_example_queries
    "icao jfk",
    "iata charles du galle";
description "Airport IATA/ICAO codes";
name "Airports";
source "Wikipedia";
code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/airports";
topics "special_interest";
category "reference";
attribution
    github => ['https://github.com/mellon85', 'mellon85'];

1;
