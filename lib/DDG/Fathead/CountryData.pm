package DDG::Fathead::CountryData;
# ABSTRACT: Provides economic and demographic data for a given country

use DDG::Fathead;

primary_example_queries "india gdp", "united states gdp", "india population", "population of india";

secondary_example_queries
    "india number of people",
    "india economy size";

description "Economic and demographic data for a given country";

name "CountryData";

icon_url "http://data.worldbank.org/profiles/datafinder/themes/datum/favicon.ico";

source "World Bank";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/country_data";

topics "everyday", "economy_and_finance";

category "reference";

attribution
    twitter => "rohitguptahpf",
    github => ["rohit-gupta", "Rohit Gupta"];

1;