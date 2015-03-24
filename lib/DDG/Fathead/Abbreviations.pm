package DDG::Fathead::Abbreviations;

use DDG::Fathead;

primary_example_queries "what does ASAP mean";

secondary_example_queries
    "What does ASAP stand for",
    "What is ASAP short for";

description "database of acronyms, abbreviations and meanings";

name "Abbreviations.com";

icon_url "/i/abbreviations.com.ico";

source "Abbreviations.com";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/abbreviation_com";

topics "everyday", "special_interest";

category "reference";

attribution
    github => ['https://github.com/stands4', 'STANDS4'],
    twitter => ['https://twitter.com/justadded', 'STANDS4'];

1;
