package DDG::Fathead::Anagram;

use DDG::Fathead;

primary_example_queries "anagram airplane 5";

secondary_example_queries
    "anagram airplane 5 10",
    'anagram: "duck duck go" 4';

description "Anagram finder within a range";

name "Anagram";

icon_url "";

# don't know what to put here
source "?";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/anagram";

topics "crytography", "trivia", "words_and_games";

category "language";

attribution
    github => ['https://github.com/maurizzzio', 'maurizzzio'],
    twitter => ['https://twitter.com/iMauricio', 'Mauricio Poppe'];

1;
