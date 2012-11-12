package DDG::Fathead::Bible;

use DDG::Fathead;

primary_example_queries "genesis 1:11";

secondary_example_queries
    "gen 1:12",
    "gen 1 12",
    "bible genesis 11:2",
    "gen 2:13 bible";

description "Bible verses";

name "Bible";

icon_url "/i/blueletterbible.org.ico";

source "Blue Letter Bible";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/bible";

topics "everyday", "special_interest";

category "reference";

attribution
    github => ['https://github.com/MithrandirAgain', 'MithrandirAgain'],
    twitter => ['https://twitter.com/mithrandiragain', 'mithrandiragain'];

1;
