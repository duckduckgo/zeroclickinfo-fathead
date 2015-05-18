package DDG::Fathead::MadeHow;
# ABSTRACT: Explains the manufacturing process on many products.

use DDG::Fathead;

primary_example_queries "make bulletproof vest";

secondary_example_queries
    "fire extinguisher",
    "optical fiber";

description "How products are made from http://www.madehow.com.";

name "MadeHow";

icon_url "/favicon.ico";

source "How Products Are Made";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/made_how";

topics "geek", "everyday", "science";

category "reference";

attribution
    github => ['marcantonio', 'Marc Soda'],
    twitter => 'marcantoniosr';

1;
