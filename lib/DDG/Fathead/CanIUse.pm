package DDG::Fathead::CanIUse;

use DDG::Fathead;

primary_example_queries "caniuse indexeddb";

secondary_example_queries
    "caniuse contenteditable",
    "caniuse css-gradients";

description "Can I Use - Browser feature support";

name "CanIUse";

icon_url "/i/caniuse.com.ico";

source "GitHub";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/caniuse";

topics "geek", "web_design";

category "reference";

attribution
    github => ['https://github.com/csytan', 'csytan'],
    web => ['http://csytan.com', 'Chris Tan'];

1;
