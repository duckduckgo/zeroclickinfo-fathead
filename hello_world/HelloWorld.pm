package DDG::Fathead::HelloWorld;

use DDG::Fathead;

primary_example_queries "hello world perl";

secondary_example_queries
    "javascript hello world",
    "hello world in c";

description "Hello World programs in many program languages";

name "HelloWorld";

icon_url "/i/www.github.com.ico";

source "GitHub";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/hello_world";

topics "geek", "programming";

category "programming";

attribution
    twitter => ['https://twitter.com/jperla', 'jperla'],
    web => ['http://www.jperla.com/blog', 'Joseph Perla'];

1;
