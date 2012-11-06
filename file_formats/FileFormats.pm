package DDG::Fathead::FileFormats;

use DDG::Fathead;

primary_example_queries => "file .cat";

secondary_example_queries =>
    "file extension .pdf",
    "psd file";

description "File extension descriptions";

name "FileFormats";

icon_url "/assets/icon_wikipedia.v101.png";

source "Wikipedia";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/file_formats";

topics => "everyday_goodies", "geek", "sysadmin";

categories => "computing_info", "software";

attribution
    github => ['https://github.com/whee', 'whee'],
    web => ['http://smaertness.net', 'Brian Hetro'];

1;
