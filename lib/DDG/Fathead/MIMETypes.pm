package DDG::Fathead::MimeTypes;

use DDG::Fathead;

primary_example_queries ".jpg mime";

secondary_example_queries
    "mime type pdf",
    "mp4 mime";

description "MIME type reference";

name "MIMETypes";

icon_url "/assets/icon_wikipedia.v101.png";

source "Wikipedia";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/mime_types";

topics "geek", "web_design", "special_interest";

category "reference";

attribution
    github => ['https://github.com/vaishaks', 'vaishaks'],
    twitter => ['https://twitter.com/vaishaks', 'vaishaks'];

1;
