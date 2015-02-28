package DDG::Fathead::LegalDocs;

use DDG::Fathead;

primary_example_queries "Series AA Investors' Rights Agreement";

secondary_example_queries
    "nda doc",
    "legal series aa stock purchase agreement";

description "Legal documents";

name "LegalDocs";

icon_url "/i/www.docracy.com.ico";

source "Docracy";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/legal_docs";

topics "economy_and_finance", "special_interest";

category "special";

attribution
    github => ['https://github.com/megamattron', 'megamattron'],
    web => ['http://www.docracy.com/', 'Docracy'];

1;
