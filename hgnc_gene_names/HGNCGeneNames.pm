package DDG::Fathead::HGNCGeneNames;

use DDG::Fathead;

primary_example_queries => "A1BG gene";

secondary_example_queries =>
    "UGRP",
    "hgnc G6PD";

description "Human gene information";

name "HGNCGeneNames";

icon_url "/i/www.genenames.org.ico";

source "HGNC";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/hgnc_gene_names";

topics => "science", "special_interest";

category => "reference";

attribution
    github => ['https://github.com/jrandall', 'jrandall'],
    twitter => ['https://twitter.com/joshulux', 'joshulux'];

1;
