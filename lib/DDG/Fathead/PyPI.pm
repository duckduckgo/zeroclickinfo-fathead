package DDG::Fathead::PyPI;

use DDG::Fathead;

primary_example_queries "pypi beautifulsoup";

secondary_example_queries
    "django pip",
    "pylons python package";

description "Python packages";

name "PyPI";

icon_url "/i/pypi.python.org.ico";

source "Python Package Index";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/pypi";

topics "geek", "sysadmin", "programming";

category "software";

attribution
    github => ['https://github.com/ezgraphs', 'ezgraphs'];

1;
