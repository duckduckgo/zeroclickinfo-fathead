package DDG::Fathead::Nginx;

use DDG::Fathead;

primary_example_queries "nginx access_log";

secondary_example_queries
    "auth_basic nginx";

description "Nginx reference";

name "NginxDoc";

icon_url "/i/nginx.org.ico";

source "Nginx Documentation";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/nginx";

topics "geek", "programming", "sysadmin";

category "reference";

attribution
    github => ['pfirsichbluete', 'Pfirsichbluete'];

1;
