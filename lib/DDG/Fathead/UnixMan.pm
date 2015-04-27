package DDG::Fathead::UnixMan;

use DDG::Fathead;

primary_example_queries "man nmap";

secondary_example_queries
    "wc man";

description "UNIX man page reference";

name "UnixMan";

icon_url "/i/www.linuxcommand.org.ico";

source "LinuxCommand.org";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/unix_man";

topics "geek", "sysadmin";

category "software";

attribution
    github => ['https://github.com/flaming-toast', 'flaming-toast'],
    web => ['http://ftoast.org', 'Jessica Yu'];

1;
