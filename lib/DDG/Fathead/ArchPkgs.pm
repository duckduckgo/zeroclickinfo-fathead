package DDG::Fathead::ArchPkgs;

use DDG::Fathead;

primary_example_queries "arch awesome";

secondary_example_queries
    "pacman rxvt",
    "gcc arch package",
    "nmap arch linux package",
    "arch linux firefox";

description "Arch Linux packages";

name "ArchPkgs";

icon_url "/i/www.archlinux.org.ico";

source "Arch Linux";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/arch_pkgs";

topics "geek", "sysadmin", "special_interest";

category "software";

attribution
    github => ['https://github.com/mrshu', 'mrshu'],
    twitter => ['https://twitter.com/mr__shu', 'mr__shu'];

1;
