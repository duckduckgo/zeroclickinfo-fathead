package DDG::Fathead::Sysctl::Proc::Sys::Kernel;
#ABSTRACT: 'Fathead generator for documentation of the sysctl files in /proc/sys/kernel/';

use DDG::Fathead;

primary_example_queries 'iata vie';
secondary_example_queries
    'sysctl cap_last_cap',
    'dmesg_restrict',
    '/proc/sys/kernel osrelease',
    '/proc/sys/kernel/shmmax',
    'proc sys kernel unknown_nmi_panic',
    'linux panic_on_stackoverflow';
description 'Documentation for the sysctl files in /proc/sys/kernel/';
name 'Sysctl::Proc::Sys::Kernel';
source 'https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl/kernel.txt';
code_url 'https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/Sysctl/Proc/Sys/Kernel';
category 'computing_tools';
topics 'computing', 'sysadmin', 'special_interest';
attribution github => ['http://github.com/nospampleasemam', 'nospampleasemam'],
               web => ['http://dylansserver.com/', 'Dylan Lloyd'];

1;
