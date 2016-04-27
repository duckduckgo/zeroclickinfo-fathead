package DDG::Fathead::SysctlProcSysKernel;

use DDG::Fathead;

primary_example_queries 'iata vie';
secondary_example_queries
    'sysctl cap_last_cap',
    'dmesg_restrict',
    '/proc/sys/kernel osrelease',
    '/proc/sys/kernel/shmmax',
    'proc sys kernel unknown_nmi_panic',
    'linux panic_on_stackoverflow';
description 'documentation for the sysctl files in /proc/sys/kernel/';
name 'SysctlProcSysKernel';
source 'https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl/kernel.txt';
code_url 'https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/SysctlProcSysKernel';
category 'computing_tools';
topics 'computing', 'sysadmin', 'special_interest';
attribution github => ['https://github.com/majuscule', 'majuscule'],
               web => ['https://disinclined.org', 'Dylan Lloyd'];

1;
