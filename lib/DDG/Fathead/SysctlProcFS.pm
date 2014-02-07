package DDG::Fathead::SysctlProcFS;

use DDG::Fathead;

primary_example_queries 'iata vie';
secondary_example_queries
    'sysctl cap_last_cap',
    'dmesg_restrict',
    '/proc/sys/kernel osrelease',
    '/proc/sys/kernel/shmmax',
    'proc sys kernel unknown_nmi_panic',
    'linux panic_on_stackoverflow';
description 'documentation for the sysctl files mounted on /proc/sys/';
name 'SysctlProcSysKernel';
source 'https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/plain/Documentation/sysctl';
code_url 'https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/SysctlProcFS';
category 'computing_tools';
topics 'computing', 'sysadmin', 'special_interest';
attribution github => ['http://github.com/nospampleasemam', 'nospampleasemam'],
               web => ['http://dylansserver.com/', 'Dylan Lloyd'];

1;
