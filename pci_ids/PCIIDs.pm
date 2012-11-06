package DDG::Fathead::PCDIDs;

use DDG::Fathead;

primary_example_queries => "pci 8086 0100 1028 04aa";

secondary_example_queries =>
    "8086 0100 1028 04aa",
    "pciid 4444 0016",
    "8086 0100 1028 04aa pci id";

description "Reference for IDs used in PCI devices";

name "PCIIDs";

source "pciids.sourceforge.net";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/pci_ids";

topics => "geek", "sysadmin", "special_interest";

category => "reference";

attribution
    github => ['https://github.com/nospampleasemam', 'nospampleasemam'],
    web => ['http://dylansserver.com', 'Dylan Lloyd'];

1;
