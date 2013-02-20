package DDG::Fathead::CVE;

use DDG::Fathead;

primary_example_queries 'cve-2009-1234';

secondary_example_queries
    'cve 2009-1234',
    '2009-1234 cve';

description 'Common Vulnerabilities and Exposures (CVE) reference';

name 'CVE';

icon_url '';

source 'CVE Details';

code_url 'https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/cve';

topics 'computing', 'programming', 'special_interest';

category 'reference';

attribution
    github => ['https://github.com/soh-cah-toa', 'soh-cah-toa'],
    web    => ['http://cybercrud.net', 'Kevin Polulak'];

1;
