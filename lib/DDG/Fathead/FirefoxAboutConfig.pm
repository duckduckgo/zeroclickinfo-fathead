package DDG::Fathead::FirefoxAboutConfig;

use DDG::Fathead;

primary_example_queries "firefox keyword.url";

secondary_example_queries
    "bidi.numeral",
    "browser.cache.disk.capacity about:config",
    "plugin.scan.plid.all firefox config";

description "Firefox about:config information";

name "FirefoxAboutConfig";

icon_url "/i/mozillazine.org.ico";

source "MozillaZine";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/firefox_about_config";

topics "geek", "sysadmin", "special_interest";

category "software";

attribution
    github => ['https://github.com/nospampleasemam', 'nospampleasemam'],
    web => ['http://dylansserver.com', 'Dylan Lloyd'];

1;
