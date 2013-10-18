package DDG::Fathead::Haxelib;

use DDG::Fathead;

primary_example_queries "haxelib openfl";

secondary_example_queries
    "nme haxe library",
    "nodejs haxelib";

description "Lookup packages from the Haxe libraries";

name "Haxelib";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/haxelib";

icon_url "/i/www.haxe.org.ico";

source "Haxelib";

topics "programming";

category "programming";

spice to => 'http://haxelib-json.herokuapp.com/package/$1?callback={{callback}}';

attribution
	github => ['https://github.com/TopHattedCoder','TopHattedCoder'];
