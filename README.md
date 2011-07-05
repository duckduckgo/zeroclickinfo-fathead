DuckDuckGo ZeroClickInfo FatHeads
=================================

About
-----

See https://github.com/duckduckgo/duckduckgo/wiki for a general overview on contributing to DuckDuckGo.

This repository is for contributing static, keyword based content to 0-click, e.g. getting a perl function reference when you search for perl split. 


Contributing
------------

This repository is organized by type of content, each with its own directory. Some of those projects are in use on the live system, and some are still in development.

Inside each directory are a couple of different files for specific cases. 

* project/fetch.xx

This script is called to fetch the data. 

* project/parse.xx

This is the script used to parse the data once it has been fetched.


Output Formats
--------------

Please name the output file project.tsv (tab delimited) but do not store the data file(s) in the repository (as noted above).

The output format from parse.xx depends on the type of content. In any case, it should be a tab delimited file, with one line per entry. Usually there is no need for newline characters, but if there is a need for some reason, escape them with a backslash like \\n.

For programming reference, the fields are:

* my $page = $line[0] || ''; -- REQURIED: this is the name of the function.
* my $namespace = $line[1] || ''; -- Usually blank unless for something like JavaScript
* my $url = $line[2] || ''; -- REQUIRED: this is the target URL for more information.
* my $description = $line[3] || ''; -- SOME COMBO OF THESE IS REQUIRED.
* my $synopsis = $line[4] || ''; -- Look at https://duckduckgo.com/?q=perl+split
* my $details = $line[5] || ''; -- The part in grey is the $synopsis and the stuff below is the $description
* my $type = $line[6] || ''; -- usually blank
* my $lang = $line[7] || ''; -- usually blank