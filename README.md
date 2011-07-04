DuckDuckGo ZeroClickInfo FatHeads
=================================

About
-----



Contributing
------------

We organize this repository in projects. Some of those projects are in use on the live system, some are still in development.

Each project has its own directory. Inside this directory we use different files for specific cases. For clientside operations we only support of course Javascript files with .js ending. On serverside operations we support .sh for Bash, .pl for Perl and .py for Python. The triggered scripts are called:

* project/fetch.xx

This script is called to fetch the data. It is called in a cycle we define in our live systems.

* project/daemon.xx

This script is for a daemon that should be running nonstop to deliver the data required for the FatHead

* project/parse.js

This is the clientside javascript that is used to parse the data
