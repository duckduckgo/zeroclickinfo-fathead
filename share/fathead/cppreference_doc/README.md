Cppreference fathead plugin
===========================

This is a fathead plugin for C++ standard library reference manual hosted at
http://en.cppreference.com.

Data source
-----------

fetch.sh downloads and untars a snapshot of the website, which is available at
http://en.cppreference.com/w/Cppreference:Archives. A new snapshot is released
periodically. The location of the newest archive that is tested for DuckDuckGo
is upload.cppreference.com/w/Cppreference:DDG-link?action=raw.

Dependencies
------------

* python 3
* python 3 lxml
