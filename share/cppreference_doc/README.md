Cppreference fathead plugin
===========================

This is a fathead plugin for C++ standard library reference manual hosted at
http://en.cppreference.com.

Data source
-----------

fetch.sh downloads and untars a snapshot of the website, which is available at
http://en.cppreference.com/w/Cppreference:Archives. A new snapshot is released
periodically. The script needs to be for the new location of the snapshot.

Dependencies
------------

* python 3
* python 3 lxml
