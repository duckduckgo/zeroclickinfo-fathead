Cppreference Fathead
===========================

This is a Fathead for C++ standard library reference manual hosted at http://en.cppreference.com.

Data Source
-----------

fetch.sh downloads and untars a snapshot of the website, which is available at
http://en.cppreference.com/w/Cppreference:Archives. A new snapshot is released periodically.

The location of the newest archive that is tested for DuckDuckGo is upload.cppreference.com/w/Cppreference:DDG-link?action=raw.
This archive version maybe different from the latest public archive present at http://en.cppreference.com/w/Cppreference:Archives.

The archive at upload.cppreference.com/w/Cppreference:DDG-link?action=raw is updated by linking to the most recent cppreference archive that someone actually tested to work, to ensure that the Fathead functionality does not accidentally break when new cppreference archives are released.

The parsing script [index2ddg.py](https://github.com/p12tic/cppreference-doc/blob/master/index2ddg.py) as well as other required scripts are present in the [cppreference-doc repository](https://github.com/p12tic/cppreference-doc). Changes can be made by sending a pull request to this repository.

Dependencies
------------

* python 3
* python 3 lxml