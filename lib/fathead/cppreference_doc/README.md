Cppreference Fathead
===========================

This is a Fathead for C++ standard library reference manual hosted at
http://en.cppreference.com.

Data Source
-----------

fetch.sh downloads and untars a snapshot of the website, which is available at
http://en.cppreference.com/w/Cppreference:Archives. A new snapshot is released periodically.

The location of the newest archive that is tested for DuckDuckGo is
http://upload.cppreference.com/w/Cppreference:DDG-link?action=raw.
This archive version maybe different from the latest public archive present at
http://en.cppreference.com/w/Cppreference:Archives.

The archive at http://upload.cppreference.com/w/Cppreference:DDG-link?action=raw
is updated by linking to the most recent cppreference archive that someone
actually tested to work, to ensure that the Fathead functionality does not
accidentally break when new cppreference archives are released.

Parse script
--------------

The parse scripts are located in cppreference-doc subdirectory. They were
originally maintained in the [cppreference-doc repository](https://github.com/p12tic/cppreference-doc),
but was copied here to simplify the process of the parse script development.

The scripts still implicitly depend on the contents and the format of the
archives that are uploaded to the cppreference.com website. Therefore, the
scripts should be occasionally synchronized between the repositories. Please
submit a PR to the cppreference-doc repository with any changes that land here
first.

Dependencies
------------

* python 3
* python 3 lxml
