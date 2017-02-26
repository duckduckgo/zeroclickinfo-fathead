Introduction:
-------------
This is used to crawl swiftdoc.org to create a Fathead output
that's specified in http://docs.duckduckhack.com/resources/fathead-overview.html

See: https://forum.duckduckhack.com/t/create-swiftdoc-instant-answer/242

Page Structure:
---------------
Crawling swiftdoc.org is simple. All the links can be found in the root domain (index.html)
so all we have to do is to collect all those links and send them off to processing.

Pipeline:
---------
Fetch -> Scrape / Process -> Output

Screencaps:
--------

<img width="712" alt="screen shot 2016-07-27 at 3 04 21 pm" src="https://cloud.githubusercontent.com/assets/81969/17188568/b05d6ac2-540b-11e6-9233-57b0a002e400.png">
<img width="438" alt="screen shot 2016-07-27 at 3 04 32 pm" src="https://cloud.githubusercontent.com/assets/81969/17188569/b05d788c-540b-11e6-83b5-fffe0156c606.png">

What's missing?
----

Disambiguation is currently missing but I think we can make a PR for that instead in the future. What we have here should be an MVP.

How do I test this?
-----
You need `node` and `npm` installed on your machine. After that, run:

`cd lib/fathead/swift && npm install && ./parse.sh`