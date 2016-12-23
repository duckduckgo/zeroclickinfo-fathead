Plugin: MDN JavaScript Reference
Author: Eric Edens (eric.edens@gmail.com)
Date: May 1, 2013


This Fathead plugin provides documentation snippets for JavaScript. It covers
the core language, as covered in the Mozilla Developer Network (MDN); it 
currently does not support non-core APIs such as the DOM or XMLHttpRequest.

The plugin's core data source is MDN's JavaScript 1.5 reference, 
https://developer.mozilla.org/en-US/docs/JavaScript/Reference. From each wiki
article, it extracts:
  - Title
  - Summary
  - Syntax or example

It then combines Syntax and Summary to create an abstract for the article.

As MDN is a wiki, a core issue is consistency of article titles. This plugin
utilizes an auxiliary reference (propref.csv) in order to standardize titles.
For implementation details, see parse.Standardizer

Dependencies:
  Python 2.7
  lxml, https://pypi.python.org/pypi/lxml
