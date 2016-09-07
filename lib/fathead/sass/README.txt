Dependencies:
- Python 2.7.6
- pip install beautifulsoup4==4.4.1

titles.txt:
The titles.txt file outlines all articles that are parsed. 
The file output is:
parsed title    Y/N    title    redirect

where parsed title is the article name parsed from file, 
Y/N is weather this title should be included, 
title is the article title (None is used is article title is same as parsed title), 
and redirect are all redirect names, sepereated by commas (None is used for no redirects). 
