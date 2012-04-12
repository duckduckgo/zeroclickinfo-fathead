Currencies for each country are from: 
http://en.wikipedia.org/wiki/List_of_circulating_currencies

There are two options for output available now. 
One to ganerate option.txt and another one to generate hash.txt:

1) python parse.py
2) python parse.py -hash

DETAILS:

1) Output is the file (output.txt) with list of countries and currencies with iso code in ()
All data are tab separated values.
For example: 
Slovakia			Euro (EUR)

If there is more than one currency used by each country then each currency is in separate line
For example:
Zimbabwe			Botswana pula (BWP)				
Zimbabwe			British pound (GBP)	

2) Output is the file (hash.txt). Every country takes only one line and so it is easy to import into 'CurrencyIn.pm' module:
https://github.com/Alchymista/zeroclickinfo-goodies/tree/master/lib/DDG/Goodie

Format is Country:Currency,Currency,... each on one line

For example:
Ascension Island:Ascension pound,Saint Helena pound (SHP)
Australia:Australian dollar (AUD)
