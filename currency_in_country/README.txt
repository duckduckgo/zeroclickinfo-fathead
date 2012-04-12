Currencies for each country are from: 
http://en.wikipedia.org/wiki/List_of_circulating_currencies

There are three options for output available now. 
One to generate option.txt another one to generate hash.txt and also one to generate copy_paste_hash.txt:

1) python parse.py
2) python parse.py -hash
3) python parse.py -copy		<-- preferred option 


DETAILS:

1) Output is the file (output.txt) with list of countries and currencies with iso code in ()
For example: 
Slovakia			Euro (EUR)

If there is more than one currency used by each country then each currency is in separate line
For example:
Zimbabwe			Botswana pula (BWP)				
Zimbabwe			British pound (GBP)	

2) Output is the file (hash.txt). Every country takes only one line so it is easy to import into Perl module.
Format is Country:Currency,Currency,... each on one line

For example:
Ascension Island:Ascension pound,Saint Helena pound (SHP)
Australia:Australian dollar (AUD)

3) Last option creates (copy_paste_hash.txt) with hash table ready for copy paste to 'CurrencyIn.pm' module.
This one seems to be preferred method. Country name is lowercased for better comparison with user input. But it gets capitalized in search results.
Output example:
"abkhazia"=>["Russian ruble (RUB)"],
"afghanistan"=>["Afghan afghani (AFN)"],
