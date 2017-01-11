About:
-------------
data.url links to the page in the documentation for iText 7.0.1 which lists hyperlinks
to each of the classes in the API.  The page associated with each of these links is then placed in 
the downloads folder.

The script parses the class name and description from each of the downloaded Java Doc pages.  The
documentation is sparse in places, so if a description is not provided, the classname is not saved
in output.txt. 

Dependencies:
-----
This script requires python 3 along with the requests and Beautiful Soup 4 libraries.
