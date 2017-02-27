About:
-------------
The fetch script downloads the documentation for every hyperlink on the All Classes page in the documentation for iText 7.0.1 to each of the classes in the API.  The page associated with each of these links is then placed in the downloads folder.

The script parses the class name and description from each of the downloaded Java Doc pages.  It then scrapes each method in the methods section of the page.  If the method is overloaded, only the version with the longest description is included in output.txt  The documentation is sparse in places, so if a description is not provided, the classname is not saved in output.txt. 

Dependencies:
-----
This script requires python 3 along with the requests and Beautiful Soup 4 libraries.  The 
libraries can be installed using pip3 install -r requirements.txt
