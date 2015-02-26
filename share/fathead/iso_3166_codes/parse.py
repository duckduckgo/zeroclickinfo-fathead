#!/usr/bin/python
# -*- coding: utf-8 -*-

# Released under the GPL v2 license 
# https://www.gnu.org/licenses/old-licenses/gpl-2.0.html

import lxml.etree, lxml.html
import re

from unidecode import unidecode

url = "http://www.iso.org/iso/list-en1-semic-3.txt"
title = "ISO 3166 Country Codes"
article_type = "A"

outp = "output.txt"
inp = "download/raw.data"

#Open input file
input_file = open( inp, "r" )

#Read and throw out first line
input_file.readline()

output_file = open( outp, "w")

#Loop thru the remainder of the file, format each line
#and print it to the output file.
for line in input_file.readlines() :
	line = line.strip();
	pair = line.split( ';' );
	if len( pair ) < 2 :
		continue;

        pair[0] = unidecode(pair[0])
        
        abstract = "\"" + pair[1] + "\" is the ISO 3166 country code for \"" + pair[0].title() + ".\""
	
        output_file.write( "\t".join([
            pair[1],        # Title
            article_type,   # Type
            '',             # Redirect
            '',             # Other uses
            '',             # Categories
            '',             # References
            '',             # See also
            '',             # Further reading
            '',             # External links
            '',             # Disambiguation
            '',             # Images
            abstract,       # Abstract
            url,            # Source URL
            ] ))

        output_file.write( "\n" );

input_file.close();
output_file.close();

