#!/usr/bin/python

# Released under the GPL v2 license 
# https://www.gnu.org/licenses/old-licenses/gpl-2.0.html

import lxml.etree, lxml.html
import re
import codecs

url = "http://www.iso.org/iso/list-en1-semic-3.txt"
title = "ISO 3166 Country Codes"
article_type = "A"

outp = "output.txt"
inp = "raw.data"

#Open input file
input_file = open( inp, "r" )

#Read and throw out first line
input_file.readline()

output_file = codecs.open( outp, "w", encoding="utf-8")

#Loop thru the remainder of the file, format each line
#and print it to the output file.
for line in input_file.readlines() :
	line = line.strip()
	pair = line.split( ';' )
	if len( pair ) < 2 :
		continue;
	output_file.write( "\t".join ( [ pair[ 1 ],
									"",
									url,
									pair[0],
									"",
									"",
									"",
									"" ] ).decode('latin-1')
				 )
	output_file.write( "\n" )

input_file.close()
output_file.close()

