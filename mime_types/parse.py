#!/usr/bin/python
import mimetypes
mimetypes.init()
OUTPUT = "output.txt"
fout = open(OUTPUT,"w")
for extention, mimetype in mimetypes.types_map.iteritems():
	fout.write("\t".join(["MIME type "+extention, "A", "", "", "", "", "", "", "", "", "", "", 
	"The MIME type for this "+extention+" is "+mimetype, ""])+"\n")
