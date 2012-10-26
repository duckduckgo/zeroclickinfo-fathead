#!/usr/bin/python
import mimetypes
mimetypes.init()
OUTPUT = "output.txt"
fout = open(OUTPUT,"w")
for extension, mimetype in mimetypes.types_map.iteritems():
	fout.write("\t".join([extension, "A", "", "", "", "", "", "", "", "", "", "The MIME type for the extension <code>"+extension+"</code> is <code>"+mimetype+"</code>", "http://hg.python.org/cpython/file/2.7/Lib/mimetypes.py"])+"\n")
