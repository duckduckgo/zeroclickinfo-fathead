#!/usr/bin/python
import mimetypes
mimetypes.init()
OUTPUT = "output.txt"
fout = open(OUTPUT,"w")
for extension, mimetype in mimetypes.types_map.iteritems():
<<<<<<< HEAD
	fout.write("\t".join([extension, "A", "", "", "", "", "", "", "", "", "", "", 
	"The MIME type for the extension <code>"+extension+"</code> is <code>"+mimetype+"</code>", ""])+"\n")
=======
	fout.write("\t".join([extension, "A", "", "", "", "", "", "", "", "", "", "The MIME type for the extension <code>"+extension+"</code> is <code>"+mimetype+"</code>.", "http://hg.python.org/cpython/file/2.7/Lib/mimetypes.py"])+"\n")
>>>>>>> 7f54d32e86a45bec96751770df22543334337606
