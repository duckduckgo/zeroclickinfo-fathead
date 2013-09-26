#!/usr/bin/env python2
import mimetypes
mimetypes.init()
OUTPUT = "output.txt"
fout = open(OUTPUT,"w")
for extension, mimetype in mimetypes.types_map.iteritems():
	fout.write("\t".join([extension, "A", "", "", "", "", "", "", "", "", "", "The MIME type for the extension %s is %s." % (extension, mimetype), "http://en.wikipedia.org/wiki/MIME_type#List_of_common_media_types"])+"\n")
