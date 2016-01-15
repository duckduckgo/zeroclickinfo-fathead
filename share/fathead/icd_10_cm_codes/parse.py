url = "https://www.cms.gov/Medicare/Coding/ICD10/"
title = "International Classification of Diseases 10 Clinical Modification Code (ICD-10 CM)"
article_type = "A"

outp = "output.txt"
inp = "download/icd10cm_codes_2016.txt"  #reference the ICD file here

#Open input file
input_file = open(inp, "r" )

#Read and throw out first line
input_file.readline()

output_file = open( outp, "w")

#Loop thru the remainder of the file, format each line
#and print it to the output file.
for line in input_file.readlines() :
	line = line.strip();
	pair = line.split( '   ' );
	if len(pair ) < 2 :
		continue;

        pair[0] = unicode(pair[0])

        abstract = "ICD-10 Description: " + pair[1]

        output_file.write( "\t".join([
            pair[0],        # Title
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