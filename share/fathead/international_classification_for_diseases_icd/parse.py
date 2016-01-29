url = "https://www.cms.gov/Medicare/Coding/ICD10/"
title = "International Classification of Diseases Code (ICD-10)"
article_type = "A"

outp = "output.txt"
cm_inp = "download/icd10cm_codes_2016.txt"  # reference the ICD file here
pcs_inp = "download/icd10pcs_codes_2016.txt"

# Open input files
cm_input_file = open(cm_inp, "r")
pcs_input_file = open(pcs_inp, "r")

# Open output file
output_file = open(outp, "w")

# Read and throw out first line
cm_input_file.readline()

# Loop through the remainder of the file, format each line
# and print it to the output file.
for line in cm_input_file.readlines():

    line = line.strip();
    icd_cm_code = line[0:7]
    if len(icd_cm_code.strip()) > 3:
            icd_cm_alt_code = icd_cm_code[0:3]+"."+icd_cm_code[3:7]
    icd_cm_desc = line[8:200]
    print icd_cm_code
    print icd_cm_alt_code
    print icd_cm_desc


    icd_cm_code = unicode(icd_cm_code)

    abstract = "ICD-10 CM Description: " + icd_cm_desc

    output_file.write("\t".join([
        icd_cm_code,  # Title
        article_type,  # Type
        'R',  # Redirect
        icd_cm_alt_code,  # Other uses
        '',  # Categories
        '',  # References
        '',  # See also
        '',  # Further reading
        '',  # External links
        '',  # Disambiguation
        '',  # Images
        abstract,  # Abstract
        url,  # Source URL
    ]))

    output_file.write("\n");

    # Read and throw out first line
pcs_input_file.readline()

# Loop through the remainder of the file, format each line
# and print it to the output file.
for line in pcs_input_file.readlines():
    line = line.strip();
    icd_pcs_code = line[0:7]
    icd_pcs_desc = line[8:200]
    print line[0:7]
    print line[8:200]

    icd_pcs_code = unicode(icd_pcs_code)

    abstract = "ICD-10 Description: " + icd_pcs_desc

    output_file.write("\t".join([
        icd_pcs_code,  # Title
        article_type,  # Type
        '',  # Redirect
        '',  # Other uses
        '',  # Categories
        '',  # References
        '',  # See also
        '',  # Further reading
        '',  # External links
        '',  # Disambiguation
        '',  # Images
        abstract,  # Abstract
        url,  # Source URL
    ]))

    output_file.write("\n");


cm_input_file.close();
pcs_input_file.close();
output_file.close();
