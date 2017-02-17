#!/usr/bin/python
import os
import re
import parse_utils
from bs4 import BeautifulSoup

# delete previous output
if os.path.exists('output.txt'):
    os.remove('output.txt')

# Parse root file
content = BeautifulSoup(parse_utils.read_root_file(), 'html.parser')

# iterate classes
for f in parse_utils.collect_doc_files_from('./docs/javadoc-api/org'):
    result = content.find_all(href=re.compile(f.replace("./docs/javadoc-api/", "")))
    classUrl = ""
    if len(result) != 0:
        classUrl = result[0].get('href')
    parse_utils.output("output.txt", parse_utils.get_docs(f, classUrl))
