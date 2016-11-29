#!/usr/bin/python
import os
import re
import parse_utils
from bs4 import BeautifulSoup

# delete previous output
if os.path.exists('output.txt'):
    os.remove('output.txt')

# Parse root file
content = BeautifulSoup(parse_utils.readRootFile(), 'html.parser')

# iterate classes
for f in parse_utils.collectDocFilesFrom('./docs/api'):
    classUrl = content.find_all(href=re.compile(f.replace("./docs/api/", "")))[0].get('href')
    parse_utils.output("output.txt", parse_utils.getDocs(f, classUrl))
