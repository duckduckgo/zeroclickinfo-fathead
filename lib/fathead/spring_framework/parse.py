#!/usr/bin/python
import os
import re
import parse_utils
from bs4 import BeautifulSoup
from collections import defaultdict


KEY = 0

if __name__ == '__main__':
    # delete previous output
    if os.path.exists('output.txt'):
        os.remove('output.txt')

    # Parse root file
    content = BeautifulSoup(parse_utils.read_root_file(), 'html.parser')

    output = defaultdict(list)
    # iterate classes
    for f in parse_utils.collect_doc_files_from('./docs/javadoc-api/org'):
        print(f)
        result = content.find_all(href=re.compile(f.replace("./docs/javadoc-api/", "")))
        classUrl = ""
        if len(result) != 0:
            classUrl = result[0].get('href')

        data = parse_utils.get_docs(f, classUrl)
        output[data[KEY]].append(data)

    for value in output.values():
        parse_utils.output("output.txt", value)

