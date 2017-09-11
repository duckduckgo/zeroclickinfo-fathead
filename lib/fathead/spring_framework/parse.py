#!/usr/bin/python
import os
import parse_utils
from collections import defaultdict

KEY = 0

if __name__ == '__main__':
    # delete previous output
    if os.path.exists('output.txt'):
        os.remove('output.txt')

    output = defaultdict(list)
    # iterate classes
    for f in parse_utils.collect_doc_files_from('./docs/javadoc-api/org'):
        print(f)
        # Remove root directory and filetype
        class_path = f[19:-5]

        data = parse_utils.get_docs(f, class_path)
        output[data[KEY]].append(data)

    for value in output.values():
        parse_utils.output("output.txt", value)

