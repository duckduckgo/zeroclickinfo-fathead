__author__ = 'kvaranasi'
import csv
import os
import re

rootdir = os.getcwd()
rootdir += '/lib/fathead'
example = '/home/codio/workspace/zeroclickinfo-fathead/lib/fathead/hello_world/output.txt'
example_1 = '/home/codio/workspace/zeroclickinfo-fathead/lib/fathead/hello_world/output_1.txt'
count = 0

for subdir, dirs, files in os.walk(rootdir):
    for file in files:
        if file == 'output.txt':
            filepath = subdir + os.sep + file

            if filepath.endswith(".txt"):
                print (filepath)
example_rows = []
with open(example) as f:
    content = f.readlines()
    for each in content:
        example_rows.append(each.replace(r'\t', '    '))
with open(example_1, 'w+') as f:
    for each in example_rows:
        f.write(each)
