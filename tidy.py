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
        print each.count(r'\t'), each
        print each.replace(r'\t', '    ')
#             x = each.split(r'\t')
#             y = '    '.join(x)
#             example_rows.append(y)
# with open(example, 'rb') as csvfile:
#     spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')
#     for row in spamreader:
#         for each in row:
#             print each.split(r'\t')
# with open(example, 'wb') as csvfile:
#     spamwriter = csv.writer(csvfile)
#     spamwriter.writerows(example_rows)