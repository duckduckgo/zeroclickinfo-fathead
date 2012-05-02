import csv

ted = csv.reader(open('download/ted.csv', 'rb'))
for talk in ted:
    print talk