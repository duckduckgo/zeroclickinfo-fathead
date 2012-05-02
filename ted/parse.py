import csv

ted = csv.reader(open('download/ted.csv', 'rb'))
talks = [talk for talk in ted]
for talk in talks[1:]:
    print talk