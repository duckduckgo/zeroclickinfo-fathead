import csv
import urllib

ted = csv.reader(open('download/ted.csv', 'rb'))
talks = [talk for talk in ted]
for talk in talks[1:]:
    title = talk[4]
    article = 'A'
    categories = talk[6]
    see_also = "[" + talk[3] + "]"
    speaker = talk[3].lower()
    external_links = "[" + talk[0] + talk[1] + " Official site]\\n" + \
                      "[http://www.ted.com/speakers/" + (speaker).replace(" ", "_") + \
                      ".html TED Profile]\\n"
    abstract = talk[5]
    print "{title}\t{article}\t\t\t{categories}\t\t{see_also}\t\t{external_links}\t{abstract}\n".format(
    title=title, article=article, categories=categories, see_also=see_also, external_links=external_links, abstract=abstract)
