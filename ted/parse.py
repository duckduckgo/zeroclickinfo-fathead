import csv
import urllib
from BeautifulSoup import BeautifulSoup

#Get the file stored in the download folder
ted = csv.reader(open('download/ted.csv', 'rb'))
talks = [talk for talk in ted]
output = open("output.txt", "w")

#Remove the top row and iterate
for talk in talks[1:]:
    title = talk[4]
    article = 'A'
    categories = talk[6]
    see_also = "[" + talk[3] + "]"

    #Show the speaker's webpage on TED
    speaker = talk[3].lower()
    speaker = "http://www.ted.com/speakers/" + (speaker).replace(" ", "_") + ".html"
    external_links = "[" + talk[0] + talk[1] + " Official site]\\n" + \
                     "[" + speaker + " TED Profile]\\n"

    #Summary is already provided
    abstract = talk[5]
    page = urllib.urlopen(speaker)
    soup = BeautifulSoup(page)

    #Images are probably in a tag with a name of rel and a value of image_src.
    #If it does not come from images.ted.com/images/ted, do not provide an image.
    image = soup.findAll(rel='image_src')
    if not len(image):
        image = ""
    elif image[0].find("http://images.ted.com/images/ted"):
        image = ""
    else:
        image = "[[Image:" + image[0]['href'] + "]]"

    #Write to file
    output.write("{title}\t{article}\t\t\t{categories}\t\t{see_also}\t\t{external_links}\t\t{image}\t{abstract}\t{relative_url}\n".format(
    title=title, article=article, categories=categories, see_also=see_also, external_links=external_links, image=image, abstract=abstract, relative_url=""))
output.close()