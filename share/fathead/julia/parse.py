from bs4 import BeautifulSoup
import re

class JuliaPackage:
    """Extracts information about the package"""
    def __init__(self, package):
        self.name = package.find_all("a")[0].text
        self.href = package.find_all("a")[0].get("href")
        self.description = package.find("h4").text
        self.version = package.select("p a")[0].text
        self.licence = package.select("p a")[1].text
        self.author = package.select("p a")[2].text
        self.stars = package.find("span",attrs={"title":"GitHub stars"}).text.strip()

    def __str__(self):
        fields = [
            self.name,
            "A",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            '<br>'.join([
                u"Description: {}".format(self.description),
                u"Version: {}".format(self.version),
                u"Author: {}".format(self.author),
                u"Licence: {}".format(self.licence),
                u"GitHub stars: {}".format(self.stars)]
            ),
            self.href
        ]
        return '%s' % ('\t'.join(fields))

    def has_redirect(self):
        return len(self.split_camel_case(self.name)) > 1

    def redirect_str(self):
        return ' '.join(
            [' '.join(self.split_camel_case(self.name)),
             "R","","","","","","","","","","",""]
        )

    def split_camel_case(self, name):
        s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
        return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower().split('_')

if __name__ == '__main__':
    doc = open("download/packages.html", "r").read()
    soup = BeautifulSoup(doc, "html.parser")
    packages = soup.find_all("div",class_="pkglisting")

    with open('output.txt', 'w') as output:
        for p in packages:
            x = JuliaPackage(p)
            output.write(x.__str__().encode('utf-8') + "\n")
            if x.has_redirect:
                output.write(x.redirect_str().encode('utf-8') + "\n")