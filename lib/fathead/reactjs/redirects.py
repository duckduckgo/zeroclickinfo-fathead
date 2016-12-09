import re

"""
Goes through the articles list and creates redirects
"""
class GenerateRedirects():

    def __init__(self, articles):
        # Saves redirects into a list
        self.redirects = []

        # Test for React.* redirects #597
        for article in articles:
            if article.lower().startswith('react.'):
                redirect = self.clarify(article[6:])
                self.redirects.append( "%s,%s" % (redirect, article) )

    def clarify(self, redirect):
        # Symbols such as dots replaced with spaces
        redirect = re.sub('\.', ' ', redirect)
        # split CamelCase only for methods
        if ' ' in redirect:
            redirect = "%s %s" % (redirect.rsplit(' ', 1)[0], (re.sub('(?!^)([A-Z]+)', r' \1', redirect.rsplit(' ', 1)[1])))
        elif '()' in redirect:
            redirect = re.sub('(?!^)([A-Z]+)', r' \1', redirect)
        # Parentheses removed
        redirect = re.sub('[()]', '', redirect)
        return redirect.lower()

    def get_redirects(self):
        # Returns the redirects List
        return self.redirects

"""
Retrieves articles from articles.txt file into a list,
generates redirects and writes them to the redirects.txt file
"""
if __name__ == "__main__":
    # Retrieves articles from articles.txt file into a list
    with open('articles.txt') as articlesFile:
        articles = [article.rstrip('\n') for article in articlesFile]

    # Gets redirects
    redirects = GenerateRedirects(articles).get_redirects()

    # Writes redirects to the redirects.txt file
    with open('redirects.txt', 'w+') as redirectsFile:
        redirectsFile.write('\n'.join(redirects))
