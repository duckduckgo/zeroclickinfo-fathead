import os

from bs4 import BeautifulSoup

DOCS_URL = "http://pandas.pydata.org/pandas-docs/stable/index.html"
PACKAGE_NAME = "numpy"

OUTPUT_FILE = "output.txt"

class Python_Data(object):
    """
    Object responsible for loading raw HTML data from Python docs:
    """
    def __init__(self, file):
        """
        Initialize PythonData object. Load data from HTML.
        """
        self.HTML = ""
        self.FILE = file
        self.load_data()

    def load_data(self):
        """
        Open the HTML file and load it into the object.
        """
        with open(self.FILE, 'r') as data_file:
            self.HTML = data_file.read()
