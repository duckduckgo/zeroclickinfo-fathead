# -*- coding: utf-8 -*-
from os.path import join
import requests

from bs4 import BeautifulSoup

SCIKIT_LEARN_BASE_URL = 'http://scikit-learn.org/stable/auto_examples/'
SCIKIT_INDEX_URL = 'http://scikit-learn.org/stable/auto_examples/index.html'


def download_file(fetch_me):
    """
    Fetches a file in given url into the 'download' directory
    Args:
        fetch_me: URL to file

    Returns:
        local_filename: Path to local version of the downloaded file.
    """
    local_filename = join('download', fetch_me.split('/')[-1])
    r = requests.get(fetch_me, stream=True)
    with open(local_filename, 'wb') as f:
        for chunk in r.iter_content(chunk_size=1024):
            if chunk:
                f.write(chunk)
    return local_filename


if __name__ == "__main__":
    local_file = download_file(SCIKIT_INDEX_URL)
    with open(local_file, 'r') as index:
        soup = BeautifulSoup(index, 'html.parser')
        # Download everything in found in "examples" list
        for example_link in soup.select('#examples .caption-text a'):
            href_path = join(SCIKIT_LEARN_BASE_URL, example_link.get('href'))
            download_file(href_path)
