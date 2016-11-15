from os.path import join
import re
import requests


from bs4 import BeautifulSoup


REACT_ROOT_URL = "https://facebook.github.io"
REACT_API_BASE_URL = "https://facebook.github.io/react/docs/hello-world.html"


def download_file(url):
    """
    Fetches a Reference API html file in download directory
    Args:
        url: URL to API Reference
    Returns:
        local_filename: Path to local version of the downloaded file.
    """
    local_filename = join('download', url.split('/')[-1])
    r = requests.get(url, stream=True)
    with open(local_filename, 'wb') as f:
        for chunk in r.iter_content(chunk_size=1024):
            if chunk:
                f.write(chunk)
    return local_filename



def get_all_reference_api_files(base_url):
    """
    Fetches URL of the Different type of API 
    from Base API
    """
    r = requests.get(base_url)
    soup = BeautifulSoup(r.text, "html.parser")
    reference = soup.find('h3', text=re.compile(r'Reference')).next_sibling.next_sibling
    urls = reference.findAll('a')
    api_urls = []
    for url in urls:
        api_urls.append(REACT_ROOT_URL+url.attrs['href'])
    return api_urls



if __name__ == "__main__":
    api_urls = get_all_reference_api_files(REACT_API_BASE_URL)
    for api_url in api_urls:
        local_file = download_file(api_url)
