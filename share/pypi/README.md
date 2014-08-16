PyPI package name and description fetcher and parser.

# Dependencies

* Python2.7
* Python packages (install by calling `pip install gevent==1.0.1 requests==2.3.0`):
  * gevent 1.0.1
  * requests 2.3.0

# Notes

* Despite PyPI having a JSON API, this is not a spice due to PyPI's case sensitivity. For example,
  https://pypi.python.org/pypi/Django/json works while https://pypi.python.org/pypi/django/json doesn't, even though
  https://pypi.python.org/pypi/django (without '/json') correctly redirects to https://pypi.python.org/pypi/Django.
  This is a known issue: https://bitbucket.org/pypa/pypi/issue/23/package-redirect-behaviour-inconsistent
* The package JSONs contain more information that may be worth including in the search result. Notably, the description
  field might be useful to users.
