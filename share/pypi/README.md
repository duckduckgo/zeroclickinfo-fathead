PyPI package name and description fetcher and parser.

# Dependencies

* Python2.7
* wget

# Notes

* Getting all the package JSONs takes a while. It may be possible to speed the process up by using something more
  intelligent than parallel wget processes.
* Despite PyPI having a JSON API, this is not a spice due to PyPI's case sensitivity. For example,
  https://pypi.python.org/pypi/Django/json works while https://pypi.python.org/pypi/django/json doesn't, even though
  https://pypi.python.org/pypi/django (without '/json') correctly redirects to https://pypi.python.org/pypi/Django.
  This is a known issue: https://bitbucket.org/pypa/pypi/issue/23/package-redirect-behaviour-inconsistent
* The package JSONs contain more information that may be worth including in the search result. Notably, the description
  field might be useful to users.
