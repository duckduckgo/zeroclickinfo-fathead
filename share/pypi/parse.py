#!/usr/bin/env python
# -*- coding: utf-8 -*-
from xmlrpclib import ServerProxy

pypi = ServerProxy('http://pypi.python.org/pypi')
package_list = pypi.list_packages()
package_version = [(py, pypi.package_releases(py)[0]) for py in package_list]
package_release = [pypi.release_data(py, ver) for py, ver in package_version]
for pr in package_release:
    pr['images'] = ''
    pr['classifiers'] = ', '.join(pr['classifiers'])
    print "%(name)s\tA\t\t\t%(classifiers)s\t\t%(release_url)s\t\t%(package_url)s\t\t%(images)s\t%(description)s\t%(home_page)s\n" % pr
