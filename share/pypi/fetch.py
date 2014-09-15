#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

import codecs
import json
import logging
import xmlrpclib
import sys

from gevent import monkey
from gevent.pool import Pool
import requests

monkey.patch_all()

logging.basicConfig(level=logging.INFO, format='%(asctime)s [%(levelname)s] %(name)s: %(message)s')
logger = logging.getLogger()

# Global counters
download_count = 0
not_found_count = 0

# Shared requests session (speeds things up significantly compared to calling requests.get())
session = requests.Session()


def download_package_dict(package_name):
    """Download the given package, returning the response's JSON as a dict or None if an error occurred."""

    global download_count, not_found_count

    download_count += 1
    if download_count % 1000 == 0:
        logger.info('Downloaded %s packages (%s not found)', download_count, not_found_count)

    response = session.get('https://pypi.python.org/pypi/%s/json' % package_name)
    if response.status_code == requests.codes.ok:
        return response.json()
    elif response.status_code == requests.codes.not_found:
        not_found_count += 1
    else:
        logger.warning('Unexpected error code for package %s: %s', package_name, response.status_code)


def download_all_package_dicts(num_greenlets=5, limit=None):
    """
    Download all the package dicts from PyPI.

    This function returns a generator that maps the download_package_dict() function to all the packages listed by
    PyPI's XML-RPC interface. Due to an inconsistency between the XML-RPC and JSON APIs, some 404 errors are expected,
    so None values are yielded by this generator.

    Change num_greenlets to control the level of parallelisation. The default was set to 5 based on local experiments.
    It took about 10 minutes to download all the package JSONs with this setting. Choosing a higher value seems to
    flood PyPI with requests and cause rate limiting.

    Set limit to an integer value to limit the number of packages downloaded. This is useful when experimenting with
    this script.
    """

    package_names = xmlrpclib.ServerProxy('https://pypi.python.org/pypi').list_packages()[:limit]
    logger.info('Downloading %s packages', len(package_names))
    return Pool(num_greenlets).imap(download_package_dict, package_names)


if __name__ == '__main__':
    with codecs.open('download/package-jsons', 'wb', encoding='utf-8') as out_file:
        for package_dict in download_all_package_dicts(limit=int(sys.argv[1]) if len(sys.argv) > 1 else None):
            if package_dict:
                out_file.write(json.dumps(package_dict))
                out_file.write('\n')
