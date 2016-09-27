#!/bin/bash

rm -rf download
mkdir -p download
cd download
wget http://docs.python-requests.org/en/master/user/quickstart/
wget -O advanced.html  http://docs.python-requests.org/en/master/user/advanced/
