#!/bin/bash 

mkdir -p download

# Download the home page
wget http://man7.org/linux/man-pages/dir_all_alphabetic.html -O download/homepage.html

# fetch the pages
python3 fetch.py

# Remove the homepage so we don't parse it
rm download/homepage.html

# Remove systemd.index page, the pages are already listed
# on the main homepage
rm download/30-systemd-environment-d-generator.7.html

# it's an empty page
rm download/gnutls_x509_trust_list_add_system_trust.3.html

# Remove pcredemo because it's just a demo nothing to parse
rm download/pcredemo.3.html

