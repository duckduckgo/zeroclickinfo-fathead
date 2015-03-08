#!/bin/bash

[ ! -e download ] && mkdir download

wget -P download -O download/abbr.txt -N 'http://www.abbreviations.com/root/admin/dumpduckexport.php'