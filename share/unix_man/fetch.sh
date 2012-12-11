#!/bin/bash 

wget -rqP download --reject php,images,jpg,js,css,png,ico,gif,style --wait 1 -nH -nd http://linuxcommand.org/smp_{a..z}.php

# just in case a second source is needed in the future, tldp.org
#wget --wait 1 -nH -nd -r http://www.tldp.org/manpages/man-html/man-html-20070604.tar.bz2 
# extract and prepare pages for parsing 
#tar -xvjf man-html-20070604.tar.bz2 
#mv man-html-20070604 tldp
#mv tldp/html/htmlman{1..5}/* tldp
#mv tldp/html/htmlman{7..9}/* tldp
#rm -rf tldp/html

