#!/usr/bin/env bash

output_txt='./output.txt';
redirect_txt='./redirects.txt';

#get all the titles from the output.txt
awk '{print $1}' ${output_txt} > temp_names

rm -f ${redirect_txt}

#create redirect for . seperation
cat temp_names |  while read -r ;
do
	temp_redirect=$(echo ${REPLY} | sed -e 's/\./ /g');
	[ "$temp_redirect" != "$REPLY" ] && echo "$temp_redirect, $REPLY" >> $redirect_txt;
done;

#create redirect for camel case

#create redirect for hathead name in the end or begining

#create redirects for entities type (moules)




