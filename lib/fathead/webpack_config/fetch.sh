#!/usr/bin/env bash
ouput_dir='downloads';

mkdir -p $ouput_dir;

#getting all the links from  a tags with class code-link
./fetch.js > tempLinks;

#removin the part after #, removing duplicate links and getting only /configuration links
awk -F '#' '{print $1}' tempLinks | sort | uniq | grep configuration > links;

#fetching all the links and saving them to output_dir
wget -i links -P "./$ouput_dir/" 

#cleanup
rm -f tempLinks links;

