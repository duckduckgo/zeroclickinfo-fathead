#!/usr/bin/env bash

#this script parse all the downloaded data.
#basicly it iterates all the files in the download dir,
#sends them to the parse.js script (which know how to parse one file at a time)
#and then output the data to output.txt

download_dir='./downloads'
output='./output.txt'

#clean up last runs
rm -f ${output};

total_files=$(ls "$download_dir" | wc -l )
counter=1;

ls ${download_dir} | while read -r;
do
    temp_file="$download_dir/$REPLY";
    echo "processing $counter/$total_files: $temp_file";
    ./parse.js ${temp_file} >> ${output};
    
    #removing the last line if it is empty
    last_line=$(tail -n 1 ${output});
    [ -z "$last_line" ] && sed -i '$ d' ${output}

    counter=$((counter+1));
done;
