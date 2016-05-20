# cleanup download dir first
find download/functions/ -type f -not -name "*functions*.html" -exec rm {} \;
cd Parsers
rm output.txt
perl parse.pl >> output.txt 
perl parse-functions.pl >> output.txt
mv output.txt ../
