echo Parsing
DIR="/home/ubuntu/zeroclickinfo-fathead/share/duckduckhack"
ls download/*.html | while read LINE; do
perl parseDocs.pl $LINE 2>/dev/null
done

#rm -rf $DIR/download
cp output.txt /tmp2/ddg/sources/100.abstract.process.txt

printf "\nDone\n"
