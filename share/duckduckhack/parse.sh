echo Parsing
DIR="/home/ubuntu/zeroclickinfo-fathead/share/duckduckhack"
ls download/*.html | while read LINE; do
perl parseDocs.pl $LINE 2>/dev/null
done

printf "\nDone\n"
