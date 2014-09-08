# !/bin/bash
# run as root
chmod u+x parseDocs.pl
chmod u+x getDocs.pl
cpanm install Mojo

DIR="/home/ubuntu/zeroclickinfo-fathead/share/duckduckhack"

mkdir download

cd download
echo Getting duckduckhack front page
# get the front ddh page
wget --quiet http://duck.co/duckduckhack/ddh-intro

echo Getting list of links to documents
# parse the side bar links to each individual doc
perl $DIR/getDocs.pl $DIR/download/ddh-intro

echo Getting the individual doc pages
# get the individual pages
cat $DIR/download/links.txt | while read LINE; do
	wget --quiet --output-document=$LINE.'html' http://duck.co/duckduckhack/$LINE
done

# some duplicates
rm $DIR/download/faq#*

# page needs some work
rm $DIR/download/fathead_basic_tutorial*

cd ..
rm output.txt
