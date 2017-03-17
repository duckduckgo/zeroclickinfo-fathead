# Got the Archive link from 
# http://en.cppreference.com/w/Cppreference:Archives
# Right now, link is entered manually, 
# It will take additional effort to automate this.
# Will pickup later

mkdir -p download 
# wget http://upload.cppreference.com/mwiki/images/e/e1/html_book_20170214.zip
unzip html_book_20170214.zip -d html_book
mv html_book/reference/en/c/ download/
rm -rf html_book

cd "download/c/"

for file in *.html
do 
if [ -d "${file%%.html}" ]; then
	rm $file
fi
done

