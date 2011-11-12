# Externalized the looping through files.
# This limits memory/resource usage/garbage collection issues in ruby and 
# makes the crawling more fault tolerant
for i in `find . -name '*.html' | grep -v 'index.html'`; 
do 
	echo $i; 
	ruby parse.rb $i output.txt
done
