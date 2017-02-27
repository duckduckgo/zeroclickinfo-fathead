mkdir -p download

START=$(date +%s)
wget -r -nc -k -P ./download -l 4 -I /w/c  http://en.cppreference.com/w/
END=$(date +%s)
DIFF=$(( $END - $START ))
echo "It took $DIFF seconds"


#for file in $(find download/ -type f)
#do echo "Moving file" $file
#mv $file download/
#done

echo "Deleting Empty directories"
find download/ -empty -type d -delete
