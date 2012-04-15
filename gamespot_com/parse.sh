# Externalized the looping through files.
# This limits memory/resource usage/garbage collection issues in ruby and 
# makes the crawling more fault tolerant

function parse {

  input_dir=$1;
  output_file=$2;

  for i in `find $input_dir -name '*.html' | grep -v 'index.html'`; 
  do 
	echo $i; 
	ruby parse.rb $i $output_file;
  done

}

# Multiprocessing
echo "Staring:  `date`";
echo "Starting output1";
(parse 'download'  'output1.txt') &
echo "Starting output2";
(parse 'download2' 'output2.txt') &
echo "Starting output3";
(parse 'download3' 'output3.txt') &
echo "Starting output4";
(parse 'download4' 'output4.txt') &

wait
echo "Concatenating..."
export LC_ALL='C'
cat output1.txt output2.txt output3.txt output4.txt | sort > output.txt

echo "Done:  `date`";
