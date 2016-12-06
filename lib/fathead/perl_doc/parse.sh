rm -f {Parsers/,}output.txt && cd Parsers && perl parse.pl && mv output.txt ..

LC_ALL=C sort output.txt -o output.txt
