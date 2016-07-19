cd Parsers && perl parse.pl > output.txt \
  && perl parse-functions.pl >> output.txt \
  && mv output.txt ..
