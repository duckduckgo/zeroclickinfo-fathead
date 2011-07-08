mkdir -p data
cd data
wget -np -nc -r -l 1 -I /en/CSS https://developer.mozilla.org/en/CSS_Reference
cd developer.mozilla.org/en/CSS/
mv "::first-letter" ":first-letter"
mv "::first-line" ":first-line"
