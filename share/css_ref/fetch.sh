mkdir -p download
cd download
wget -nc -r -l 1 -I "/en/CSS,/En/CSS" https://developer.mozilla.org/en/CSS_Reference
cd developer.mozilla.org/
cp -R En/* en/
