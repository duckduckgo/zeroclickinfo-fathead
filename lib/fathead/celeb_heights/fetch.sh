mkdir -p download;
cd download; 
rm *;
for x in {A..Z}; do wget "http://www.celebheights.com/s/all$x.html"; done;
