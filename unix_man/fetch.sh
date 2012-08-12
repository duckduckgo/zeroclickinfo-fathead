mkdir -p downloads
cd downloads
#or use empty header option
wget --reject html,css,png,ico,gif,style -r -e robots=off --wait 2 -nd http://linux.die.net/man/
