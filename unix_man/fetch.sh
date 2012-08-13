mkdir -p download
cd download
# --user-agent="" was an option, but die.net refuses
# I couldn't crawl the entire man page database, as it is HUGE and I was blocked twice for most likely sending way too many GET requests within a certain time period. However, in their robots.txt, they allow DuckDuckGo to crawl the site so the -e robots=off option could probably be taken off.

wget -e robots=off --reject html,css,png,ico,gif,style --wait 2 -nd -r http://linux.die.net/man/ 
