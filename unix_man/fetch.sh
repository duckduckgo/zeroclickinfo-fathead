mkdir -p download
cd download
#
# Other possible sources....
# linuxcommand.org,

# Chose linux.die.net as source, as it was the only site I found 
# that had static html man pages and I could crawl it for the most part (with restrictions). 

# --user-agent="" is an option, but die.net refuses
# I couldn't crawl the entire man page database, 
# as it is HUGE and I was blocked twice 
# for most likely sending way too many GET requests within a certain time period. 

# However their robots.txt they allow DuckDuckGo to crawl the site so the -e robots=off option could probably be taken off.
# consider --header="accept-encoding: gzip" for compression, I couldn't test it because 
# linux.die.net only allows certain agents to crawl their site
wget -e robots=off --reject html,css,png,ico,gif,style --wait 3 -nH -r http://linux.die.net/man/ 
