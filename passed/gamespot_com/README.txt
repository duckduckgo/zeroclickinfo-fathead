# Upgrade to hpricot-0.8.4 due to hitting the following bug:
# hpricot/parse.rb:33: [BUG] Segmentation fault

# To fetch (this takes awhile ~ 25 minutes (real	22m27.199s): 3190 html files)
bash fetch.sh

# to parse and produce output.txt (~ 15 minutes (real 15m17.134s):)
bash parse.sh

This just gets links and assembles external links based upon game system type.  
The source link is somewhat arbitrary at this point