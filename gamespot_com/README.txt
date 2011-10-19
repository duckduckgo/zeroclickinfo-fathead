# Upgrade to hpricot-0.8.4 due to hitting the following bug:
# hpricot/parse.rb:33: [BUG] Segmentation fault
#
# To recreate the fetch script:
ruby generate_fetch.rb > fetch.sh

# To fetch (this takes awhile ~ 60 minutes: 4685 html files)
bash fetch.sh

# to parse and produce output.txt (~ 2 minutes)
bash parse.sh
                  
