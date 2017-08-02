#!/usr/bin/env python3
#
# -*- coding: utf-8 -*-
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A Fathead fetcher for wikiHow articles.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This program fetches all the contents from the wikihow website based
# on their article titles. Please make sure the headers of the bot are
# updated properly before running.
#
# Please read wikihow's service agreement before running this program
# http://www.wikihow.com/wikiHow:Terms-of-Use

__MODULE__ = "wikihow"
__AUTHOR__ = "DuckDuckGo [https://duckduckgo.com]"
__SOURCE__ = "http://www.wikihow.com/"
__CONTACT__ = "pjhampton[at]protonmail.com"

# A py3 single threaded script that fetches the wikihow articles

import time
from urllib.request import urlopen, Request

TESTING = False
fetch_errors = list()

## opens the url list
with open('data/urls.txt', 'r', encoding="latin1") as f:
    urls = f.read()
    urls = urls.split("\n")

## iterates throuh and gets the html .5 seconds per URI
seed_iterator = 171902
for i in range(len(urls)):
    tmp_error = {}
    filename = str(seed_iterator) + ".txt"

    print("Calling:", urls[i])
    req = Request(urls[i])
    req.add_header('Referer', __AUTHOR__)
    req.add_header('User-Agent', 'fathead-ddg/0.1 (Contact: ' + __CONTACT__ + ')')

    try:
        response = urlopen(req)
        html_content = response.read()
        with open("data/" + filename, 'w') as f:
            print(filename)
            f.write(html_content.decode("utf-8"))
    except (KeyboardInterrupt, SystemExit):
        break
    except:
        tmp_error["url"] = urls[i]
        tmp_error["reason"] = "bad response"

    if bool(tmp_error):
        fetch_errors.append(tmp_error)

    seed_iterator += 1
    time.sleep(.1) # slow me down

# Finish up and create errors/warnings report
print("Fin.")

print("Creating error report in fetch_errors.txt")
with open("fetch_errors.csv", "w") as f:
    for error in fetch_errors:
        f.write(error["reason"] + ", " + error["url"] + "\n")

