#!/bin/bash

cd $(dirname -- "$0")

# wget madness here... this takes a long time
# may want to consider --limit-rate=20k to avoid being blocked
wget -m -c --remote-encoding=UTF-8 --wait 0.25 https://developer.apple.com/library/mac/sitemap.php
