#!/bin/bash

cd $(dirname -- "$0")

# wget madness here... this takes a long time
# may want to consider --limit-rate=20k to avoid being blocked
# sadly you cannot --no-clobber with -m
wget -m --remote-encoding=UTF-8 https://developer.apple.com/library/mac/sitemap.php
