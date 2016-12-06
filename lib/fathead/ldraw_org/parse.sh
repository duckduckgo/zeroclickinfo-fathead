#!/bin/bash

ruby parse.rb download/ldraw_org_unofficial_part_list.html > output.txt
LC_ALL=C sort output.txt -o output.txt
