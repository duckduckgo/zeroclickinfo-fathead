#!/bin/bash
perl parse.pl download/$(cat data.url | cut -d/ -f6) output.txt
