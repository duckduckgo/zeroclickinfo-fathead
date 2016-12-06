#!/bin/bash

ruby parse.rb

LC_ALL=C sort output.txt -o output.txt
