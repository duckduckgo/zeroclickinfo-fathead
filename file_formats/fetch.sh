#!/bin/bash
mkdir -p download
curl -o download/data 'https://en.wikipedia.org/wiki/Special:Export/List_of_file_formats_(alphabetical)'
