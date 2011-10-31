#!/bin/bash
# per zeroclickinfo-fathead/README.md, storing temporary file to "download" directory
mkdir -p download
curl "http://www.ldraw.org/cgi-bin/ptlist.cgi" --output "download/ldraw_org_unofficial_part_list.html"
