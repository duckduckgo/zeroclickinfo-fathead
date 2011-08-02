# fetch complete HGNC dataset (publicly linked from: http://www.genenames.org/cgi-bin/hgnc_stats.pl, URL is currently: http://www.genenames.org/cgi-bin/hgnc_downloads.cgi?title=HGNC+output+data&hgnc_dbtag=on&preset=all&status=Approved&status=Entry+Withdrawn&status_opt=2&level=pri&=on&where=&order_by=gd_app_sym_sort&limit=&format=text&submit=submit&.cgifields=&.cgifields=level&.cgifields=chr&.cgifields=status&.cgifields=hgnc_dbtag), currently ~20MB
# save into hgnc_complete_dataset.tsv.gz (expect ~3MB of gzipped output)
wget -O - -i data.url | gzip -c > hgnc_complete_dataset.tsv.gz

