The US Code is (inconsistently) divided into titles, chapters, subchapters, parts, subparts, sections, subsections, paragraphs, and perhaps even further. However, the "primary key" used in the legal profession is (title, section), so the first two fields of the output file are title and section. All further fields are merely human-readable content.

The output file contains some duplicate information. For example, every section in a given chapter contains the heading of the chapter. I figured it was worth the loss of 2 or 3 megabytes for the convenience of needing only to look up one record for every query.

Some of the section numbers are ranges (eg Title 15, Section "79 to 79z–6"). All of the sections in the range have the same content. This can be handled by generating the interior of the range, or simply using string > or < during lookup.

The output contains generated links to www.law.cornell.edu. I chose that site because it is more user-friendly than the gpo.gov one, and it has a much more convenient URL interface: The text of any section can be reached by navigating to www.law.cornell.edu/uscode/text/$title/$section. The government site uses a more cumbersome scheme.

Dependencies:
	python3
	bs4	(beautiful soup 4)
