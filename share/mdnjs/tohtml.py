"""
 Generate a quick HTML dump of a Fathead output.txt
"""
import csv

from parse import FatWriter

HTML = """
 <meta http-equiv='Content-Type' content='text/html; charset=utf-8'>
 <style>
   body {{
     font-family: 'Helvetica Neue', 'Segoe UI', sans-serif;
     font-size: 12px;
   }}
   .block {{
     margin: 40px auto;
     width: 600px;
   }}
   h1 {{
     color: rgb(67, 67, 67);
     font-size: 18px;
     font-style: normal;
     font-variant: normal;
     font-weight: bold;
     line-height: 36px;
   }}
 </style>
 <body>
 {body}
 </body>
"""
ROW = """
<div class="block">
<h1>{title} (JavaScript)</h1>
<div>
  {abstract}
</div>
<a href="{source_url}">{source_url}</a>
<p>{redirect}</p>
<pre>{disambiguation}</pre>
</div>
"""

def run(infname, outfname):
  infile = open(infname)
  reader = csv.DictReader(infile, FatWriter.FIELDS, dialect='excel-tab')
  with open(outfname, 'w') as outfile:
    rows = []
    for line in reader:
      rows.append(ROW.format(**line))
    body = '\n'.join(rows)
    outfile.write(HTML.format(body=body).replace('\\n', '\n'))

if __name__ == '__main__':
  infname = 'output.txt'
  outfname = 'output.html'
  run(infname, outfname)
