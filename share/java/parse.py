#!/usr/bin/python
import os
import parse_utils


# delete previous output
if os.path.exists('output.txt'):
  os.remove('output.txt')

# iterate package "java"
for f in parse_utils.collectDocFilesFrom('./docs/api/java'):
  parse_utils.output("output.txt", parse_utils.getDocs(f))

# iterate package "javax"
for f in parse_utils.collectDocFilesFrom('./docs/api/javax'):
  parse_utils.output("output.txt", parse_utils.getDocs(f))
