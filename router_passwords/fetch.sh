#!/bin/bash

if [ -d "download" ]; then
  rm -r download
fi
mkdir download &&

perl -MURI::Escape -ne '
  chomp;
  s|/|\\/|;
  print "curl -o download/\"$_\" "
      . "--data \"router="
      . uri_escape($_)
      . "\" www.routerpasswords.com\n"
' brands | bash
