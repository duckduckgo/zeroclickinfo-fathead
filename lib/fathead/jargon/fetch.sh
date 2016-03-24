#!/bin/sh

if [ ! -e download ]; then
    mkdir download
fi

#The Jargon File hasn't been updated since 2003, so don't bother fetching a new copy if we already have it
if [ ! -e download/jargsrc.tar.gz ]; then
    wget "http://catb.org/~esr/jargon/jargsrc.tar.gz" -O download/jargsrc.tar.gz
fi

tar xzf download/jargsrc.tar.gz --exclude=docbook-xsl-\* --exclude=graphics --exclude=\*html --exclude=\*xsl --exclude=READ.ME --exclude=Makefile --exclude=chaff.xml --exclude=jargon-mode.el --exclude=jargon.css --exclude=BUGS -C download/
