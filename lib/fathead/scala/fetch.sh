#!/bin/bash

rm -rf download
mkdir -p download
cd download
wget http://www.scala-lang.org/files/archive/scala-docs-2.10.2.txz
xz -d scala-docs-2.10.2.txz
tar xvf scala-docs-2.10.2.tar
mv scala-docs-2.10.2 api
