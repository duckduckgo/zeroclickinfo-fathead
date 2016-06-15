#!/bin/bash

rm -rf download
mkdir download
wget --quiet -O download/packages.html https://opam.ocaml.org/packages/
