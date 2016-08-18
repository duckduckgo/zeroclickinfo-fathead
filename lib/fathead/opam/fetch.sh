#!/bin/bash

rm -rf download
mkdir download
wget -O download/packages.html https://opam.ocaml.org/packages/
