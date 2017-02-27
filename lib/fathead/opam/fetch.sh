#!/bin/bash

rm -rf download
mkdir download
wget https://opam.ocaml.org/packages/ -O download/packages.html