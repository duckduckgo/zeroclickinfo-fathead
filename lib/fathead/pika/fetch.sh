#!/bin/bash

rm -rf download
mkdir -p download
cd download
wget https://media.readthedocs.org/htmlzip/pika/latest/pika.zip -O pika.zip
unzip -o -q pika.zip