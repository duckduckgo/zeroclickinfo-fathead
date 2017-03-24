#!/bin/sh

mkdir -p "./download"

curl https://media.readthedocs.org/htmlzip/celery/v4.0.2/celery.zip -o "./download/celery.zip"

unzip -o "./download/celery.zip" -d "./download/"

mv  "./download/celery-v4.0.2/index.html" "./download/celery.html"

rm -rf "./download/celery.zip" "./download/celery-v4.0.2"
