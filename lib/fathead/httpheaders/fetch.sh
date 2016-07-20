#!/bin/bash

mkdir -p download

wget -O download/request.json  'https://en.wikipedia.org/w/api.php?action=query&pageids=13106156&prop=revisions&rvprop=content&rvsection=5&rvparse&format=json&continue'
wget -O download/response.json 'https://en.wikipedia.org/w/api.php?action=query&pageids=13106156&prop=revisions&rvprop=content&rvsection=7&rvparse&format=json&continue'
