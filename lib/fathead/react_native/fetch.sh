#!/bin/bash

mkdir -p download
cd download
wget -np -nc -r -l 2  http://facebook.github.io/react-native/releases/0.40/
mv facebook.github.io/react-native/releases/0.40/docs docs

