#!/bin/bash

mkdir download
cd download
wget "$(cat ../data.url)"
cd ../
