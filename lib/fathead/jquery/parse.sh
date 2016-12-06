#!/bin/bash -eu

dir=$(dirname $0)

download_dir=${dir}/download
script=${dir}/parse.js
output=${dir}/output.txt

node "${script}" "${download_dir}/"*.xml | sort > "${output}"

LC_ALL=C sort output.txt -o output.txt
