#!/bin/bash -eu

dir=$(dirname $0)

download_dir=${dir}/download
script=${dir}/parse.js
output=${dir}/output.txt

node "${script}" "${download_dir}/"*.xml > "${output}"
