#!/bin/bash -eu

dir=$(dirname $0)

url=$(<"${dir}/data.url")
download_dir=${dir}/download
archive=$(mktemp)
docs_rel_dir=api.jquery.com-master/entries

curl \
  --silent \
  --create-dirs \
  --output "${archive}" \
  "${url}"

mkdir -p "${download_dir}"

# -qq: silent; -j: junk paths; -o: overwrite
unzip \
  -qqjo \
  "${archive}" \
  "${docs_rel_dir}/*.xml" \
  -d "${download_dir}"

rm -f "${archive}"
