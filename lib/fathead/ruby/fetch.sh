#!/bin/bash -eu

url=$(<data.url)
archive=$(mktemp)
download_dir=download

curl \
  --silent \
  --create-dirs \
  --output "${archive}" \
  "${url}"

mkdir -p "${download_dir}"

tar \
  --extract \
  --overwrite \
  --directory "${download_dir}" \
  --file "${archive}"

rm -f "${archive}"
