# !/bin/bash -eu

dir="$(command dirname -- "${0}")"


url=$(<"${dir}/data.url")
download_dir=${dir}/download
archive=$(mktemp)

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
