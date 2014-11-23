# see http://stackoverflow.com/a/10959815/834
rm -rf download
mkdir -p download
wget --quiet --no-check-certificate --no-cookies --header "Cookie: oraclelicense=accept-securebackup-cookie" -O download/jdk-8u25-docs-all.zip http://download.oracle.com/otn-pub/java/jdk/8u25-b17/jdk-8u25-docs-all.zip
unzip -q download/jdk-8u25-docs-all.zip