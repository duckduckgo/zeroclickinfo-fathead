Dependencies:

Python 3.2.1+
http://www.python.org/download/

lxml 2.3.1+ 
http://lxml.de/index.html#download


I use the following Bash script to compile and install both on Ubuntu 10.10:

THREAD_COUNT=$([ $(which getconf) ] && getconf _NPROCESSORS_ONLN || grep -cE '^processor' /proc/cpuinfo)
TMP_DIR=$(mktemp -d /tmp/$(basename -- $0).XXXXXXXXXX)

# checkinstall
sudo apt-get -y install checkinstall

# python 3.2
sudo apt-get -y install libncursesw5-dev libreadline5-dev libssl-dev libgdbm-dev libc6-dev libsqlite3-dev tk-dev libbz2-dev
wget -P $TMP_DIR http://www.python.org/ftp/python/3.2.1/Python-3.2.1.tar.xz
tar -xJf $TMP_DIR/Python-3.2.1.tar.xz -C $TMP_DIR
pushd $TMP_DIR/Python-3.2.1 > /dev/null
./configure
make -j $THREAD_COUNT
sudo checkinstall --pkgname=python3.2 --pkgversion=3.2.1 --backup=no --deldoc=yes --fstrans=no --default make altinstall
popd > /dev/null
ln -sv /usr/local/bin/python3.2 /usr/local/bin/python3
ln -sv /usr/local/bin/pydoc3.2 /usr/local/bin/pydoc3

# lxml
sudo apt-get -y install libxml2-dev libxslt1-dev
wget -P $TMP_DIR http://lxml.de/files/lxml-2.3.1.tgz
tar -xzf $TMP_DIR/lxml-2.3.1.tgz -C $TMP_DIR
pushd $TMP_DIR/lxml-2.3.1 > /dev/null
sudo python3.2 setup.py install
popd > /dev/null

# cleanup
sudo rm -Rf $TMP_DIR
