Dependencies
------------
$PWD is assumed to be the directory c++_ref
pushd $PWD
cd /usr/ports/www/node && make PACKAGE_BUILDING=yes install clean
cd /usr/local && curl http://npmjs.org/install.sh | sh
cd /usr/local/lib/node_modules
popd
npm install .
