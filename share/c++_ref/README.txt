Dependencies
------------
$PWD is assumed to be the directory c++_ref
pushd $PWD
cd /usr/ports/www/node && make PACKAGE_BUILDING=yes install clean
popd
npm install .
