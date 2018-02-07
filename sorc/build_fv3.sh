#! /bin/sh

target=$1
if [ $# -ne 1 ]; then
 echo "Usage: $0 wcoss or cray or theia"
 exit
fi

set -xue

cd fv3gfs.fd/
FV3=$( pwd -P )/FV3
cd tests/
./compile.sh "$FV3" "$1" "NCEP64LEV=Y HYDRO=N 32BIT=Y" 1
mv -f fv3_1.exe ../NEMS/exe/fv3_gfs_nh.prod.32bit.x
