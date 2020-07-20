#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

source ../modulefiles/modulefile.prod_util.$target             > /dev/null 2>&1

# Move to util/sorc folder
cd ../util/sorc

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

for prod_util in fsync_file
do
 cd $prod_util.cd
 make -f makefile clean
 make -f makefile
 make -f makefile install
 make -f makefile clean
 cd ..
done

for prod_util in mdate ndate nhour
do
 cd $prod_util.fd
 make -f makefile clean
 make -f makefile
 make -f makefile install
 make -f makefile clean
 cd ..
done
exit
