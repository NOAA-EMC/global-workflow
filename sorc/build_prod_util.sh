#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/modulefile.prod_util.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/modulefile.prod_util.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/modulefile.prod_util.$target           > /dev/null 2>&1
  fi
fi

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
