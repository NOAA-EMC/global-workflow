#!/bin/sh
#
#  07052015	E.Mirvis -   made build more universal - environmental module based (see readme)
#               EMC/NCEP/NOAA
#
#  excutables created from build_tropcy.sh:
#        1) vint.fd/vint.x
#        2) tave.fd/tave.x
#        3) syndat_qctropcy.fd/syndat_qctropcy
#        4) syndat_maksynrc.fd/syndat_maksynrc
#        5) syndat_getjtbul.fd/syndat_getjtbul
#        6) supvit.fd/supvit
#
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  export MOD_PATH=${cwd}/lib/modulefiles
fi

source ../modulefiles/modulefile.storm_reloc_v6.0.0.$target
export FC=mpiifort
export JASPER_LIB=${JASPER_LIB:-$JASPER_LIBRARY_DIRS/libjasper.a}

export INC="${G2_INCd} -I${NEMSIO_INC}"
export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}"
export LIBS_SUP="${W3EMC_LIBd} ${W3NCO_LIBd}"
echo lset
echo lset
 export LIBS_REL="${W3NCO_LIB4}"
export LIBS_REL="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${LIBS_REL} ${SIGIO_LIB4} ${BACIO_LIB4} ${SP_LIBd}"
export LIBS_SIG="${SIGIO_INC4}"
export LIBS_SYN_GET="${W3NCO_LIB4}"
export LIBS_SYN_MAK="${W3NCO_LIB4} ${BACIO_LIB4}"
export LIBS_SYN_QCT="${W3NCO_LIB8}"
echo $LIBS_REL
echo NEXT

cd vint.fd
   make clean
   make -f makefile
   make install
   cd ../
cd tave.fd
   make clean
   make -f makefile
   make install
   cd ../
cd syndat_qctropcy.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd syndat_maksynrc.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd syndat_getjtbul.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd supvit.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../

exit
