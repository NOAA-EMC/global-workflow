#!/bin/sh
# 
#  07052015	E.Mirvis -   made build more universal - environmental module based (see readme)
#               EMC/NCEP/NOAA
#
#  excutables created from build_tropcy.sh: 
#        1) relocate_mv_nvortex.fd/relocate_mv_nvortex
#        2) vint.fd/vint.x
#        3) tave.fd/tave.x
#        4) syndat_qctropcy.fd/syndat_qctropcy
#        5) syndat_maksynrc.fd/syndat_maksynrc
#        6) syndat_getjtbul.fd/syndat_getjtbul
#        7) supvit.fd/supvit
#        8) gettrk.fd/gettrk
#
set -x -e

. /apps/lmod/lmod/init/sh
module purge
source ../modulefiles/Module_storm_reloc_v6.0.0_THEIA
module list

 machine=theia
 export LIBDIR=/scratch3/NCEPDEV/nwprod/lib/
# export NEMSIOGFS_LIB=/global/save/emc.glopara/svn/nceplibs/nemsiogfs/libnemsiogfs.a
# export NEMSIOGFS_INC=/global/save/emc.glopara/svn/nceplibs/nemsiogfs/incmod/nemsiogfs
 export NEMSIOGFS_LIB=/scratch4/NCEPDEV/global/save/glopara/svn/nceplibs/branches/alvsf_fix/intel/libnemsiogfs_v1.1.0.a
 export NEMSIOGFS_INC=/scratch4/NCEPDEV/global/save/glopara/svn/nceplibs/branches/alvsf_fix/intel/include/nemsiogfs_v1.1.0
 export NEMSIO_LIB=/scratch4/NCEPDEV/global/save/glopara/svn/nceplibs/nemsio/trunk/libnemsio.a
 export NEMSIO_INC=/scratch4/NCEPDEV/global/save/glopara/svn/nceplibs/nemsio/trunk/incmod/nemsio
 export W3EMC_LIBd=/scratch4/NCEPDEV/global/save/glopara/nwtest.nems/w3emc/sorc/w3emc/v2.2.0/libw3emc_v2.2.0_d.a 
 export INC="${G2_INCd} -I${NEMSIO_INC}"
 export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}"
 export LIBS_SUP="${W3EMC_LIBd} ${W3NCO_LIBd}"
 export LIBS_REL="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${W3NCO_LIBd} ${SIGIO_LIB4} ${BACIO_LIB4} ${SP_LIBd}"
 export LIBS_SIG="${SIGIO_INC4}"
 export LIBS_SYN_GET="${W3NCO_LIB4}"
 export LIBS_SYN_MAK="${W3NCO_LIB4} ${BACIO_LIB4}"
 export LIBS_SYN_QCT="${W3NCO_LIB8}"
 export FC=mpiifort
 export FFLAGS="-openmp -O3 -g -traceback -r8 -I${NEMSIOGFS_INC} -I${NEMSIO_INC} -I${SIGIO_INC4}"

#for dir in *.fd; do
# cd $dir
# make clean
# make -f makefile
# cd ..
#done

mkdir ../exec
cd relocate_mv_nvortex.fd
   make clean
   make -f makefile_theia
   make install
   cd ../
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
   cd ../
cd syndat_maksynrc.fd
   make clean
   make -f makefile
   make install
   cd ../
cd syndat_getjtbul.fd
   make clean
   make -f makefile
   make install
   cd ../
cd supvit.fd
   make clean
   make -f makefile
   make install
   cd ../
cd gettrk.fd
   make clean
   make -f makefile
   make install
   cd ../
