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

if [ $target = wcoss ]; then

    targetx=wcoss
    module load ../modulefiles/modulefile.storm_reloc_v6.0.0.$target
    module list

    export LIBDIR=/nwprod/lib
    export NEMSIOGFS_LIB=/global/save/Fanglin.Yang/svn/gfs/tags/nemsiogfs/intel/libnemsiogfs_v1.1.0.a
    export NEMSIOGFS_INC=/global/save/Fanglin.Yang/svn/gfs/tags/nemsiogfs/intel/include/nemsiogfs_v1.1.0
    export NEMSIO_LIB=/global/save/emc.glopara/svn/nceplibs/nemsio/trunk/libnemsio.a
    export NEMSIO_INC=/global/save/emc.glopara/svn/nceplibs/nemsio/trunk/incmod/nemsio
    export W3EMC_LIBd=/global/save/Hang.Lei/test/w3emc/sorc/w3emc/v2.2.0/libw3emc_v2.2.0_d.a

    export LIBS_REL="${W3NCO_LIBd}"

    export FC=mpiifort
    export FFLAGS="-openmp -O3 -g -traceback -r8 -I${NEMSIOGFS_INC} -I${NEMSIO_INC} -I${SIGIO_INC4}"

elif [ $target = theia ]; then

    targetx=theia
    source ../modulefiles/modulefile.storm_reloc_v6.0.0.$target > /dev/null 2>&1
    module list

    export LIBS_REL="${W3NCO_LIBd}"

    export FC=mpiifort
    export FFLAGS="-openmp -O3 -g -traceback -r8 -I${NEMSIOGFS_INC} -I${NEMSIO_INC} -I${SIGIO_INC4}"

elif [ $target = wcoss_cray ]; then

    targetx=cray
    if [ $USE_PREINST_LIBS = true ]; then
      source ../modulefiles/modulefile.storm_reloc_v5.1.0.$target           > /dev/null 2>&1
    else
      source ../modulefiles/modulefile.storm_reloc_v5.1.0.${target}_userlib > /dev/null 2>&1
    fi
    module load  intel/15.0.3.187 cray-libsci/13.0.3
    module list

    export LIBS_REL="${W3NCO_LIB4}"

    #export FFLAGS="-openmp -O3 -g -traceback -r8 -I${NEMSIOGFS_INC} -I${NEMSIO_INC} -I${SIGIO_INC4}"
    export FFLAGS="-openmp -O1 -g -traceback -r8 -I${NEMSIOGFS_INC} -I${NEMSIO_INC} -I${SIGIO_INC4}"

elif [ $target = wcoss_dell_p3 ]; then

    targetx=wcoss_dell_p3
    if [ $USE_PREINST_LIBS = true ]; then
      source ../modulefiles/modulefile.storm_reloc_v5.1.0.$target           > /dev/null 2>&1
    else
      source ../modulefiles/modulefile.storm_reloc_v5.1.0.${target}_userlib > /dev/null 2>&1
    fi
    module load  ips/18.0.1.163 impi/18.0.1         

    export LIBS_REL="${W3NCO_LIB4}"

    #export FFLAGS="-qopenmp -O3 -g -traceback -r8 -I${NEMSIOGFS_INC} -I${NEMSIO_INC} -I${SIGIO_INC4}"
    export FFLAGS="-qopenmp -O1 -g -traceback -r8 -I${NEMSIOGFS_INC} -I${NEMSIO_INC} -I${SIGIO_INC4}"

else

    echo "Unknown machine = $target"
    exit 1
fi

export INC="${G2_INCd} -I${NEMSIO_INC}"
export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}"
export LIBS_SUP="${W3EMC_LIBd} ${W3NCO_LIBd}"
export LIBS_REL="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${LIBS_REL} ${SIGIO_LIB4} ${BACIO_LIB4} ${SP_LIBd}"
export LIBS_SIG="${SIGIO_INC4}"
export LIBS_SYN_GET="${W3NCO_LIB4}"
export LIBS_SYN_MAK="${W3NCO_LIB4} ${BACIO_LIB4}"
export LIBS_SYN_QCT="${W3NCO_LIB8}"

#cd relocate_mv_nvortex.fd
#   make clean
#   make -f makefile_$targetx
#   make install
#   make clean
#   cd ../
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
cd gettrk.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../

exit
