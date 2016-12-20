#!/bin/ksh
set -x
#-----------------------------------------------------
#-use standard module. called by ../../build_wcoss.sh 
#-----------------------------------------------------

#-------------------------------------------------
export ESMF_LIB=/apps/esmf/3.1.0rp5/intel/intelmpi/lib/libO/Linux.intel.64.intelmpi.default
export ESMF_MOD=/apps/esmf/3.1.0rp5/intel/intelmpi/mod/modO/Linux.intel.64.intelmpi.default


export FINC=-I$ESMF_MOD
export FINCM="-I$SIGIO_INC4 -I$W3EMC_INCd "
export LIBSM="$BACIO_LIB4 \
              $SIGIO_LIB4 \
              $NEMSIO_LIB \
              $SP_LIBd    \
              $W3EMC_LIBd \
              $W3NCO_LIBd \
              -lrt -lstdc++  \
              -L$ESMF_LIB -lesmf"

#-------------------------------------------------------------------
export ALIGN=
export PRECISE=precise
export LDFLAGSM=" -openmp "
export USE_MKL=YES
if [ $USE_MKL = YES ] ; then
  export LDFLAGSM="-openmp -lmkl_intel_lp64 -lmkl_core -lmkl_sequential -lpthread -lm"
  export ALIGN="-align array32byte"              # For bit reproducibility on wcoss
  export PRECISE=source
fi

export debug=${debug:-NO}
if [ $debug = YES ] ; then
   export OPTSB="-g -O0 -check all -ftrapuv -convert big_endian $ALIGN -fp-stack-check -fstack-protector -heap-arrays -recursive $ALIGN"  
   export OPTSBT="$OPTSB -traceback"
   export EXECM=global_fcst_dbg
else
   export OPTSB="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE -xAVX "  
   export OPTSBX="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE -xAVX "  
   export OPTSBT=$OPTSB
   export OPTSBTX=$OPTSBX
   export EXECM=global_fcst
fi

export OPTSIOM="$OPTSBT -r8 -openmp"
export OPTSIOX="$OPTSBTX -r8 -openmp"
export OPTSM="$OPTSBT -r8 -openmp"
export OPTS_SERM="$OPTSBT -r8 "
export OPTS90M="$OPTSBT   -r8 "
export OPTS90AM="$OPTSBT  -r8 "

#----------------------------
export F77M="mpiifort -openmp"
export F90M="mpiifort -openmp"
export F77B=mpiifort
export F90B=mpiifort
export FCC=mpicc
export LDRM=mpiifort
export FRRM=-FR

echo $F77M
make -f Makefile clean
if [ $USE_MKL = YES ] ; then
  make -f Makefile
else
  make -f Makefile model-mpi-port
fi
make -f Makefile install
make -f Makefile clean
