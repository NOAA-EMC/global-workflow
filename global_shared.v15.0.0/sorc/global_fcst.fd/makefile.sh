#!/bin/ksh
set -x

mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)

#export debug=YES                      # uncomment this line if you want debug option
#export CLEAN=NO                       # uncomment this if you don't want to clean before compiling
export USE_MKL=YES
export ICS_VERSION=14.0.1
 
#---------------------------------------------------------
if [ $mac = t -o $mac = g ] ; then # For WCOSS
#---------------------------------------------------------
 machine=wcoss
 ptmp="/ptmpp1/$LOGNAME"
 export NWPRODLIB=/nwprod/lib

 export SIGIO_VER=v2.0.1
 export SIGIO_DIR=$NWPRODLIB/sigio/$SIGIO_VER
 export SIGIO_LIB4=sigio_${SIGIO_VER}_4
 export SIGIO_INC4=${SIGIO_DIR}/incmod/$SIGIO_LIB4

 export W3NCO_VER=v2.0.6
 export W3NCO_DIR=$NWPRODLIB/w3nco/$W3NCO_VER
 export W3NCO_LIBd=w3nco_${W3NCO_VER}_d

 export W3EMC_VER=v2.0.5
 export W3EMC_DIR=$NWPRODLIB/w3emc/$W3EMC_VER
 export W3EMC_LIBd=w3emc_${W3EMC_VER}_d
 export W3EMC_INCd=$NWPRODLIB/w3emc/$W3EMC_VER/incmod/w3emc_${W3EMC_VER}_d

 export SP_VER=v2.0.2
 export SP_DIR=$NWPRODLIB/sp/$SP_VER
 export SP_LIBd=sp_${SP_VER}_d

 export BACIO_VER=v2.0.1
#export BACIO_DIR=$NWPRODLIB/bacio/$BACIO_VER
#export BACIO_LIB4=bacio_${BACIO_VER}_4
 export BACIO_DIR=/usrx/local/nceplibs
 export BACIO_LIB4=bacio_v2.0.1p_4

 export NEMSIO_VER=v2.2.1
 export NEMSIO_DIR=$NWPRODLIB/nemsio/$NEMSIO_VER
 export NEMSIO_LIB=nemsio_${NEMSIO_VER}

 export ESMF_LIB=/usrx/local/esmf-3.1.0rp5/lib/libO/Linux.intel.64.intelmpi.default
 export ESMF_MOD=/usrx/local/esmf-3.1.0rp5/mod/modO/Linux.intel.64.intelmpi.default

 if [ $USE_MKL = YES ] ; then
   export ALIGN="-align array32byte"              # For bit reproducibility on wcoss
   . /usrx/local/Modules/3.2.10/init/ksh          # To enable module unload and load
   module unload ics
   export ICS_VERSION=${ICS_VERSION:-14.0.1}
   module load ics/$ICS_VERSION
  export PRECISE=source
  #export PRECISE=precise
 fi

 export FINC=-I$ESMF_MOD
 export FINCM="-I$SIGIO_INC4 -I$W3EMC_INCd "
 export LIBSM="-L$BACIO_DIR -l$BACIO_LIB4 -L$SIGIO_DIR -l$SIGIO_LIB4 -L$NEMSIO_DIR -l$NEMSIO_LIB -L$SP_DIR -l$SP_LIBd \
               -L$W3EMC_DIR -l$W3EMC_LIBd -L$W3NCO_DIR -l$W3NCO_LIBd -lrt -lstdc++ -L$ESMF_LIB -lesmf"

#---------------------------------------------------------
elif [ $mac = z -o $mac = h -o $mac = f ] ; then # For ZEUS
#---------------------------------------------------------
 machine=zeus
 ptmp=/scratch2/portfolios/NCEPDEV/ptmp/$LOGNAME
 export LIBDIR=/contrib/nceplibs/nwprod/lib
 export NCEPLIB=/contrib/nceplibs/dev/lib
 export ESMF_LIB=/apps/esmf/3.1.0rp5/intel/mpt/lib/libO/Linux.intel.64.mpi.default        
 export ESMF_MOD=/apps/esmf/3.1.0rp5/intel/mpt/mod/modO/Linux.intel.64.mpi.default        

 export W3LIB=w3nco_d
 export FINC=-I$ESMF_MOD
 export FINCM="-I$NCEPLIB/incmod/sigio_v2.0.1_beta -I$LIBDIR/incmod/$W3LIB"
 export LIBSM="-L$NCEPLIB -lsigio_v2.0.1_beta -L$LIBDIR -lbacio_4 -lnemsio -lsp_d -l$W3LIB -lrt -lstdc++ -L$ESMF_LIB -lesmf"
#---------------------------------------------------------
fi
#---------------------------------------------------------

export ALIGN=${ALIGN:-""}
#
#  WARNING!!! The default endianness is local to the machine.
#   If your initial conditions are bigendian and want to compile on littleendian
#   machine, you must set NATIVE_ENDIAN=NO
#
NATIVE_ENDIAN=NO
#
sorc_dir=$(pwd)
exec_dir=$(pwd)
mkdir -p $exec_dir

#make_dir=$ptmp/branch/sorc/$(basename $sorc_dir)
#mkdir -p $make_dir ; cd $make_dir || exit 8
#cp -rp ${sorc_dir}/* .

if [ $NATIVE_ENDIAN = YES ] ; then
 cp $sorc_dir/sigio_r_module_native.f sigio_r_module.f
 cp $sorc_dir/bafrio_native.f         bafrio.f
fi
export PRECISE=${PRECISE:-precise}

#
#-------------------------------------------------------------------
#-------------------------------------------------------------------
if [ $machine = zeus -o $machine = wcoss ] ; then
 export CFLAGS="-DLINUX"
 export ARCHM=
 export PGSZM=
 export FRRM=-FR

 export debug=${debug:-NO}
 if [ $debug = YES ] ; then
   export OPTSB="-g -O0 -check all -ftrapuv -convert big_endian $ALIGN -fp-stack-check -fstack-protector -heap-arrays -recursive $ALIGN"  
   export OPTSBT="$OPTSB -traceback"
   export EXECM=$exec_dir/global_fcst_dbg
 else
   if [ $machine = zeus ]; then
    export OPTSB="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE "  
    export OPTSBX="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE "  
   else
    export OPTSB="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE -xAVX "  
    export OPTSBX="-g -O3 -convert big_endian $ALIGN -fp-model $PRECISE -xAVX "  
   fi
   export OPTSBT=$OPTSB
   export OPTSBTX=$OPTSBX
   export EXECM=$exec_dir/global_fcst
 fi

 export OPTSIOM="$OPTSBT -r8 -openmp"
 export OPTSIOX="$OPTSBTX -r8 -openmp"
 export OPTSM="$OPTSBT -r8 -openmp"
 export OPTS_SERM="$OPTSBT -r8 $ARCHM"
 export OPTS90M="$OPTSBT   -r8 "
 export OPTS90AM="$OPTSBT  -r8 "
 export LDFLAGSM=$PGSZM

 #----------------------------
 if [ $machine = wcoss ] ; then
   export F77M=mpiifort
   export F90M=mpiifort
   export F77B=$F77M
   export FCC=mpcc
   export LDRM=mpiifort
   if [ $USE_MKL = YES ] ; then
      module unload ics
      module load ics/$ICS_VERSION
      export LDFLAGSM="$PGSZM -openmp -lmkl_intel_lp64 -lmkl_core -lmkl_sequential -lpthread -lm"
   else
      export LDFLAGSM="$PGSZM -openmp "
   fi
 #----------------------------
 elif [ $machine = zeus ] ; then
   export F77M="ifort -openmp"  #-recursive"
   export F90M="ifort -openmp"  #-recursive"
   export F77B="ifort "         #-recursive" #-openmp"  #-recursive"
   export F90B="ifort "         #-recursive" #-openmp"  #-recursive"
   export LDRM="ifort -lmpi"
   export FCC=cc
   if [ $USE_MKL = YES ] ; then
      export LDFLAGSM="$PGSZM -openmp -mkl"
   else
      export LDFLAGSM="$PGSZM -openmp "
   fi
 fi
 #----------------------------
else
 echo 'machine not supported at this time'
 exit
fi
#-------------------------------------------------------------------
#-------------------------------------------------------------------

echo $F77M
make -f Makefile clean
if [ $USE_MKL = YES ] ; then
  make -f Makefile
else
  make -f Makefile model-mpi-port
fi
make -f Makefile install
make -f Makefile clean
