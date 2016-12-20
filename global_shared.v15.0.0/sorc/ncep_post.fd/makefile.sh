#!/bin/sh
set -x
mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)
################################# options ###############################################
 export CLEAN=NO                                 # comment this line to clean before compiling
#debug=YES                                       # turn on debug mode     - default - NO
#make_post_lib=YES                               # create post library    - default - NO
 make_post_exec=YES                              # create post executable - default - YES
#make_nowrf=NO                                   # compile with wrf stub instead of WRF lib
 make_nowrf=YES                                  # compile with wrf stub instead of WRF lib
#BMPYXML=_bmpyxml                                # use original bumpy xml file
                                                 # make sure to clean when changing BMPXML 
################################# options ###############################################
#
if [ $mac2 = ga ] ; then                         # For GAEA
 machine=gaea
 center=${center:-ncep}
 make_nowrf=${make_nowrf:-YES}                   # to compile with wrf stub instead of WRF lib
elif [ $mac2 = tf ] ; then                       # For Theia
 machine=theia
 make_nowrf=${make_nowrf:-YES}                   # to compile with wrf stub instead of WRF lib
elif [ $mac = z -o $mac = h -o $mac = f ] ; then # For ZEUS
 machine=zeus
 make_nowrf=${make_nowrf:-YES}                   # to compile with wrf stub instead of WRF lib
elif [ $mac = t -o $mac = e -o $mac = g ] ; then # For WCOSS
 machine=wcoss
elif [ $mac = l -o $mac = s ] ; then             #    wcoss_c (i.e. luna and surge)
 export machine=wcoss_c
 make_nowrf=${make_nowrf:-YES}                   # to compile with wrf stub instead of WRF lib
fi
debug=${debug:-NO}
export make_post_lib=${make_post_lib:-NO}
export make_post_exec=${make_post_exec:-YES}
export make_nowrf=${make_nowrf:- NO}
export BMPYXML=${BMPYXML:-""}
if [ $machine = wcoss ] ; then
  export NETCDFPATH="/usrx/local/NetCDF/3.6.3"
  export WRFPATH="/nwprod/sorc/wrf_shared.v1.1.0"
  export NWPROD="/nwprod"
  export XMLPATH=$NWPROD
  export IPPATH=$NWPROD
  export SPPATH=/usrx/local/nceplibs
  export BACIOPATH=/usrx/local/nceplibs
  export ipv=""
  export spv=_v2.0.2p
  export crtmv=2.0.6
  export crtmv_inc=$crtmv
  export xmlv=_v2.0.0
  export baciov=_v2.0.1p
  export FC=mpiifort
  export CPP="/lib/cpp -P"
  export CPPFLAGS="-DLINUX"
  export CC=cc
  if [ $debug = YES ] ; then
    export OPTS="-O0 -openmp "
#   export DEBUG="-g -check all -ftrapuv -convert big_endian -fp-stack-check -fstack-protector -heap-arrays -recursive -traceback"
    export DEBUG="-g -traceback -convert big_endian -ftrapuv -check bounds -check format -check output_conversion -check pointers -check uninit -fp-stack-check"
  else
    export OPTS="-O3 -convert big_endian -fp-model source -openmp -xAVX"
    export DEBUG=""
  fi
  export LIST=""
  export FREE="-FR"
  export TRAPS=""
  export PROFILE=""
  module unload ics
  module load ics/14.0.1
elif [ $machine = wcoss_c ] ; then
  module unload PrgEnv-intel
  module load PrgEnv-intel
  module unload iobuf
  module load iobuf
  module unload NetCDF-intel-sandybridge/4.2
  module load NetCDF-intel-sandybridge/3.6.3
  module list
  export WRFPATH="/gpfs/hps/nco/ops/nwprod/wrf_shared.v1.1.0-intel"
  export FC=ftn
  export CPP="/lib/cpp -P"
  export CPPFLAGS="-DLINUX"
  export CC=cc
  if [ $debug = YES ] ; then
    export OPTS="-O0 -openmp "
    export DEBUG="-g -traceback -convert big_endian -ftrapuv -check bounds -check format -check output_conversion -check pointers -check uninit -fp-stack-check"
  else
    export OPTS="-O3 -convert big_endian -fp-model source -openmp"
#   export OPTS="-O3 -convert big_endian -fp-model source -openmp -xAVX"
    export DEBUG=""
  fi
  export LIST=""
  export FREE="-FR"
  export TRAPS=""
  export PROFILE=""
elif [ $machine = zeus ] ; then
  export NETCDFPATH="/apps/netcdf/3.6.3/intel"
  export WRFPATH="/scratch2/portfolios/NCEPDEV/meso/save/Dusan.Jovic/WRFV3"
  export NWPROD="/contrib/nceplibs/nwprod"
  export XMLPATH="/home/Hui-Ya.Chuang"
  export IPPATH=$NWPROD
  export SPPATH=$NWPROD
  export ipv=""
  export spv=_v2.0.1
  export crtmv=2.0.7
  export FC="ifort -lmpi"
  export CPP="/lib/cpp -P"
  export CC=cc
  export ARCH=""
  export CPPFLAGS="-DLINUX"
  if [ $debug = YES ] ; then
    export OPTS="-O0 -openmp -g"
    export DEBUG="-g -check all -ftrapuv -convert big_endian -fp-stack-check -fstack-protector -heap-arrays -recursive -traceback"
  else
    export export OPTS="-O3 -convert big_endian -traceback -g -fp-model source -openmp"
    export DEBUG=""
  fi
  export LIST=""
  export FREE="-FR"
  export TRAPS=""
  export PROFILE=""
elif [ $machine = theia ] ; then
  export NETCDFPATH="/apps/netcdf/4.3.0-intel"
# export WRFPATH="/scratch4/NCEPDEV/meso/save/Dusan.Jovic/WRFV3"
  export WRFPATH="/scratch4/NCEPDEV/global/save/Shrinivas.Moorthi/theia/nceplibs/nwprod/lib/sorc/WRFV3"

  export NWPROD="/scratch4/NCEPDEV/global/save/Shrinivas.Moorthi/theia/nceplibs/nwprod"
  export ipv=_v2.0.3
  export spv=""
##export spv=_v2.0.1
  export crtmv=2.0.7
  export gfsiov=""
  export w3ev=_v2.1.0
  export w3nv=""
  export xmlv=_v2.0.0
  export g2tv=""
  export baciov=_v2.1.0

# export NWPROD="/scratch3/NCEPDEV/nwprod"
# export ipv=_v2.0.0
# export spv=_v2.0.2
# export crtmv=2.0.6
# export crtmv=2.1.3
# export gfsiov=_v1.1.0
# export w3ev=_v2.0.5
# export w3nv=_v2.0.6
# export xmlv=_v2.0.0
# export g2tv=_v1.3.0
# export baciov=_v2.0.1

  export XMLPATH=$NWPROD
  export IPPATH=$NWPROD
  export SPPATH=$NWPROD
  export BACIOPATH=$NWPROD/lib

  export FC=mpiifort
  export CPP="/lib/cpp -P"
  export CC=cc
  export ARCH=""
  export CPPFLAGS="-DLINUX"
  if [ $debug = YES ] ; then
    export OPTS="-O0 -openmp -g"
    export DEBUG="-g -check all -ftrapuv -convert big_endian -fp-stack-check -fstack-protector -heap-arrays -recursive -traceback"
  else
    export export OPTS="-O3 -convert big_endian -traceback -g -fp-model source -openmp"
#   export export OPTS="-O2 -convert big_endian -traceback -g -fp-model source "
#   export export OPTS="-O2 -convert big_endian -traceback -g -fp-model source -openmp"
    export DEBUG=""
  fi
  export LIST=""
  export FREE="-FR"
  export TRAPS=""
  export PROFILE=""
elif [ $machine = gaea ] ; then
# export NETCDFPATH="/opt/cray/netcdf/4.1.1.0/netcdf-intel"
  export NETCDFPATH="/opt/cray/netcdf/4.3.2/INTEL/140"
  export WRFPATH="/lustre/f1/unswept/ncep/Shrinivas.Moorthi/nceplibs/nwprod/lib/sorc/WRFV3"
# export WRFPATH="/lustre/f1/unswept/ncep/Shrinivas.Moorthi/nceplibs/nwprod/lib/sorc/wrf_shared.v1.1.0"
  export NWPROD="/lustre/f1/unswept/ncep/Shrinivas.Moorthi/nceplibs/nwprod"
  export IPPATH=$NWPROD
  export SPPATH=$NWPROD
  export baciov=_v2.1.0
  export BACIOPATH=/lustre/f1/unswept/ncep/Shrinivas.Moorthi/nceplibs/nwprod/lib/sorc/bacio_fast_byteswap/bacio${baciov}_4
  export ipv=""
  export spv=_v2.0.1
  export xmlv=_v2.0.0
  export FC=ftn
  export CPP="/lib/cpp -P"
  export ARCH=""
  export CPPFLAGS="-DLINUX"
  export CC=icc
  if [ $debug = YES ] ; then
    export OPTS="-O0 -g"
    export DEBUG="-g -check all -ftrapuv -convert big_endian -fp-stack-check -fstack-protector -heap-arrays -recursive -traceback"
  else
    export export OPTS="-O3 -convert big_endian -traceback -g -fp-model source"
    export DEBUG=""
  fi
  export LIST=""
  export FREE=-FR
  export TRAPS=""
  export PROFILE=""

  export gfsiov=""
  export crtmv=2.0.7
  export w3ev=_v2.1.0
  export w3nv=""
fi
#export gfsiov=${gfsiov:-_v1.1.0}
export crtmv=${crtmv:-2.0.7}
export crtmv_inc=${crtmv_inc:-v$crtmv}
export XMLPATH=${XMLPATH:-$NWPROD}
export BACIOPATH=${BACIOPATH:-$NWPROD/lib}
export xmlv=${xmlv:-""}
export w3ev=${w3ev:-_v2.0.3}
#export w3nv=${w3nv:-_v2.0.3}
export ipv=${ipv:-""}
export spv=${spv:-""}

if [ ${CLEAN:-YES}  = YES ] ; then make -f Makefile_new$BMPYXML clean ; fi

export CFLAGS="-DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'"
if [ $machine = wcoss_c ] ; then
 if [ $make_nowrf = YES ] ; then
  export WRF_INC=""
  export WRF_LI=""
 else
  export WRF_INC="-I${WRFPATH}/external/io_quilt -I${WRFPATH}/frame"
  export WRF_LIB="${WRFPATH}/main/libwrflib.a ${WRFPATH}/frame/pack_utils.o ${WRFPATH}/frame/module_internal_header_util.o ${WRFPATH}/external/io_grib1/libio_grib1.a ${WRFPATH}/external/io_grib_share/libio_grib_share.a ${WRFPATH}/external/io_int/libwrfio_int.a ${WRFPATH}/external/io_netcdf/libwrfio_nf.a ${WRFPATH}/external/esmf_time_f90/libesmf_time.a ${WRFPATH}/external/RSL_LITE/librsl_lite.a"
 fi
 NETCDF_LIB="${NETCDF}/lib/libnetcdf.a"
 export FFLAGS="${OPTS} ${FREE} ${TRAPS} ${DEBUG} ${WRF_INC} -I${XMLPARSE_INC} -I${G2_INC4} -I${G2TMPL_INC} -I${NEMSIO_INC} -I${SIGIO_INC4} -I${SFCIO_INC4} -I${GFSIO_INC4} -I${W3EMC_INC4} -I${CRTM_INC} -I${NETCDF_INCLUDE} -I${PNG_INC}"

#export FFLAGS="${OPTS} ${FREE} ${TRAPS} ${DEBUG} -I${WRF_INC} -I${XMLPARSE_INC} -I${G2_INC4} -I${NEMSIO_INC} -I${SIGIO_INC4} -I${SFCIO_INC4} -I${W3EMC_INC4} -I${CRTM_INC} -I${NETCDF_INCLUDE} -I${GFSIO_INC}"

 export LIBS="${XMLPARSE_LIB} ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${NEMSIO_LIB} ${GFSIO_LIB4} ${SIGIO_LIB4} ${SFCIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${CRTM_LIB} ${WRF_LIB} ${NETCDF_LDFLAGS_F}"

#export LIBS=" ${WRF_LIB} ${XMLPARSE_LIB} ${G2_LIB4} ${G2TMPL_LIB} ${NEMSIO_LIB} ${GFSIO_LIB4} ${SIGIO_LIB4} ${SFCIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} ${CRTM_LIB}  ${NETCDF_LIB} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}"
#export LIBS="${XMLPARSE_LIB} ${G2_LIB4} ${G2TMPL_LIB} ${NEMSIO_LIB} ${GFSIO_LIB4} ${SIGIO_LIB4} ${SFCIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} ${CRTM_LIB}  ${NETCDF_LIB} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${WRF_LIB}"

else
 SFCIO_INC="-I${NWPROD}/lib/incmod/sfcio_4"
 SFCIO_LIB="${NWPROD}/lib/libsfcio_4.a"

# to use new avg version
 NEMSIO_INC="-I/nems/save/Jun.Wang/nceplibs/nemsio/nemsio_avg/incmod"
 NEMSIO_LIB="-L/nems/save/Jun.Wang/nceplibs/nemsio/nemsio_avg -lnemsio"

#NEMSIO_INC="-I${NWPROD}/lib/incmod/nemsio"
#NEMSIO_LIB="-L${NWPROD}/lib -lnemsio"

 BACIO_LIB="-L${BACIOPATH} -lbacio${baciov}_4"
 SIGIO_INC="-I${NWPROD}/lib/incmod/sigio_4"
 SIGIO_LIB="${NWPROD}/lib/libsigio_4.a"
 NCDLIBS="-L${NETCDFPATH} -lnetcdf"
 NCDFFLAGS="-I${NETCDFPATH}"
 if [ $make_nowrf = YES ] ; then
  export WRF_INC=""
  export WRF_LI=""
 else
  export WRF_INC="-I${WRFPATH}/external/io_quilt -I${WRFPATH}/frame"
  export WRF_LIB="${WRFPATH}/main/libwrflib.a                          \
                  ${WRFPATH}/frame/pack_utils.o
                  ${WRFPATH}/frame/module_internal_header_util.o       \
                  ${WRFPATH}/external/io_grib1/libio_grib1.a           \
                  ${WRFPATH}/external/io_grib_share/libio_grib_share.a \
                  ${WRFPATH}/external/io_int/libwrfio_int.a            \
                  ${WRFPATH}/external/io_netcdf/libwrfio_nf.a          \
                  ${WRFPATH}/external/esmf_time_f90/libesmf_time.a     \
                  ${WRFPATH}/external/RSL_LITE/librsl_lite.a"
 fi

 G2_INC="-I${NWPROD}/lib/incmod/g2_4 -I${NWPROD}/lib/incmod/g2tmpl${g2tv}"
 G2_LIB="-L${NWPROD}/lib -lg2tmpl${g2tv} -lg2_4 -ljasper -lpng -lz"

 GFSIO_INC="-I${NWPROD}/lib/incmod/gfsio${gfsiov}_4"
 GFSIO_LIB="-L${NWPROD}/lib -lgfsio${gfsiov}_4"

 IP_LIB="-L${IPPATH}/lib -lip${ipv}_4"
 SP_LIB="-L${SPPATH} -lsp${sp}_4"

 W3_INC="-I${NWPROD}/lib/incmod/w3emc${w3ev}_4"
 W3_LIB="-L${NWPROD}/lib -lw3nco${w3nv}_4 -lw3emc${w3ev}_4"

 CRTM_INC="-I${NWPROD}/lib/incmod/crtm_${crtmv_inc}"
 CRTM_LIB="-L${NWPROD}/lib -lcrtm_v${crtmv}"
 XML_INC="-I${XMLPATH}/lib/incmod/xmlparse${xmlv}"
 XML_LIB="-L${XMLPATH}/lib -lxmlparse${xmlv}"

 NETCDF_LIB="${NETCDFPATH}/lib/libnetcdf.a"
 NETCDF_INC="-I${NETCDFPATH}/include"

 export FFLAGS="${OPTS} ${FREE} ${TRAPS} ${DEBUG} ${WRF_INC} ${XML_INC} ${G2_INC} ${NEMSIO_INC} ${GFSIO_INC} ${SIGIO_INC} ${SFCIO_INC} ${W3_INC} ${CRTM_INC} ${NETCDF_INC}"

 export LIBS="${WRF_LIB} ${XML_LIB} ${G2_LIB} ${NEMSIO_LIB} ${GFSIO_LIB} ${SIGIO_LIB} ${SFCIO_LIB} ${IP_LIB} ${SP_LIB} ${W3_LIB} ${BACIO_LIB} ${CRTM_LIB} ${NETCDF_LIB}"

fi
if [ $make_post_lib = NO ] ; then
 if [ $make_post_exec = YES ] ; then
  if [ $make_nowrf = YES ] ; then
   make -f Makefile_nowrf_new${BMPYXML}
  else
   make -f Makefile_new${BMPYXML}
  fi
 fi
else
 if [ $make_post_exec = YES ] ; then
  if [ $make_nowrf = YES ] ; then
   make -f Makefile_nowrf_new${BMPYXML}
  else
   make -f Makefile_new${BMPYXML}
  fi
 fi
 export POSTLIBPATH=${POSTLIBPATH:-$(pwd)}
 if [ ${CLEAN:-YES}  = YES ] ; then rm -rf $POSTLIBPATH/incmod/post${BMPYXML}_4 ; fi
 mkdir -p $POSTLIBPATH/incmod/post${BMPYXML}_4
 make -f Makefile_lib_new${BMPYXML}
fi


