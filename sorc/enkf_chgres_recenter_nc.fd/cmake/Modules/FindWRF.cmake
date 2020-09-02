# - Find the WRF modules

set( NO_DEFAULT_PATH )
find_library( IOINT_LIB  
    NAMES libwrfio_int.a
    HINTS
        ${WRFPATH}/external/io_int 
        $ENV{WRFPATH}/external/io_int 
        /usr/local/jcsda/nwprod_gdas_2014/lib/sorc/nam_nmm_real_fcst.fd/external/io_int
        /scratch3/NCEPDEV/nceplibs/ext/WRF/3.7/WRFV3/external/io_int
    ${NO_DEFAULT_PATH})

find_library( WRFNETCDF_LIB
    NAMES libwrfio_nf.a
    HINTS
       ${WRFPATH}/external/io_netcdf /usr/local/jcsda/nwprod_gdas_2014/lib/sorc/nam_nmm_real_fcst.fd/external/io_netcdf
       $ENV{WRFPATH}/external/io_netcdf /usr/local/jcsda/nwprod_gdas_2014/lib/sorc/nam_nmm_real_fcst.fd/external/io_netcdf
        /scratch3/NCEPDEV/nceplibs/ext/WRF/3.7/WRFV3/external/io_netcdf
    ${NO_DEFAULT_PATH})
find_file( FRAMEPACK
    NAMES pack_utils.o
    HINTS
        ${WRFPATH}/frame /usr/local/jcsda/nwprod_gdas_2014/lib/sorc/nam_nmm_real_fcst.fd/frame
        $ENV{WRFPATH}/frame /usr/local/jcsda/nwprod_gdas_2014/lib/sorc/nam_nmm_real_fcst.fd/frame
        /scratch3/NCEPDEV/nceplibs/ext/WRF/3.7/WRFV3/frame
    ${NO_DEFAULT_PATH})
find_file( FRAMEMODULE
    NAMES module_machine.o
    HINTS
        ${WRFPATH}/frame /usr/local/jcsda/nwprod_gdas_2014/lib/sorc/nam_nmm_real_fcst.fd/frame
        $ENV{WRFPATH}/frame /usr/local/jcsda/nwprod_gdas_2014/lib/sorc/nam_nmm_real_fcst.fd/frame
        /scratch3/NCEPDEV/nceplibs/ext/WRF/3.7/WRFV3/frame
    ${NO_DEFAULT_PATH})

set( WRF_LIBRARIES ${IOINT_LIB} ${WRFNETCDF_LIB} ${FRAMEPACK} ${FRAMEMODULE} )

