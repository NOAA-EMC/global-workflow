if (NOT BASEDIR)
  if(${COMPILER_TYPE} STREQUAL "intel" )
    string(REGEX MATCH "mpt" MPT ${MPI_Fortran_INCLUDE_PATH}) 
    string(REGEX MATCH "impi" IMPI ${MPI_Fortran_INCLUDE_PATH}) 
    message("REGEX returns ${MPT} ")
    if( MPT MATCHES "mpt" )
      message("setting mpt paths ")
      set(BASEDIR "/discover/swdev/mathomp4/Baselibs/GMAO-Baselibs-5_0_2/x86_64-unknown-linux-gnu/ifort_15.0.2.164-mpt_2.14/Linux")
    elseif( IMPI MATCHES "impi" )
      set(BASEDIR "/discover/swdev/mathomp4/Baselibs/GMAO-Baselibs-5_0_2/x86_64-unknown-linux-gnu/ifort_16.0.3.210-intelmpi_5.1.3.210/Linux")
    else()
      message (FATAL_ERROR "ERROR: Could not find matching BASELIBS Must specify a value for BASEDIR with cmake ... -DBASEDIR=<path>.")
    endif()
    message("compiler version is ${COMPILER_VERSION}")
  endif()
  if(${COMPILER_TYPE} STREQUAL "gnu" )
    string(REGEX MATCH "openmpi" OPENMPI ${MPI_Fortran_INCLUDE_PATH}) 
    message("REGEX returns ${OPENMPI} ")
    if( OPENMPI MATCHES "openmpi" )
      message("setting openmpi paths ")
      set(BASEDIR "/discover/swdev/mathomp4/Baselibs/GMAO-Baselibs-4_0_8/x86_64-unknown-linux-gnu/gfortran_7.2.0-openmpi_3.0.0/Linux")
    else()
      message (FATAL_ERROR "ERROR: Could not find matching BASELIBS Must specify a value for BASEDIR with cmake ... -DBASEDIR=<path>.")
    endif()
    message("compiler version is ${COMPILER_VERSION}")
  endif()
  if(${COMPILER_TYPE} STREQUAL "pgi" )
    string(REGEX MATCH "openmpi" OPENMPI ${MPI_Fortran_INCLUDE_PATH}) 
    if( OPENMPI MATCHES "openmpi" )
      set(BASEDIR "/discover/swdev/mathomp4/Baselibs/GMAO-Baselibs-5_0_1/x86_64-unknown-linux-gnu/pgfortran_16.5-openmpi_1.10.3/Linux")
    else()
      message (FATAL_ERROR "ERROR: Could not find matching BASELIBS Must specify a value for BASEDIR with cmake ... -DBASEDIR=<path>.")
    endif()
    message("compiler version is ${COMPILER_VERSION}")
  endif()
endif ()
if (ESMA_SDF)
  message (FATAL_ERROR "ERROR: -hdf option was thought to be obsolete when CMake was crafted.")
endif ()

link_directories (${BASEDIR}/lib)

#------------------------------------------------------------------
# netcdf
# The following command provides the list of libraries that netcdf
# uses.  Unfortunately it also includes the library path and "-l"
# prefixes, which CMake handles in a different manner. So we need so
# strip off that item from the list
execute_process (
  COMMAND ${BASEDIR}/bin/nf-config --flibs
  OUTPUT_VARIABLE LIB_NETCDF
  )

string(REGEX MATCHALL " -l[^ ]*" _full_libs "${LIB_NETCDF}")
set (NETCDF_LIBRARIES)
foreach (lib ${_full_libs})
  string (REPLACE "-l" "" _tmp ${lib})
  string (STRIP ${_tmp} _tmp)
  list (APPEND NETCDF_LIBRARIES ${_tmp})
endforeach()
#------------------------------------------------------------------

list(APPEND NETCDF_INCLUDES ${BASEDIR}/include/netcdf)
list(APPEND NETCDF_INCLUDES ${BASEDIR}/include/hdf5)

message(STATUS "NETCDF_INCLUDES: ${NETCDF_INCLUDES}")
message(STATUS "NETCDF_LIBRARIES: ${NETCDF_LIBRARIES}")

