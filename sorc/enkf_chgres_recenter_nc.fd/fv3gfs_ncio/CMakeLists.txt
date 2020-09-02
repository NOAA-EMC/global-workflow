cmake_minimum_required(VERSION 2.8)
if(BUILD_FV3GFS_NCIO)
  set(Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  # NetCDF-4 library
  include_directories( ${NETCDF_INCLUDES} ${FV3GFS_NCIO_INCS} )
  FILE(GLOB FV3GFS_NCIO_SRC ${CMAKE_CURRENT_SOURCE_DIR}/module_fv3gfs_ncio.f90 )
  set_source_files_properties( ${FV3GFS_NCIO_SRC} PROPERTIES COMPILE_FLAGS ${FV3GFS_NCIO_Fortran_FLAGS} )
  add_library(fv3gfs_ncio STATIC ${FV3GFS_NCIO_SRC})
endif(BUILD_FV3GFS_NCIO)

