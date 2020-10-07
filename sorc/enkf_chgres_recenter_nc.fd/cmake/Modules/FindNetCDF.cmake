# - Find NetCDF
# Find the native NetCDF includes and library
#
#  NETCDF_INCLUDES    - where to find netcdf.h, etc
#  NETCDF_LIBRARIES   - Link these libraries when using NetCDF
#  NETCDF_FOUND       - True if NetCDF found including required interfaces (see below)
#
# Your package can require certain interfaces to be FOUND by setting these
#
#  NETCDF_CXX         - require the C++ interface and link the C++ library
#  NETCDF_F77         - require the F77 interface and link the fortran library
#  NETCDF_F90         - require the F90 interface and link the fortran library
#
# The following are not for general use and are included in
# NETCDF_LIBRARIES if the corresponding option above is set.
#
#  NETCDF_LIBRARIES_C    - Just the C interface
#  NETCDF_LIBRARIES_CXX  - C++ interface, if available
#  NETCDF_LIBRARIES_F77  - Fortran 77 interface, if available
#  NETCDF_LIBRARIES_F90  - Fortran 90 interface, if available
#
# Normal usage would be:
#  set (NETCDF_F90 "YES")
#  find_package (NetCDF REQUIRED)
#  target_link_libraries (uses_f90_interface ${NETCDF_LIBRARIES})
#  target_link_libraries (only_uses_c_interface ${NETCDF_LIBRARIES_C})


set(NETCDF_DIR $ENV{NETCDF})
message("Enviroment NETCDF is ${NetCDF}")

if (NETCDF_INCLUDES AND NETCDF_LIBRARIES)
  # Already in cache, be silent
  set (NETCDF_FIND_QUIETLY TRUE)
endif (NETCDF_INCLUDES AND NETCDF_LIBRARIES)

if(DEFINED ENV{NETCDF4}) 
  message("Enviroment NETCDF4 is ${NetCDF4}")
  set(NETCDF_DIR $ENV{NETCDF4})
elseif(DEFINED ENV{NETCDF_DIR})
  set(NETCDF_DIR $ENV{NETCDF_DIR})
elseif(DEFINED ENV{NETCDF_HOME})
  set(NETCDF_DIR $ENV{NETCDF_HOME})
elseif( DEFINED ENV{NETCDF} )
  set(NETCDF_DIR $ENV{NETCDF})
elseif(DEFINED ENV{SSEC_NETCDF4_DIR})
  set(NETCDF_DIR $ENV{SSEC_NETCDF4_DIR})
elseif(DEFINED ENV{SSEC_NETCDF_DIR})
  set(NETCDF_DIR $ENV{SSEC_NETCDF_DIR})
endif()
if(DEFINED ENV{NETCDF_FORTRAN}) 
  set(NETCDF_FORTRAN $ENV{NETCDF_FORTRAN})
elseif(DEFINED ENV{NETCDF_FORTRAN_DIR}) 
  set(NETCDF_FORTRAN $ENV{NETCDF_FORTRAN_DIR})
endif()
find_path (NETCDF_INCLUDES netcdf.h
  HINTS ${NETCDF_DIR}/include $ENV{SSEC_NETCDF_DIR}/include )

find_program (NETCDF_META netcdf_meta.h
    HINTS ${NETCDF_INCLUDES} ${CMAKE_INSTALL_PREFIX}
    )
if (NETCDF_META)
  file (STRINGS ${NETCDF_META} NETCDF_VERSION REGEX "define NC_VERSION_MAJOR")
  string (REGEX REPLACE "#define NC_VERSION_MAJOR " "" NETCDF_VERSION ${NETCDF_VERSION})
  string (REGEX REPLACE "\\/\\*\\!< netcdf-c major version. \\*\\/" "" NETCDF_VERSION ${NETCDF_VERSION})
  string (REGEX REPLACE " " "" NETCDF_VERSION ${NETCDF_VERSION} )
  if(${NETCDF_VERSION} GREATER "3")
    set(NETCDF_F90 "YES")
  endif()
endif (NETCDF_META)

find_library (NETCDF_flib 
     names libnetcdff.a netcdff.a libnetcdff.so netcdff.so 
     HINTS 
        ${NETCDF_DIR}/lib
        ${NETCDF_FORTRAN_DIR}/lib
        ${NETCDF_FORTRAN}/lib
        ${NETCDF_FORTRAN_ROOT}/lib
)

if (NETCDF_flib)
    set(NETCDF_F90 "YES")
    
endif()
find_library (NETCDF_LIBRARIES_C       
    NAMES netcdf
    HINTS ${NETCDF_DIR}/lib )
mark_as_advanced(NETCDF_LIBRARIES_C)

if("${NETCDF_DIR}" STREQUAL "")
  message(FATAL_ERROR "
         Cannot find NETCDF!!!!

         ")
endif()
find_file (NETCDF_NCDUMP
    NAMES ncdump
    HINTS ${NETCDF_DIR}/bin )
mark_as_advanced(NETCDF_NCDUMP)
execute_process(COMMAND ${NETCDF_NCDUMP} 
  ERROR_VARIABLE  NCDUMP_INFO)
string(FIND "${NCDUMP_INFO}" "version" VERSION_LOC REVERSE)
math(EXPR VERSION_LOC "${VERSION_LOC} + 9")
string(SUBSTRING "${NCDUMP_INFO}" ${VERSION_LOC} 1  NETCDF_MAJOR_VERSION)
if (${NETCDF_MAJOR_VERSION} LESS 4)
  message(FATAL_ERROR "
         Current NETCDF is ${NETCDF_DIR} 
         !!!! NETCDF version 4.0 and above is required !!!!

         ")
endif()

set (NetCDF_has_interfaces "YES") # will be set to NO if we're missing any interfaces
set (NetCDF_libs  ${NETCDF_LIBRARIES_C} ${NETCDF_LIBRARIES_Fortran})
message("netcdf_libs is ${NetCDF_libs}")
get_filename_component (NetCDF_lib_dirs "${NETCDF_LIBRARIES_C}" PATH)

macro (NetCDF_check_interface lang header libs)
  if (NETCDF_${lang})
    find_path (NETCDF_INCLUDES_${lang} NAMES ${header}
      HINTS ${NETCDF_INCLUDES} ${NETCDF_FORTRAN}/include  NO_DEFAULT_PATH)
    find_library (NETCDF_LIBRARIES_${lang} NAMES ${libs}
      HINTS ${NetCDF_lib_dirs} ${NETCDF_FORTRAN}/lib NO_DEFAULT_PATH)
    mark_as_advanced (NETCDF_INCLUDES_${lang} NETCDF_LIBRARIES_${lang})
    if (NETCDF_INCLUDES_${lang} AND NETCDF_LIBRARIES_${lang})
      list (INSERT NetCDF_libs 0 ${NETCDF_LIBRARIES_${lang}}) # prepend so that -lnetcdf is last
    else (NETCDF_INCLUDES_${lang} AND NETCDF_LIBRARIES_${lang})
      set (NetCDF_has_interfaces "NO")
      message (STATUS "Failed to find NetCDF interface for ${lang}")
    endif (NETCDF_INCLUDES_${lang} AND NETCDF_LIBRARIES_${lang})
  endif (NETCDF_${lang})
endmacro (NetCDF_check_interface)

NetCDF_check_interface (CXX netcdfcpp.h netcdf_c++)
NetCDF_check_interface (F77 netcdf.inc  netcdff)
NetCDF_check_interface (F90 netcdf.mod  netcdff)
if( NETCDF_LIBRARIES_F90 )
  set( NETCDF4 "YES" )
endif()

set (NETCDF_LIBRARIES "${NetCDF_libs}" CACHE STRING "All NetCDF libraries required for interface level")
# handle the QUIETLY and REQUIRED arguments and set NETCDF_FOUND to TRUE if
# all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (NetCDF DEFAULT_MSG NETCDF_LIBRARIES NETCDF_INCLUDES NetCDF_has_interfaces)

mark_as_advanced (NETCDF_LIBRARIES NETCDF_INCLUDES)
