# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{SP_VER})
  set(SP_VER $ENV{SP_VER})
  STRING(REGEX REPLACE "v" "" SP_VER ${SP_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_SP )
  if(DEFINED ENV{SP_LIBd} )
    set(SP_LIBRARY $ENV{SP_LIBd} )
    message("SP library ${SP_LIBRARY} set via Environment variable")
  else()
    find_library( SP_LIBRARY 
    NAMES libsp_d.a libsp_i4r8.a libsp_v${SP_VER}_d.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/sp/v${SP_VER}
      ${COREPATH}/sp/v${SP_VER}/intel
      ${COREPATH}/sp/v${SP_VER}/ips/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( sp "sp_v${SP_VER}_d")
    message("Found SP library ${SP_LIBRARY}")
  endif()
  if(DEFINED ENV{SP_LIB4} )
    set(SP_4_LIBRARY $ENV{SP_LIB4} )
    message("SP library ${SP_4_LIBRARY} set via Environment variable")
  else()
    find_library( SP_4_LIBRARY
    NAMES libsp_4.a libsp_i4r4.a libsp_v${SP_VER}_4.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/sp/v${SP_VER}
      ${COREPATH}/sp/v${SP_VER}/intel
      ${COREPATH}/sp/v${SP_VER}/ips/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( sp "sp_v${SP_VER}_4")
    message("Found SP_4 library ${SP_4_LIBRARY}")
  endif()
endif()
if( NOT SP_LIBRARY ) # didn't find the library, so build it from source
    message("Could not find SP library, so building from libsrc")
    if( NOT DEFINED ENV{SP_SRC} )
        findSrc( "sp" SP_VER SP_DIR )
    else()
      set( SP_DIR "$ENV{SP_SRC}/libsrc" CACHE STRING "SP Source Location")
    endif()
    set( libsuffix "_v${SP_VER}${debug_suffix}" )
    set( SP_LIBRARY "${LIBRARY_OUTPUT_PATH}/libsp${libsuffix}.a" CACHE STRING "SP Library" )
    set( SP_4_LIBRARY "${LIBRARY_OUTPUT_PATH}/libsp_4${libsuffix}.a" CACHE STRING "SP_4 Library" )
    set( sp "sp${libsuffix}")
    set( sp4 "sp_4${libsuffix}")
    set( BUILD_SP "ON" CACHE INTERNAL "Build the SP library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/sp)
    set( SP_LIBRARY ${sp} )
    set( SP_4_LIBRARY ${sp4} )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${SP_LIBRARY} )
    else()
      set( CORE_BUILT ${SP_LIBRARY} )
    endif()
else( NOT SP_LIBRARY )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${SP_LIBRARY} )
  else()
    set( CORE_LIBRARIES ${SP_LIBRARY} )
  endif()
endif( NOT SP_LIBRARY )


set( SP_LIBRARY_PATH ${SP_LIBRARY} CACHE STRING "SP Library Location" )
set( SP_4_LIBRARY_PATH ${SP_4_LIBRARY} CACHE STRING "SP_4 Library Location" )

