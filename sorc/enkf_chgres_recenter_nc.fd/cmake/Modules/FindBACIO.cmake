# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{BACIO_VER})
  set(BACIO_VER $ENV{BACIO_VER})
  STRING(REGEX REPLACE "v" "" BACIO_VER ${BACIO_VER})
endif()
if(NOT BUILD_BACIO )
  if(DEFINED ENV{BACIO_LIB4})
    set(BACIO_LIBRARY $ENV{BACIO_LIB4} )
  else()
    find_library( BACIO_LIBRARY 
      NAMES libbacio.a libbacio_4.a libbacio_v${BACIO_VER}_4.a 
      HINTS $ENV{COREPATH}/lib /usr/local/jcsda/nwprod_gdas_2014/lib	
          ${COREPATH}/bacio/v${BACIO_VER}
          ${COREPATH}/bacio/v${BACIO_VER}/intel
          ${COREPATH}/bacio/v${BACIO_VER}/ips/${COMPILER_VERSION}
      PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH}
      )
    message("Found BACIO library ${BACIO_LIBRARY}")
  endif()
endif()
if( NOT BACIO_LIBRARY ) # didn't find the library, so build it from source
    message("Could not find BACIO library, so building from libsrc")
    if( DEFINED ENV{BACIO_SRC} )
      set( BACIO_DIR $ENV{BACIO_SRC} CACHE STRING "BACIO Source Directory" )
    else()
      findSrc( "bacio" BACIO_VER BACIO_DIR )
      set(BACIOINC  "${CMAKE_BINARY_DIR}/include")
    endif()
    set( libsuffix "_v${BACIO_VER}${debug_suffix}" )
    set( bacio "bacio${libsuffix}")
    set( BUILD_BACIO "ON" CACHE INTERNAL "Build Bacio library" )
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/bacio)
    set( BACIO_LIBRARY ${bacio} )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${BACIO_LIBRARY} )
    else()
      set( CORE_BUILT ${BACIO_LIBRARY} )
    endif()
else( NOT BACIO_LIBRARY )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${BACIO_LIBRARY} )
  else()
    set( CORE_LIBRARIES ${BACIO_LIBRARY} )
  endif()
endif( NOT BACIO_LIBRARY )

set( BACIO_LIBRARY_PATH ${BACIO_LIBRARY} CACHE STRING "BACIO Library Location" )

