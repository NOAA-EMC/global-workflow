# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{BUFR_VER})
  set(BUFR_VER $ENV{BUFR_VER})
  STRING(REGEX REPLACE "v" "" BUFR_VER ${BUFR_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_BUFR )
  if(DEFINED ENV{BUFR_LIBd} )
    set(BUFR_LIBRARY $ENV{BUFR_LIBd} )
    message("BUFR library ${BUFR_LIBRARY} set via Environment variable")
  else()
  find_library( BUFR_LIBRARY 
    NAMES libbufr.a libbufr_d_64.a libbufr_i4r8.a libbufr_v${BUFR_VER}_d_64.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/bufr/v${BUFR_VER}
      ${COREPATH}/bufr/v${BUFR_VER}/intel
      ${COREPATH}/bufr/v${BUFR_VER}/ips/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( bufr "bufr_v${BUFR_VER}")
    message("Found BUFR library ${BUFR_LIBRARY}")
  endif()
endif()
if( NOT BUFR_LIBRARY ) # didn't find the library, so build it from source
    message("Could not find BUFR library, so building from libsrc")
    if( NOT DEFINED ENV{BUFR_SRC} )
        findSrc( "bufr" BUFR_VER BUFR_DIR )
    else()
      set( BUFR_DIR "$ENV{BUFR_SRC}/libsrc" CACHE STRING "BUFR Source Location")
    endif()
    set( libsuffix "_v${BUFR_VER}${debug_suffix}" )
    set( BUFR_LIBRARY "${LIBRARY_OUTPUT_PATH}/libbufr${libsuffix}.a" CACHE STRING "BUFR Library" )
    set( bufr "bufr${libsuffix}")
    set( BUILD_BUFR "ON" CACHE INTERNAL "Build the BUFR library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/bufr)
    set( BUFR_LIBRARY ${bufr} )

    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${BUFR_LIBRARY} )
    else()
      set( CORE_BUILT ${BUFR_LIBRARY} )
    endif()
else( NOT BUFR_LIBRARY )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${BUFR_LIBRARY} )
  else()
    set( CORE_LIBRARIES ${BUFR_LIBRARY} )
  endif()
endif()
set( BUFR_LIBRARY_PATH ${BUFR_LIBRARY} CACHE STRING "BUFR Library Location" )

