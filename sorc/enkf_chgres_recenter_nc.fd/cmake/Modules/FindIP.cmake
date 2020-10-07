# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{IP_VER})
  set(IP_VER $ENV{IP_VER})
  STRING(REGEX REPLACE "v" "" IP_VER ${IP_VER})
endif()

set( NO_DEFAULT_PATH )
if(NOT BUILD_IP )
  if(DEFINED ENV{IP_LIBd} )
    set(IP_LIBRARY $ENV{IP_LIBd} )
    message("IP library ${IP_LIBRARY} set via Environment variable")
  else()
    find_library( IP_LIBRARY 
    NAMES libip_d.a libip_i4r8.a libip_v${IP_VER}_d.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/ip/v${IP_VER}
      ${COREPATH}/ip/v${IP_VER}/intel
      ${COREPATH}/ip/v${IP_VER}/ips/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( ip "ip_v${IP_VER}_d")
    message("Found IP library ${IP_LIBRARY}")
  endif()
  if(DEFINED ENV{IP_LIB4} )
    set(IP_4_LIBRARY $ENV{IP_LIB4} )
    message("IP 4 library ${IP_4_LIBRARY} set via Environment variable")
  else()
    find_library( IP_4_LIBRARY
    NAMES libip_4.a libip_i4r4.a libip_v${IP_VER}_4.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/ip/v${IP_VER}
      ${COREPATH}/ip/v${IP_VER}/intel
      ${COREPATH}/ip/v${IP_VER}/ips/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( ip "ip_v${IP_VER}_4")
    message("Found IP_4 library ${IP_4_LIBRARY}")
  endif()
endif()
if( NOT IP_LIBRARY ) # didn't find the library, so build it from source
    message("Could not find IP library, so building from libsrc")
    if( NOT DEFINED ENV{IP_SRC} )
        findSrc( "ip" IP_VER IP_DIR )
    else()
      set( IP_DIR "$ENV{IP_SRC}/libsrc" CACHE STRING "IP Source Location")
    endif()
    set( libsuffix "_v${IP_VER}${debug_suffix}" )
    set( IP_LIBRARY "${LIBRARY_OUTPUT_PATH}/libip${libsuffix}.a" CACHE STRING "IP Library" )
    set( IP_4_LIBRARY "${LIBRARY_OUTPUT_PATH}/libip_4${libsuffix}.a" CACHE STRING "IP_4 Library" )
    set( ip "ip${libsuffix}")
    set( ip4 "ip_4${libsuffix}")
    set( BUILD_IP "ON" CACHE INTERNAL "Build the IP library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/ip)
    set( IP_LIBRARY ${ip} )
    set( IP_4_LIBRARY ${ip4} )
    if( CORE_BUILT )
      list( APPEND CORE_BUILT ${IP_LIBRARY} )
    else()
      set( CORE_BUILT ${IP_LIBRARY} )
    endif()
else( NOT IP_LIBRARY )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${IP_LIBRARY} )
  else()
    set( CORE_LIBRARIES ${IP_LIBRARY} )
  endif()
endif( NOT IP_LIBRARY )


set( IP_LIBRARY_PATH ${IP_LIBRARY} CACHE STRING "IP Library Location" )
set( IP_4_LIBRARY_PATH ${IP_4_LIBRARY} CACHE STRING "IP_4 Library Location" )

