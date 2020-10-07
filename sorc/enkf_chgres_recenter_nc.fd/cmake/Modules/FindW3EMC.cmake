# This module defines
#  CORE_INCS
#    List of include file paths for all required modules for GSI
#  CORE_LIBRARIES
#    Full list of libraries required to link GSI executable
include(findHelpers)
if(DEFINED ENV{W3EMC_VER})
  set(W3EMC_VER $ENV{W3EMC_VER})
  set(W3EMCINC $ENV{W3EMC_INCd} )
  set(W3EMC4INC $ENV{W3EMC_INC4} )
  STRING(REGEX REPLACE "v" "" W3EMC_VER ${W3EMC_VER})
endif()
if(DEFINED ENV{W3EMC_LIBd})
    set(W3EMC_LIBRARY $ENV{W3EMC_LIBd} )
    set(W3EMCINC $ENV{W3EMC_INCd} )
    set(W3EMC_4_LIBRARY $ENV{W3EMC_LIB4} )
    set(W3EMC4INC $ENV{W3EMC_INC4} )
    message("Setting W3EMC library via environment variable ${W3EMC_LIBRARY}")
endif()

set( NO_DEFAULT_PATH )
if((NOT BUILD_W3EMC ) AND ( NOT DEFINED W3EMC_LIBRARY ))
  if(DEFINED ENV{W3EMC_LIB} )
    set(W3EMC_LIBRARY $ENV{W3EMC_LIB} )
    set(W3EMCINC $ENV{W3EMC_INC} )
    set(W3EMC_4_LIBRARY $ENV{W3EMC_LIB4} )
    set(W3EMC4INC $ENV{W3EMC_INC4} )
    message("W3EMC library ${W3EMC_LIBRARY} set via Environment variable")
    message("W3EMC_4 library ${W3EMC_4_LIBRARY} set via Environment variable")
  else()
    find_path( W3EMCINC 
      NAMES mersenne_twister.mod 
      HINTS 
        $ENV{COREPATH}/lib/incmod/w3emc_d
        $ENV{COREPATH}/include 
        /usr/local/jcsda/nwprod_gdas_2014/lib/incmod/w3emc_d 
        ${COREPATH}/w3emc/v${W3EMC_VER}/incmod/w3emc_v${W3EMC_VER}_d
        ${COREPATH}/w3emc/v${W3EMC_VER}/intel/w3emc_v${W3EMC_VER}_d
        ${COREPATH}/w3emc/v${W3EMC_VER}/ips/${COMPILER_VERSION}/impi/${COMPILER_VERSION}/include/w3emc_v${W3EMC_VER}_d
        ${COREPATH}/w3emc/v${W3EMC_VER}/ips/${COMPILER_VERSION}/smpi/${COMPILER_VERSION}/include/w3emc_v${W3EMC_VER}_d
    )
    find_path( W3EMC4INC 
      NAMES mersenne_twister.mod 
      HINTS 
        $ENV{COREPATH}/lib/incmod/w3emc_4 
        $ENV{COREPATH}/include 
        /usr/local/jcsda/nwprod_gdas_2014/lib/incmod/w3emc_4 
        ${COREPATH}/w3emc/v${W3EMC_VER}/incmod/w3emc_v${W3EMC_VER}_4
        ${COREPATH}/w3emc/v${W3EMC_VER}/intel/w3emc_v${W3EMC_VER}_4
        ${COREPATH}/w3emc/v${W3EMC_VER}/ips/${COMPILER_VERSION}/impi/${COMPILER_VERSION}/include/w3emc_v${W3EMC_VER}_4
        ${COREPATH}/w3emc/v${W3EMC_VER}/ips/${COMPILER_VERSION}/smpi/${COMPILER_VERSION}/include/w3emc_v${W3EMC_VER}_4
    )
    find_library( W3EMC_LIBRARY 
    NAMES libw3emc_d.a libw3emc_v${W3EMC_VER}_d.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014	
      ${COREPATH}/w3emc/v${W3EMC_VER}
      ${COREPATH}/w3emc/v${W3EMC_VER}/intel
      ${COREPATH}/w3emc/v${W3EMC_VER}/ips/${COMPILER_VERSION}/impi/${COMPILER_VERSION}
      ${COREPATH}/w3emc/v${W3EMC_VER}/ips/${COMPILER_VERSION}/smpi/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    find_library( W3EMC_4_LIBRARY 
    NAMES libw3emc_4.a libw3emc_i4r4.a libw3emc_v${W3EMC_VER}_4.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014	
      ${COREPATH}/w3emc/v${W3EMC_VER}
      ${COREPATH}/w3emc/v${W3EMC_VER}/intel
      ${COREPATH}/w3emc/v${W3EMC_VER}/ips/${COMPILER_VERSION}/impi/${COMPILER_VERSION}
      ${COREPATH}/w3emc/v${W3EMC_VER}/ips/${COMPILER_VERSION}/smpi/${COMPILER_VERSION}
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    message("Found W3EMC_4 library ${W3EMC_4_LIBRARY}")
  endif()
endif()
if( NOT W3EMC_LIBRARY ) # didn't find the library, so build it from source
    message("Could not find W3EMC library, so building from libsrc")
    if( NOT DEFINED ENV{W3EMC_SRC} )
        findSrc( "w3emc" W3EMC_VER W3EMC_DIR )
        set(W3EMCINC  "${CMAKE_BINARY_DIR}/include" CACHE STRING "W3EMC Include Directory")
        set(W3EMC4INC  "${CMAKE_BINARY_DIR}/include" CACHE STRING "W3EMC4 Include Directory")
    else()
      set( W3EMC_DIR "$ENV{W3EMC_SRC}/libsrc" CACHE STRING "W3EMC Source Location")
    endif()
    set( libsuffix "_v${W3EMC_VER}${debug_suffix}" )
    set( W3EMC_LIBRARY "${LIBRARY_OUTPUT_PATH}/libw3emc${libsuffix}.a" CACHE STRING "W3EMC Library" )
    set( w3emc "w3emc${libsuffix}")
    set( w3emc4 "w3emc_4${libsuffix}")
    set( BUILD_W3EMC "ON" CACHE INTERNAL "Build the W3EMC library")
    add_subdirectory(${CMAKE_SOURCE_DIR}/libsrc/w3emc)
    set( W3EMC_LIBRARY ${w3emc} )
    set( W3EMC_4_LIBRARY ${w3emc} )
    set(W3EMCINC  "${CMAKE_BINARY_DIR}/include" CACHE STRING "W3EMC Include Directory")
    set(W3EMC4INC  ${CMAKE_INCLUDE_4_OUTPUT_DIRECTORY} CACHE STRING "W3EMC4 Include Directory")
  if( CORE_BUILT )
    list( APPEND CORE_BUILT ${W3EMC_LIBRARY} )
    list( APPEND CORE_BUILT ${W3EMC_4_LIBRARY} )
  else()
    set( CORE_BUILT ${W3EMC_LIBRARY} )
    set( CORE_BUILT ${W3EMC_4_LIBRARY} )
  endif()
else( NOT W3EMC_LIBRARY )
  if( CORE_LIBRARIES )
    list( APPEND CORE_LIBRARIES ${W3EMC_LIBRARY} )
  else()
    set( CORE_LIBRARIES ${W3EMC_LIBRARY} )
  endif()
endif( NOT W3EMC_LIBRARY )

if( CORE_INCS )
  list( APPEND CORE_INCS ${W3EMCINC} )
else()
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${W3EMCINC} )
endif()

set( W3EMC_LIBRARY_PATH ${W3EMC_LIBRARY} CACHE STRING "W3EMC Library Location" )
set( W3EMC_INCLUDE_PATH ${W3EMCINC} CACHE STRING "W3EMC Include Location" )
set( W3EMC_4_LIBRARY_PATH ${W3EMC_4_LIBRARY} CACHE STRING "W3EMC_4 Library Location" )
set( W3EMC_INCLUDE_4_PATH ${W3EMC4INC} CACHE STRING "W3EMC_4 Include Location" )

