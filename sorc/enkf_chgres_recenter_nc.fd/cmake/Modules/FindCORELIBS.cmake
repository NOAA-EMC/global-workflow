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
if(DEFINED ENV{NEMSIO_VER})
  set(NEMSIO_VER $ENV{NEMSIO_VER})
  STRING(REGEX REPLACE "v" "" NEMSIO_VER ${NEMSIO_VER})
endif()
if(DEFINED ENV{SFCIO_VER})
  set(SFCIO_VER $ENV{SFCIO_VER})
  STRING(REGEX REPLACE "v" "" SFCIO_VER ${SFCIO_VER})
endif()
if(DEFINED ENV{SIGIO_VER})
  set(SIGIO_VER $ENV{SIGIO_VER})
  STRING(REGEX REPLACE "v" "" SIGIO_VER ${SIGIO_VER})
endif()
if(DEFINED ENV{SP_VER})
  set(SP_VER $ENV{SP_VER})
  STRING(REGEX REPLACE "v" "" SP_VER ${SP_VER})
endif()
if(DEFINED ENV{W3EMC_VER})
  set(W3EMC_VER $ENV{W3EMC_VER})
  STRING(REGEX REPLACE "v" "" W3EMC_VER ${W3EMC_VER})
endif()
if(DEFINED ENV{W3NCO_VER})
  set(W3NCO_VER $ENV{W3NCO_VER})
  STRING(REGEX REPLACE "v" "" W3NCO_VER ${W3NCO_VER})
endif()

set (CORE_DEPS " ")
set( NO_DEFAULT_PATH )
if(NOT  BUILD_EMC  )
  if(DEFINED ENV{W3EMC_LIBd} )
    set(W3EMC_LIBRARY $ENV{W3EMC_LIBd} )
    set(W3EMCINC $ENV{W3EMC_INCd} )
  else()
  find_path( W3EMCINC 
    NAMES mersenne_twister.mod 
    HINTS 
      $ENV{COREPATH}/lib/incmod/w3emc_4 
      $ENV{COREPATH}/include 
      /usr/local/jcsda/nwprod_gdas_2014/lib/lib/incmod/w3emc_4 
      ${COREPATH}/w3emc/v${W3EMC_VER}/incmod/w3emc_v${W3EMC_VER}_d
      ${COREPATH}/w3emc/v${W3EMC_VER}/intel/w3emc_v${W3EMC_VER}_d
  )
  find_library( W3EMC_LIBRARY 
    NAMES libw3emc_4.a libw3emc_i4r8.a libw3emc_v${W3EMC_VER}_d.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/w3emc/v${W3EMC_VER}
      ${COREPATH}/w3emc/v${W3EMC_VER}/intel
    PATH_SUFFIXES
        lib
    )
    message("Found W3EMC library ${W3EMC_LIBRARY}")
  endif()
else()
    set( libsuffix "_v${W3EMC_VER}${debug_suffix}" )
    set( W3EMC_LIBRARY "${LIBRARY_OUTPUT_PATH}/libw3emc${libsuffix}.a" CACHE STRING "W3EMC Library" )
    set( w3emc "w3emc${libsuffix}")
    if( DEFINED ENV{W3EMC_SRC} )
      set( W3EMC_DIR $ENV{W3EMC_SRC} CACHE STRING "W3EMC Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "w3emc" W3EMC_VER W3EMC_DIR )
      endif()
    endif()
endif()
if(NOT  BUILD_NCO )
  if(DEFINED ENV{W3NCO_LIBd} )
    set(W3NCO_LIBRARY $ENV{W3NCO_LIBd} )
  else()
  find_library( W3NCO_LIBRARY 
    NAMES libw3nco_v${W3NCO_VER}_d.a libw3nco_d.a  libw3nco_i4r8.a 
    HINTS 
       $ENV{COREPATH}/lib 
       /usr/local/jcsda/nwprod_gdas_2014/lib	
       ${COREPATH}/w3nco/v${W3NCO_VER}
       ${COREPATH}/w3nco/v${W3NCO_VER}/intel
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    message("Found W3NCO library ${W3NCO_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{W3NCO_SRC} )
      set( W3NCO_DIR $ENV{W3NCO_SRC} CACHE STRING "W3NCO Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "w3nco" W3NCO_VER W3NCO_DIR )
      endif()
    endif()
    set( libsuffix "_v${W3NCO_VER}${debug_suffix}" )
    set( W3NCO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libw3nco${libsuffix}.a" CACHE STRING "W3NCO Library" )
    set( w3nco "w3nco${libsuffix}")
endif()
if(NOT  BUILD_BUFR  )
  if(DEFINED ENV{BUFR_LIBd} )
    set(BUFR_LIBRARY $ENV{BUFR_LIBd} )
  else()
  find_library( BUFR_LIBRARY 
    NAMES libbufr.a libbufr_d_64.a libbufr_i4r8.a libbufr_v${BUFR_VER}_d_64.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/bufr/v${BUFR_VER}
      ${COREPATH}/bufr/v${BUFR_VER}/intel
    PATH_SUFFIXES
        lib
     ${NO_DEFAULT_PATH})
    set( bufr "bufr_v${BUFR_VER}")
    message("Found BUFR library ${BUFR_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{BUFR_SRC} )
      set( BUFR_DIR $ENV{BUFR_SRC} CACHE STRING "BUFR Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "bufr" BUFR_VER BUFR_DIR )
      endif()
    endif()
    set( libsuffix "_v${BUFR_VER}${debug_suffix}" )
    set( BUFR_LIBRARY "${LIBRARY_OUTPUT_PATH}/libbufr${libsuffix}.a" CACHE STRING "BUFR Library" )
    set( bufr "bufr${libsuffix}")
endif()
if(NOT  BUILD_SFCIO )
  if(DEFINED ENV{SFCIO_LIB4} )
    set(SFCIO_LIBRARY $ENV{SFCIO_LIB4} )
    set(SFCIOINC $ENV{SFCIO_INC4} )
  else()
  findInc( sfcio SFCIO_VER SFCIOINC )
  find_library( SFCIO_LIBRARY 
    NAMES libsfcio.a libsfcio_4.a libsfcio_i4r4.a libsfcio_v${SFCIO_VER}_4.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/sfcio/v${SFCIO_VER}
      ${COREPATH}/sfcio/v${SFCIO_VER}/intel
    PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH})
    set( sfcio "sfcio_v${SFCIO_VER}_4")
    message("Found SFCIO library ${SFCIO_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{SFCIO_SRC} )
      set( SFCIO_DIR $ENV{SFCIO_SRC} CACHE STRING "SFCIO Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "sfcio" SFCIO_VER SFCIO_DIR )
      endif()
    endif()
    set( libsuffix "_v${SFCIO_VER}${debug_suffix}" )
    set( SFCIO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libsfcio${libsuffix}.a" CACHE STRING "SFCIO Library" )
    set( sfcio "sfcio${libsuffix}")
endif()
if(NOT  BUILD_SIGIO )
  if(DEFINED ENV{SIGIO_LIB4} )
    set(SIGIO_LIBRARY $ENV{SIGIO_LIB4} )
    set(SIGIOINC $ENV{SIGIO_INC4} )
  else()
  findInc( sigio SIGIO_VER SIGIOINC )
  message("SIGIOINC is ${SIGIOINC}")
  find_library( SIGIO_LIBRARY 
    NAMES libsigio.a libsigio_4.a libsigio_i4r4.a libsigio_v${SIGIO_VER}_4.a
    HINTS 
     $ENV{COREPATH}/lib 
     /usr/local/jcsda/nwprod_gdas_2014/lib	
     ${COREPATH}/sigio/v${SIGIO_VER}
     ${COREPATH}/sigio/v${SIGIO_VER}/intel
    PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH})
    set( sigio "sigio_v${SIGIO_VER}_4")
    message("Found SIGIO library ${SIGIO_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{SIGIO_SRC} )
      set( SIGIO_DIR $ENV{SIGIO_SRC} CACHE STRING "SIGIO Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "sigio" SIGIO_VER SIGIO_DIR )
      endif()
    endif()
    set( libsuffix "_v${SIGIO_VER}${debug_suffix}" )
    set( SIGIO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libsigio${libsuffix}.a" CACHE STRING "SIGIO Library" )
    set( sigio "sigio${libsuffix}")
    set( CORE_DEPS "${CORE_DEPS} ${baseName}" )
endif()
if(NOT  BUILD_NEMSIO )
  if(DEFINED ENV{NEMSIO_LIB} )
    set(NEMSIO_LIBRARY $ENV{NEMSIO_LIB} )
    set(NEMSIOINC $ENV{NEMSIO_INC} )
  else()
  findInc( nemsio NEMSIO_VER NEMSIOINC )
  find_library( NEMSIO_LIBRARY 
    NAMES libnemsio.a libnemsio_v${NEMSIO_VER}.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/nemsio/v${NEMSIO_VER}
      ${COREPATH}/nemsio/v${NEMSIO_VER}/intel
    PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH})
    set( nemsio "nemsio_v${NEMSIO_VER}")
    message("Found NEMSIO library ${NEMSIO_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{NEMSIO_SRC} )
      set( NEMSIO_DIR $ENV{NEMSIO_SRC} CACHE STRING "NEMSIO Source Directory" )
    else()
      if( FIND_SRC ) 
        findSrc( "nemsio" NEMSIO_VER NEMSIO_DIR )
      endif()
    endif()
    set( libsuffix "_v${NEMSIO_VER}${debug_suffix}" )
    set( NEMSIO_LIBRARY "${LIBRARY_OUTPUT_PATH}/libnemsio${libsuffix}.a" CACHE STRING "NEMSIO Library" )
    set( nemsio "nemsio${libsuffix}")
endif()
if(NOT  BUILD_SP )
  if(DEFINED ENV{SP_LIBd} )
    set(SP_LIBRARY $ENV{SP_LIBd} )
  else()
  find_library( SP_LIBRARY 
    NAMES libsp_d.a libsp_i4r8.a libsp_v${SP_VER}_d.a
    HINTS 
      $ENV{COREPATH}/lib 
      /usr/local/jcsda/nwprod_gdas_2014/lib	
      ${COREPATH}/sp/v${SP_VER}
      ${COREPATH}/sp/v${SP_VER}/intel
    PATH_SUFFIXES
        lib
       ${NO_DEFAULT_PATH})
    set( sp "sp_v${SP_VER}_d")
    message("Found SP library ${SP_LIBRARY}")
  endif()
else()
    if( DEFINED ENV{SP_SRC} )
      set( SP_DIR $ENV{SP_SRC} CACHE STRING "SP Source Directory" )
    else() 
      if( FIND_SRC ) 
        findSrc( "sp" SP_VER SP_DIR )
      endif()
    endif()
    set( libsuffix "_v${SP_VER}${debug_suffix}" )
    set( SP_LIBRARY "${LIBRARY_OUTPUT_PATH}/libsp${libsuffix}.a" CACHE STRING "SP Library" )
    set( sp "sp${libsuffix}")
endif()

if( CORE_LIBRARIES )
  list( APPEND CORE_LIBRARIES ${SFCIO_LIBRARY} ${SIGIO_LIBRARY} 
                  ${NEMSIO_LIBRARY} ${SP_LIBRARY} ${W3NCO_LIBRARY} ${BUFR_LIBRARY}  
                  ${W3EMC_LIBRARY} CACHE INTERNAL "List of Core libs" )
  list( APPEND CORE_INCS ${INCLUDE_OUTPUT_PATH} ${SFCIOINC} ${SIGIOINC} ${NEMSIOINC} ${W3EMCINC}  )
else()
  set( CORE_LIBRARIES ${SFCIO_LIBRARY} ${SIGIO_LIBRARY} 
                  ${NEMSIO_LIBRARY} ${SP_LIBRARY} ${W3NCO_LIBRARY} ${BUFR_LIBRARY}  
                  ${W3EMC_LIBRARY} CACHE INTERNAL "List of Core libs" )
  set( CORE_INCS ${INCLUDE_OUTPUT_PATH} ${SFCIOINC} ${SIGIOINC} ${NEMSIOINC} ${W3EMCINC}  )
endif()

set( BUFR_LIBRARY_PATH ${BUFR_LIBRARY} CACHE STRING "BUFR Library Location" )

set( SFCIO_LIBRARY_PATH ${SFCIO_LIBRARY} CACHE STRING "SFCIO Library Location" )
set( SFCIO_INCLUDE_PATH ${SFCIOINC} CACHE STRING "SFCIO Include Location" )

set( SIGIO_LIBRARY_PATH ${SIGIO_LIBRARY} CACHE STRING "SIGIO Library Location" )
set( SIGIO_INCLUDE_PATH ${SIGIOINC} CACHE STRING "SIGIO Include Location" )

set( W3NCO_LIBRARY_PATH ${W3NCO_LIBRARY} CACHE STRING "W3NCO Library Location" )

set( W3EMC_LIBRARY_PATH ${W3EMC_LIBRARY} CACHE STRING "W3EMC Library Location" )
set( W3EMC_INCLUDE_PATH ${W3EMCINC} CACHE STRING "W3EMC Include Location" )

set( NEMSIO_LIBRARY_PATH ${NEMSIO_LIBRARY} CACHE STRING "NEMSIO Library Location" )
set( NEMSIO_INCLUDE_PATH ${NEMSIOINC} CACHE STRING "NEMSIO Include Location" )

set( SP_LIBRARY_PATH ${SP_LIBRARY} CACHE STRING "SP Library Location" )

