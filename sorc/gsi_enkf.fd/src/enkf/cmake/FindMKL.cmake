# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find MKL
# Once done this will define
#
#  MKL_FOUND         - system has Intel MKL
#  MKL_INCLUDE_DIRS  - the MKL include directories
#  MKL_LIBRARIES     - link these to use MKL
#
# The following paths will be searched with priority if set in CMake or env
#
#  MKLROOT           - root directory of the MKL installation
#  MKL_PATH          - root directory of the MKL installation
#  MKL_ROOT          - root directory of the MKL installation

option( MKL_PARALLEL "if mkl shoudl be parallel" OFF )

if( MKL_PARALLEL )

  set( __mkl_lib_par  MKL_LIB_INTEL_THREAD )
  set( __mkl_lib_name mkl_intel_thread )

  find_package(Threads)

else()

  set( __mkl_lib_par MKL_LIB_SEQUENTIAL )
  set( __mkl_lib_name mkl_sequential )

endif()

# Search with priority for MKLROOT, MKL_PATH and MKL_ROOT if set in CMake or env
find_path(MKL_INCLUDE_DIR mkl.h
          PATHS ${MKLROOT} ${MKL_PATH} ${MKL_ROOT} $ENV{MKLROOT} $ENV{MKL_PATH} $ENV{MKL_ROOT}
          PATH_SUFFIXES include NO_DEFAULT_PATH)

find_path(MKL_INCLUDE_DIR mkl.h
          PATH_SUFFIXES include)

if( MKL_INCLUDE_DIR ) # use include dir to find libs

  set( MKL_INCLUDE_DIRS ${MKL_INCLUDE_DIR} )

  if( CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64" )
    set( __pathsuffix "lib/intel64")
    set( __libsfx _lp64 )
  else()
    set( __pathsuffix "lib/ia32")
    set( __libsfx "" )
  endif()

  find_library( MKL_LIB_INTEL
                PATHS ${MKLROOT} ${MKL_PATH} ${MKL_ROOT} $ENV{MKLROOT} $ENV{MKL_PATH} $ENV{MKL_ROOT}
                PATH_SUFFIXES lib ${__pathsuffix}
                NAMES mkl_intel${__libsfx} )

  find_library( ${__mkl_lib_par}
                PATHS ${MKLROOT} ${MKL_PATH} ${MKL_ROOT} $ENV{MKLROOT} $ENV{MKL_PATH} $ENV{MKL_ROOT}
                PATH_SUFFIXES lib ${__pathsuffix}
                NAMES ${__mkl_lib_name} )

  find_library( MKL_LIB_CORE
                PATHS ${MKLROOT} ${MKL_PATH} ${MKL_ROOT} $ENV{MKLROOT} $ENV{MKL_PATH} $ENV{MKL_ROOT}
                PATH_SUFFIXES lib ${__pathsuffix}
                NAMES mkl_core )

  if( MKL_PARALLEL )
    find_library( MKL_LIB_IOMP5
                  PATHS ${MKLROOT} ${MKL_PATH} ${MKL_ROOT} $ENV{MKLROOT} $ENV{MKL_PATH} $ENV{MKL_ROOT}
                  PATH_SUFFIXES lib ${__pathsuffix}
                  NAMES iomp5 )
  endif()

  if( MKL_LIB_INTEL AND ${__mkl_lib_par} AND MKL_LIB_CORE )
    set( MKL_LIBRARIES ${MKL_LIB_INTEL} ${${__mkl_lib_par}} ${MKL_LIB_CORE} ${MKL_LIB_IOMP5} ${CMAKE_THREAD_LIBS_INIT} )
  endif()

endif()

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args( MKL DEFAULT_MSG
                                   MKL_LIBRARIES MKL_INCLUDE_DIRS )

mark_as_advanced( MKL_INCLUDE_DIR MKL_LIB_LAPACK MKL_LIB_INTEL MKL_LIB_SEQUENTIAL MKL_LIB_CORE )
