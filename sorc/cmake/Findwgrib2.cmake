# Find the wgrib2 headers, library and executable
#
# This module defines:
#
#   - wgrib2::wgrib2      - library and include directory, all in a single target.
#   - WGRIB2_INCLUDE_DIR  - include directory
#   - WGRIB2_LIBRARIES    - wgrib2 library
#   - WGRIB2API_LIBRARIES - wgrib2_api library (if using NCEPLibs-wgrib2 cmake build system)
#   - WGRIB2_EXE          - wgrib2 executable
#
# The following paths will be searched in order:
#
#   - WGRIB2_INCLUDE_DIRS - folders containing wgrib2.
#   - WGRIB2_LIBRARY_DIRS - folders containing libwgrib2.a
#   - wgrib2_ROOT         - root of wgrib2 installation
#   - wgrib2_PATH         - root of wgrib2 installation
#
# TODO
# Remove all instances of WGRIB2API_LIBRARIES when cmake build of wgrib2 is removed from all systems

find_path(
  WGRIB2_INCLUDE_DIR
  NAMES wgrib2.h wgrib2api.mod
  HINTS ${WGRIB2_INCLUDE_DIRS}
        ${wgrib2_ROOT} $ENV{wgrib2_ROOT}
        ${wgrib2_PATH} $ENV{wgrib2_PATH}
  PATH_SUFFIXES include include/wgrib2
  DOC "Path to wgrib2.h, wgrib2api.mod"
  )

find_library(
  WGRIB2_LIBRARIES
  NAMES libwgrib2.a
  HINTS ${WGRIB2_LIBRARY_DIRS}
        ${wgrib2_ROOT} $ENV{wgrib2_ROOT}
        ${wgrib2_PATH} $ENV{wgrib2_PATH}
  PATH_SUFFIXES lib lib64
  DOC "Path to libwgrib2.a"
  )

find_library(
  WGRIB2API_LIBRARIES
  NAMES libwgrib2_api.a
  HINTS ${WGRIB2_LIBRARY_DIRS}
        ${wgrib2_ROOT} $ENV{wgrib2_ROOT}
        ${wgrib2_PATH} $ENV{wgrib2_PATH}
  PATH_SUFFIXES lib lib64
  DOC "Path to libwgrib2_api.a"
  )

find_program(WGRIB2_EXE
  NAMES wgrib2
  HINTS ${wgrib2_ROOT} $ENV{wgrib2_ROOT}
        ${wgrib2_PATH} $ENV{wgrib2_PATH}
  PATH_SUFFIXES bin
  DOC "Path to wgrib2 executable"
  )

mark_as_advanced(WGRIB2_INCLUDE_DIR WGRIB2_LIBRARIES WGRIB2API_LIBRARIES WGRIB2_EXE)

message(DEBUG "[Findwgrib2.cmake]: creating target wgrib2::wgrib2")
add_library(wgrib2::wgrib2 UNKNOWN IMPORTED)
set_target_properties(wgrib2::wgrib2 PROPERTIES INTERFACE_INCLUDE_DIRECTORIES ${WGRIB2_INCLUDE_DIR})
if(WGRIB2API_LIBRARIES)
  message(DEBUG "[Findwgrib2.cmake]: linking with libwgrib2_api.a and libwgrib2.a")
  set_target_properties(wgrib2::wgrib2 PROPERTIES IMPORTED_LOCATION ${WGRIB2API_LIBRARIES})
  set_target_properties(wgrib2::wgrib2 PROPERTIES INTERFACE_LINK_LIBRARIES ${WGRIB2_LIBRARIES})
else()
  message(DEBUG "[Findwgrib2.cmake]: linking with libwgrib2.a")
  set_target_properties(wgrib2::wgrib2 PROPERTIES IMPORTED_LOCATION ${WGRIB2_LIBRARIES})
endif()

# wgrib2 changed how it outputs --version from "v0.x.y.z" to "vx.y.z" starting in wgrib2 3.0
execute_process(COMMAND ${WGRIB2_EXE} --version OUTPUT_VARIABLE version)
if(version MATCHES "^v0.*")
  string(SUBSTRING "${version}" 3 5 WGRIB2_VERSION)
else()
  string(SUBSTRING "${version}" 1 5 WGRIB2_VERSION)
endif()
unset(version)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(wgrib2
  REQUIRED_VARS WGRIB2_LIBRARIES WGRIB2_INCLUDE_DIR WGRIB2_EXE
  VERSION_VAR WGRIB2_VERSION
  )

if(wgrib2_FOUND AND NOT wgrib2_FIND_QUIETLY)
  message(STATUS "Findwgrib2:")
  message(STATUS "  - WGRIB2_INCLUDE_DIR: ${WGRIB2_INCLUDE_DIR}")
  message(STATUS "  - WGRIB2_LIBRARIES: ${WGRIB2_LIBRARIES}")
  if(WGRIB2API_LIBRARIES)
    message(STATUS "  - WGRIB2API_LIBRARIES: ${WGRIB2API_LIBRARIES}")
  endif()
  message(STATUS "  - WGRIB2_EXE: ${WGRIB2_EXE}")
  message(STATUS "  - WGRIB2_VERSION: ${WGRIB2_VERSION}")
endif()

