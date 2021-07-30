cmake_minimum_required(VERSION 3.15)

# Get the version from the VERSION file.
#file(STRINGS "VERSION" pVersion)
set(pVersion 1.0.0)

project(
  workflow_utils
  VERSION ${pVersion}
  LANGUAGES Fortran)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(GNUInstallDirs)

# User options.
option(OPENMP "use OpenMP threading" ON)

# Build type
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -check -check noarg_temp_created -check nopointer -fp-stack-check -fstack-protector-all -fpe0 -debug -ftrapuv")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -ggdb -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -ffpe-trap=invalid,zero,overflow -fbounds-check")
endif()

# Find packages.
find_package(MPI REQUIRED)
find_package(NetCDF REQUIRED Fortran)

if(OPENMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
endif()

find_package(bacio REQUIRED)
find_package(w3nco REQUIRED)
find_package(w3emc REQUIRED)
find_package(sp REQUIRED)
find_package(ip REQUIRED)
find_package(ncio REQUIRED)
find_package(nemsio REQUIRED)
find_package(sigio REQUIRED)
find_package(g2 REQUIRED)
find_package(bufr REQUIRED)
find_package(landsfcutil REQUIRED)
find_package(wgrib2 REQUIRED)

add_subdirectory(enkf_chgres_recenter.fd)
add_subdirectory(enkf_chgres_recenter_nc.fd)
add_subdirectory(fv3nc2nemsio.fd)
add_subdirectory(regrid_nemsio.fd)
add_subdirectory(gaussian_sfcanl.fd)
add_subdirectory(gfs_bufr.fd)
add_subdirectory(reg2grb2.fd)
add_subdirectory(tocsbufr.fd)
add_subdirectory(fbwndgfs.fd)
add_subdirectory(vint.fd)
add_subdirectory(tave.fd)
add_subdirectory(syndat_qctropcy.fd)
add_subdirectory(syndat_maksynrc.fd)
add_subdirectory(syndat_getjtbul.fd)
add_subdirectory(supvit.fd)
