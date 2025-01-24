cmake_minimum_required(VERSION 3.19)

project(gsi
        VERSION 1.0.0
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(CMAKE_DIRECTORY_LABELS ${PROJECT_NAME})

include(GNUInstallDirs)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(GNU|Intel|Clang|AppleClang)$")
  message(WARNING "${CMAKE_C_COMPILER_ID} is not supported.")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Intel)$")
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} is not supported.")
endif()

# User options
option(OPENMP "Enable OpenMP Threading" OFF)
option(ENABLE_MKL "Use MKL for LAPACK implementation (if available)" ON)
option(USE_GSDCLOUD "Use GSD Cloud Analysis library" OFF)
option(USE_MGBF "Use MGBF library" ON)

set(GSI_VALID_MODES "GFS" "Regional")
set(GSI_MODE "GFS" CACHE STRING "Choose the GSI Application.")
set_property(CACHE GSI_MODE PROPERTY STRINGS ${GSI_VALID_MODES})

# Ensure valid GSI_MODE is selected
if(NOT GSI_MODE IN_LIST GSI_VALID_MODES)
  message(FATAL_ERROR "GSI_MODE must be one of ${GSI_VALID_MODES}")
endif()

# Echo user options
message(STATUS "GSI: OPENMP ................. ${OPENMP}")
message(STATUS "GSI: ENABLE_MKL ............. ${ENABLE_MKL}")
message(STATUS "GSI: USE_GSDCLOUD ........... ${USE_GSDCLOUD}")
message(STATUS "GSI: USE_MGBF ............... ${USE_MGBF}")
message(STATUS "GSI: GSI_MODE ............... ${GSI_MODE}")

# Dependencies
if(ENABLE_MKL)
  find_package(MKL)
endif()
if(MKL_FOUND)
  set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
else()
  set(ENABLE_MKL OFF CACHE INTERNAL "GSI: Disable MKL since it was NOT FOUND")
  find_package(LAPACK REQUIRED)
endif()
find_package(MPI REQUIRED)
find_package(NetCDF REQUIRED Fortran)
if(OPENMP)
  find_package(OpenMP REQUIRED)
endif()

# NCEPLibs dependencies
find_package(bacio REQUIRED)
find_package(sigio REQUIRED)
find_package(sfcio REQUIRED)
find_package(nemsio REQUIRED)
find_package(ncio REQUIRED)
find_package(ncdiag REQUIRED)
find_package(sp REQUIRED)
find_package(ip REQUIRED)
find_package(w3emc REQUIRED)
find_package(bufr REQUIRED)
find_package(crtm REQUIRED)
if(GSI_MODE MATCHES "Regional")
  find_package(wrf_io REQUIRED)
endif()

# See https://github.com/NOAA-EMC/NCEPLIBS-nemsio/pull/22
target_link_libraries(nemsio::nemsio INTERFACE w3emc::w3emc_d bacio::bacio_4)

# GSD Cloud Analysis library dependency
if(USE_GSDCLOUD)
  if(NOT TARGET gsdcloud)
    find_package(gsdcloud REQUIRED)
  endif()
endif()

# MGBF library dependency
if(USE_MGBF)
  if(NOT TARGET mgbf)
    find_package(mgbf REQUIRED)
  endif()
endif()

# Get compiler flags for the GSI application
include(gsiapp_compiler_flags)

# Get the list of all source files
include(gsi_files.cmake)

# Collect common files for GSI Fortran library
list(APPEND GSI_SRC_Fortran
  ${GSI_SRC_srcs}
  ${GSI_SRC_class})

# Collect files for specific GSI Application
if(GSI_MODE MATCHES "GFS") # GFS GSI application
  list(APPEND GSI_SRC_Fortran
    ${GSI_SRC_gfs_stub}
    ${GSI_SRC_fixture_gfs})
elseif(GSI_MODE MATCHES "Regional") # Regional GSI application
  list(APPEND GSI_SRC_Fortran
    ${GSI_SRC_regional_cplr}
    ${GSI_SRC_fixture_regional})
endif()

# Compiler options and definitions
list(APPEND GSI_C_defs
  FortranByte=char
  FortranInt=int
  funder
  "FortranLlong=long long")

list(APPEND GSI_Fortran_defs _REAL8_)
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  list(APPEND GSI_Fortran_defs POUND_FOR_STRINGIFY)
endif()
if(USE_GSDCLOUD)
  list(APPEND GSI_Fortran_defs RR_CLOUDANALYSIS)
endif()

# Create a library of GSI C sources
add_library(gsi_c_obj OBJECT ${GSI_SRC_C})
target_compile_definitions(gsi_c_obj PRIVATE ${GSI_C_defs})

# Create a library of GSI Fortran sources
set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/gsi")
add_library(gsi_fortran_obj OBJECT ${GSI_SRC_Fortran})
set_target_properties(gsi_fortran_obj PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(gsi_fortran_obj INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                                                     $<BUILD_INTERFACE:${module_dir}>
                                                     $<INSTALL_INTERFACE:include/gsi>)
target_compile_definitions(gsi_fortran_obj PRIVATE ${GSI_Fortran_defs})
target_link_libraries(gsi_fortran_obj PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(gsi_fortran_obj PUBLIC MPI::MPI_Fortran)
target_link_libraries(gsi_fortran_obj PUBLIC ${LAPACK_LIBRARIES})
target_link_libraries(gsi_fortran_obj PUBLIC bacio::bacio_4)
target_link_libraries(gsi_fortran_obj PUBLIC sigio::sigio)
target_link_libraries(gsi_fortran_obj PUBLIC sfcio::sfcio)
target_link_libraries(gsi_fortran_obj PUBLIC nemsio::nemsio)
target_link_libraries(gsi_fortran_obj PUBLIC ncio::ncio)
target_link_libraries(gsi_fortran_obj PUBLIC w3emc::w3emc_d)
target_link_libraries(gsi_fortran_obj PUBLIC sp::sp_d)
if(DEFINED ENV{USE_BUFR4})
  target_link_libraries(gsi_fortran_obj PUBLIC bufr::bufr_4)
else()
  target_link_libraries(gsi_fortran_obj PUBLIC bufr::bufr_d)
endif()
target_link_libraries(gsi_fortran_obj PUBLIC crtm::crtm)
if(GSI_MODE MATCHES "Regional")
  target_link_libraries(gsi_fortran_obj PUBLIC wrf_io::wrf_io)
endif()
target_link_libraries(gsi_fortran_obj PUBLIC ncdiag::ncdiag)
if(USE_GSDCLOUD)
  if(TARGET gsdcloud)
    add_dependencies(gsi_fortran_obj gsdcloud)
  endif()
  target_link_libraries(gsi_fortran_obj PUBLIC gsdcloud::gsdcloud)
endif()
if(USE_MGBF)
  if(TARGET mgbf)
    add_dependencies(gsi_fortran_obj mgbf)
  endif()
  target_link_libraries(gsi_fortran_obj PUBLIC mgbf::mgbf)
endif()
if(OpenMP_Fortran_FOUND)
  target_link_libraries(gsi_fortran_obj PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Create the GSI library
add_library(gsi STATIC)
add_library(${PROJECT_NAME}::gsi ALIAS gsi)
set_target_properties(gsi PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(gsi PUBLIC $<BUILD_INTERFACE:${module_dir}>
                                      $<INSTALL_INTERFACE:include/gsi>)
target_link_libraries(gsi PUBLIC gsi_c_obj)
target_link_libraries(gsi PUBLIC gsi_fortran_obj)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(gsi PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Create the GSI executable
add_executable(gsi.x ${GSI_SRC_main})
add_dependencies(gsi.x gsi)
set_target_properties(gsi.x PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_link_libraries(gsi.x PRIVATE gsi)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(gsi.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install Fortran modules
install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)

# Install executable targets
install(TARGETS gsi.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# Install and export library targets
install(
  TARGETS gsi_c_obj gsi_fortran_obj gsi
  EXPORT ${PROJECT_NAME}Exports
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
       NAMESPACE ${PROJECT_NAME}::
       FILE ${PROJECT_NAME}-targets.cmake)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})

write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})

install(EXPORT ${PROJECT_NAME}Exports
        NAMESPACE ${PROJECT_NAME}::
        FILE ${PROJECT_NAME}-targets.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})
