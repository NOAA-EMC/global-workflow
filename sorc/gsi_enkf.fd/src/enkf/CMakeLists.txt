cmake_minimum_required(VERSION 3.19)

project(enkf
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

set(ENKF_VALID_MODES "GFS" "WRF" "NMMB" "FV3REG")
set(ENKF_MODE "GFS" CACHE STRING "Choose the EnKF Application.")
set_property(CACHE ENKF_MODE PROPERTY STRINGS ${ENKF_VALID_MODES})

# Ensure valid ENKF_MODE is selected
if(NOT ENKF_MODE IN_LIST ENKF_VALID_MODES)
  message(FATAL_ERROR "ENKF_MODE must be one of ${ENKF_VALID_MODES}")
endif()

# Echo user options
message(STATUS "EnKF: OPENMP ................. ${OPENMP}")
message(STATUS "EnKF: ENABLE_MKL ............. ${ENABLE_MKL}")
message(STATUS "EnKF: ENKF_MODE .............. ${ENKF_MODE}")

# Dependencies
if(ENABLE_MKL)
  find_package(MKL QUIET)
endif()
if(MKL_FOUND)
  set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
else()
  set(ENABLE_MKL OFF CACHE INTERNAL "EnKF: Disable MKL since it was NOT FOUND")
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
find_package(sp REQUIRED)
find_package(w3emc REQUIRED)
if(ENKF_MODE MATCHES "^(WRF|NMMB|FV3REG)$")
  find_package(wrf_io REQUIRED)
endif()

# See https://github.com/NOAA-EMC/NCEPLIBS-nemsio/pull/22
target_link_libraries(nemsio::nemsio INTERFACE w3emc::w3emc_d bacio::bacio_4)

if(NOT TARGET gsi)
  find_package(gsi REQUIRED)
endif()

# Get compiler flags for the GSI application
include(enkfapp_compiler_flags)

# Get the list of all source files
include(enkf_files.cmake)

# Collect common files for EnKF Fortran library
list(APPEND EnKF_SRC_Fortran
  ${EnKF_SRC_srcs})

# Collect files for specific EnKF Application
list(APPEND EnKF_SRC_Fortran
  ${EnKF_SRC_${ENKF_MODE}})

# Create a library of EnKF Fortran sources
set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/enkf")
add_library(enkf_fortran_obj OBJECT ${EnKF_SRC_Fortran})
set_target_properties(enkf_fortran_obj PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(enkf_fortran_obj INTERFACE $<BUILD_INTERFACE:${module_dir}>
                                                      $<INSTALL_INTERFACE:include/enkf>)
target_compile_definitions(enkf_fortran_obj PRIVATE "_REAL8_")
if(TARGET gsi)
  add_dependencies(enkf_fortran_obj gsi)
endif()
target_link_libraries(enkf_fortran_obj PUBLIC gsi::gsi)
target_link_libraries(enkf_fortran_obj PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(enkf_fortran_obj PUBLIC MPI::MPI_Fortran)
target_link_libraries(enkf_fortran_obj PUBLIC ${LAPACK_LIBRARIES})
target_link_libraries(enkf_fortran_obj PUBLIC bacio::bacio_4)
target_link_libraries(enkf_fortran_obj PUBLIC sigio::sigio)
target_link_libraries(enkf_fortran_obj PUBLIC sfcio::sfcio)
target_link_libraries(enkf_fortran_obj PUBLIC nemsio::nemsio)
target_link_libraries(enkf_fortran_obj PUBLIC ncio::ncio)
target_link_libraries(enkf_fortran_obj PUBLIC w3emc::w3emc_d)
target_link_libraries(enkf_fortran_obj PUBLIC sp::sp_d)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(enkf_fortran_obj PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Create the EnKF library
add_library(enkf STATIC)
add_library(${PROJECT_NAME}::enkf ALIAS enkf)
set_target_properties(enkf PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(enkf PUBLIC $<BUILD_INTERFACE:${module_dir}>
                                       $<INSTALL_INTERFACE:include/enkf>)
target_link_libraries(enkf PUBLIC enkf_fortran_obj)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(enkf PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Create the EnKF executable
add_executable(enkf.x ${EnKF_SRC_main})
add_dependencies(enkf.x enkf)
target_link_libraries(enkf.x PRIVATE enkf)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(enkf.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install Fortran modules
install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)

# Install executable targets
install(TARGETS enkf.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# Install and export library targets
install(
  TARGETS enkf_fortran_obj enkf
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
