cmake_minimum_required(VERSION 3.15)

project(gsdcloud
        VERSION 1.0.0
        LANGUAGES Fortran)

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

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Intel)$")
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} is not supported.")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace -fconvert=big-endian")
endif()

if(NOT CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-DNDEBUG)
endif()

list(APPEND GSDCLOUD_SRC
ARPS_cldLib.f90
BackgroundCld.f90
BckgrndCC.f90
PrecipMxr_radar.f90
PrecipType.f90
TempAdjust.f90
adaslib.f90
build_missing_REFcone.f90
cloudCover_NESDIS.f90
cloudCover_Surface.f90
cloudCover_radar.f90
cloudLWC.f90
cloudLayers.f90
cloudType.f90
cloud_saturation.f90
constants.f90
convert_lghtn2ref.f90
get_sfm_1d_gnl.f90
hydro_mxr_thompson.f90
kinds.f90
map_ctp.f90
map_ctp_lar.f90
mthermo.f90
pbl_height.f90
pcp_mxr_ARPSlib.f90
radar_ref2tten.f90
read_Lightning_cld.f90
read_NESDIS.f90
read_Surface.f90
read_nasalarc_cld.f90
read_radar_ref.f90
smooth.f90
vinterp_radar_ref.f90
)

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/gsdcloud")
add_library(gsdcloud STATIC ${GSDCLOUD_SRC})
add_library(${PROJECT_NAME}::gsdcloud ALIAS gsdcloud)
set_target_properties(gsdcloud PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(gsdcloud PUBLIC $<BUILD_INTERFACE:${module_dir}>
                                           $<INSTALL_INTERFACE:include/gsdcloud>)

install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)

install(TARGETS gsdcloud
        EXPORT  ${PROJECT_NAME}Exports
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
