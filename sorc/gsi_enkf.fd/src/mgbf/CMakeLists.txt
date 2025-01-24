cmake_minimum_required(VERSION 3.15)

project(mgbf
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

list(APPEND MGBF_SRC
kinds.f90
jp_pkind.f90
jp_pkind2.f90
jp_pietc.f90
jp_pietc_s.f90
jp_pmat.f90
jp_pmat4.f90
jp_pbfil.f90
jp_pbfil2.f90
jp_pbfil3.f90
mg_mppstuff.f90
mg_domain.f90
mg_domain_loc.f90
mg_parameter.f90
mg_bocos.f90
mg_transfer.f90
mg_generations.f90
mg_interpolate.f90
mg_filtering.f90
mg_timers.f90
mg_entrymod.f90
mg_intstate.f90
mg_input.f90
)

set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/mgbf")
add_library(mgbf STATIC ${MGBF_SRC})
add_library(${PROJECT_NAME}::mgbf ALIAS mgbf)
set_target_properties(mgbf PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
target_include_directories(mgbf PUBLIC $<BUILD_INTERFACE:${module_dir}>
                                           $<INSTALL_INTERFACE:include/mgbf>)

install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)

install(TARGETS mgbf
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
