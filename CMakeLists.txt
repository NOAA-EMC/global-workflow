# ------------------------------------------------------------------------- #
# Global Workflow
# ------------------------------------------------------------------------- #

# Check for minimum cmake requirement
cmake_minimum_required( VERSION 3.20 FATAL_ERROR )

project(global_workflow VERSION 1.0.0)

include(GNUInstallDirs)
enable_testing()

# Build type.
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

# Build global-workflow source codes
# add_subdirectory(sorc)

# Setup tests
add_subdirectory(ctests)
