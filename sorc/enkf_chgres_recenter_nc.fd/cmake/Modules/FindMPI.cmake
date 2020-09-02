# This extends CMake's FindHDF5.cmake to add support to include MPI include
# paths and libraries in the HDF5 ones if HDF5_IS_PARALLEL is ON
# (BUG #0014363).

# include the default FindMPI.cmake.
if(CMAKE_VERSION VERSION_LESS 3.1)
  include(${CMAKE_ROOT}/Modules/FindMPI.cmake)
elseif(CMAKE_VERSION VERSION_LESS 3.6)
  message("Using new FindMPI")
  include(${CMAKE_CURRENT_LIST_DIR}/NewCMake/FindMPI.cmake)
# set(MPI_Fortran_INCLUDE_DIRS ${MPI_Fortran_INCLUDE_PATH} CACHE INTERNAL "Deprecated Variable Name")
else()
  message("Using installed FindMPI")
  include(${CMAKE_ROOT}/Modules/FindMPI.cmake)
# set(MPI_Fortran_INCLUDE_DIRS ${MPI_Fortran_INCLUDE_PATH} CACHE INTERNAL "Deprecated Variable Name")
  message("include dirs are ${MPI_Fortran_INCLUDE_DIRS}")
  message("include PATH  ${MPI_Fortran_INCLUDE_PATH}")
endif()
