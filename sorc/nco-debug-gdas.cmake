# Turn on/off checks and warning suppressions for GDAS build

# define the default set of flags
set(GDAS_Fortran_FLAGS "-g -ftrapuv -check all")
set(GDAS_CXX_FLAGS "-ftrapuv -check=uninit -debug all -g -traceback")

# Turn off Fortran temporary array warnings
set(GDAS_Fortran_FLAGS "${GDAS_Fortran_FLAGS},noarg_temp_created")

# Warning 6178
# We compile C++ and Fortran2003 together in many parts of GDAS/JEDI
# apparently Intel 19 (per Gemini) gets confused sometimes and throws
# warnings about the interoperability layer.
# Below line will suppress these warnings
set(GDAS_CXX_FLAGS "${GDAS_CXX_FLAGS} -wd6178")

# Warning 2651
# This is from calling Atlas which thus references Eckit
# not anything related to our code but rather the libraries we are linking to... 
# always comes up at calls related to a KDTree
set(GDAS_CXX_FLAGS "${GDAS_CXX_FLAGS} -wd2651")

set(CMAKE_Fortran_FLAGS "${GDAS_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS}" CACHE STRING "Fortran flags" FORCE)
set(CMAKE_CXX_FLAGS "${GDAS_CXX_FLAGS} ${CMAKE_CXX_FLAGS}" CACHE STRING "C++ flags" FORCE)
