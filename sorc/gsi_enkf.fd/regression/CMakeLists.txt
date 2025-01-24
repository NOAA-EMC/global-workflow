# Find the GSI and EnKF control executables
find_program(GSICONTROLEXEC
  NAMES gsi.x
  PATHS ${CONTROLPATH}    ENV CONTROLPATH
        ${GSICONTROLPATH} ENV GSICONTROLPATH
  PATH_SUFFIXES bin exec
  DOC   "Searching for gsi.x"
  NO_DEFAULT_PATH
)

if (GSICONTROLEXEC)
  message(STATUS "RT: Control GSIexec Found: ${GSICONTROLEXEC}")
else()
  message(WARNING "RT: Control GSIexec Not Found, GSI Regression Tests Disabled!")
endif()

find_program(ENKFCONTROLEXEC
  NAMES enkf.x enkf_gfs.x
  PATHS ${CONTROLPATH}     ENV CONTROLPATH
        ${ENKFCONTROLPATH} ENV ENKFCONTROLPATH
  PATH_SUFFIXES bin exec
  DOC   "Searching for enkf.x"
  NO_DEFAULT_PATH
)

if (ENKFCONTROLEXEC)
  message(STATUS "RT: Control EnKFexec Found: ${ENKFCONTROLEXEC}")
else()
  message(WARNING "RT: Control EnKFexec Not Found, EnKF Regression Tests Disabled!")
endif()

# If neither are found, nothing to do; simply return
if(NOT (GSICONTROLEXEC AND ENKFCONTROLEXEC))
  message(WARNING "RT: Unable to find Control Executables.  Regression Tests Disabled!")
  set(BUILD_REG_TESTING OFF CACHE BOOL "Regression testing disabled" FORCE)
  return()
endif()

# GSI regression test names
list(APPEND GSI_REG_TEST_NAMES
  global_4denvar
  rtma
  rrfs_3denvar_rdasens
  hafs_4denvar_glbens hafs_3denvar_hybens
)

# EnKF regression test names
if(ENKF_MODE MATCHES "^(FV3REG)$")
  list(APPEND ENKF_REG_TEST_NAMES
    rrfs_enkf_conv
  )
else()
  list(APPEND ENKF_REG_TEST_NAMES
    global_enkf
  )
endif()

# Add GSI regression tests to list of tests
if(GSICONTROLEXEC)
  list(APPEND REG_TEST_NAMES ${GSI_REG_TEST_NAMES})
endif()

# Add EnKF regression tests to list of tests
if(ENKFCONTROLEXEC)
  list(APPEND REG_TEST_NAMES ${ENKF_REG_TEST_NAMES})
endif()

# Names of the GSI and EnKF executables from this build
set(GSIUPDATEEXEC ${PROJECT_BINARY_DIR}/src/gsi/gsi.x)
set(ENKFUPDATEEXEC ${PROJECT_BINARY_DIR}/src/enkf/enkf.x)

# Create Regression tests if list is not empty
if(REG_TEST_NAMES)
  # Create regression_var.out file
  file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/regression_var.out" "${CMAKE_CURRENT_SOURCE_DIR}/regression_var.sh ${PROJECT_SOURCE_DIR}/.. ${PROJECT_BINARY_DIR} ${PROJECT_SOURCE_DIR} ${GSIUPDATEEXEC} ${ENKFUPDATEEXEC} ${GSICONTROLEXEC} ${ENKFCONTROLEXEC}")

  # Run each regression test; one at a time
  foreach(REG_TEST ${REG_TEST_NAMES})
    add_test(NAME ${REG_TEST}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      COMMAND regression_driver.sh ${REG_TEST} ${CMAKE_CURRENT_BINARY_DIR})
    set_tests_properties(${REG_TEST} PROPERTIES TIMEOUT 86400)
  endforeach(REG_TEST)
endif()
