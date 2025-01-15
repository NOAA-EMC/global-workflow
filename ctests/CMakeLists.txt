# ------------------------------------------------------------------------- #
# CTests for Global Workflow
# ------------------------------------------------------------------------- #
# These ctests correspond to JJOBs (individual Rocoto jobs) that can be
# run independently, each requiring its own YAML definition of inputs
# and configurations. By integrating with Rocoto, these jobs can be
# validated, staged, and executed as self-contained tests using
# their own data and test parameters.
# ------------------------------------------------------------------------- #

# Function to set a variable from an environment variable or default value
function(set_from_env_or_default VAR_NAME ENV_VAR DEFAULT_VALUE)
  if (DEFINED ENV{${ENV_VAR}} AND NOT DEFINED ${VAR_NAME})
    set(${VAR_NAME} $ENV{${ENV_VAR}} CACHE STRING "Set from environment variable ${ENV_VAR}")
  elseif(NOT DEFINED ${VAR_NAME} AND NOT ${DEFAULT_VALUE} STREQUAL "")
    set(${VAR_NAME} ${DEFAULT_VALUE} CACHE STRING "Default value for ${VAR_NAME}")
  endif()
endfunction()

# Set HOMEgfs
if (NOT DEFINED HOMEgfs)
  set(HOMEgfs ${PROJECT_SOURCE_DIR})
endif()

# Set RUNTESTS
set_from_env_or_default(RUNTESTS RUNTESTS "${CMAKE_CURRENT_BINARY_DIR}/RUNTESTS")

# Set HPC_ACCOUNT
set_from_env_or_default(HPC_ACCOUNT HPC_ACCOUNT " ")
if (NOT DEFINED HPC_ACCOUNT)
  message(WARNING "HPC_ACCOUNT must be set. CTests will not be created.")
  return()
endif()

# Set ICSDIR_ROOT
set_from_env_or_default(ICSDIR_ROOT ICSDIR_ROOT "")
if (NOT DEFINED ICSDIR_ROOT)
  message(WARNING "ICSDIR_ROOT must be set. CTests will not be created.")
  return()
endif()

# Set STAGED_TESTS_DIR
set_from_env_or_default(STAGED_TESTS_DIR STAGED_TESTS_DIR "")
if (NOT DEFINED STAGED_TESTS_DIR)
  message(WARNING "STAGED_TESTS_DIR must be set. CTests will not be created.")
  return()
endif()

message(STATUS "gw: global-workflow baselines will be used from: '${HOMEgfs}'")
message(STATUS "gw: global-workflow tests will be run at: '${RUNTESTS}'")
message(STATUS "gw: global-workflow tests will use the allocation: '${HPC_ACCOUNT}'")
message(STATUS "gw: global-workflow tests will use ICSDIR_ROOT: '${ICSDIR_ROOT}'")
message(STATUS "gw: global-workflow tests will use staged data from:  '${STAGED_TESTS_DIR}'")

# Prepare test scripts
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/scripts/setup.sh.in
               ${CMAKE_CURRENT_BINARY_DIR}/scripts/setup.sh @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/scripts/stage.sh.in
               ${CMAKE_CURRENT_BINARY_DIR}/scripts/stage.sh @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/scripts/execute.sh.in
               ${CMAKE_CURRENT_BINARY_DIR}/scripts/execute.sh @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/scripts/validate.sh.in
                ${CMAKE_CURRENT_BINARY_DIR}/scripts/validate.sh @ONLY)

function(AddJJOBTest)

  set(prefix     ARG)
  set(novals     NOTRAPFPE NOVALGRIND)
  set(singlevals CASE JOB TEST_DATE)
  set(multivals  TEST_DEPENDS)

  cmake_parse_arguments(${prefix}
                       "${novals}" "${singlevals}" "${multivals}"
                        ${ARGN})

  set(TEST_NAME ${ARG_CASE}_${ARG_JOB})
  set(CASE_PATH ${HOMEgfs}/ci/cases/pr)
  set(CASE_YAML ${CASE_PATH}/${ARG_CASE}.yaml)

  add_test(NAME test_${TEST_NAME}_setup
    COMMAND ./setup.sh ${TEST_NAME} ${CASE_YAML} ${ARG_TEST_DATE}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts)
  set_tests_properties(test_${TEST_NAME}_setup PROPERTIES LABELS "${ARG_CASE};${ARG_JOB}")
    
  add_test(NAME test_${TEST_NAME}_stage
      COMMAND ./stage.sh ${TEST_NAME} ${ARG_TEST_DATE}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts)
  set_tests_properties(test_${TEST_NAME}_stage PROPERTIES DEPENDS test_${TEST_NAME}_setup LABELS "${ARG_CASE};${ARG_JOB}")
  
  add_test(NAME test_${TEST_NAME}_execute
      COMMAND ./execute.sh ${TEST_NAME} ${ARG_JOB} ${ARG_TEST_DATE}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts)
  set_tests_properties(test_${TEST_NAME}_execute PROPERTIES DEPENDS test_${TEST_NAME}_stage LABELS "${ARG_CASE};${ARG_JOB}")
 
  # TODO - This is a stub for the validation step
  add_test(NAME test_${TEST_NAME}_validate
      COMMAND ./validate.sh ${TEST_NAME} ${CASE_YAML}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts)
  set_tests_properties(test_${TEST_NAME}_validate PROPERTIES DEPENDS test_${TEST_NAME}_execute LABELS "${ARG_CASE};${ARG_JOB}")
endfunction()

AddJJOBTest(
  CASE "C48_ATM"
  JOB  "gfs_fcst_seg0"
  TEST_DATE "2021032312"
)
