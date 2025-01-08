cmake_minimum_required(VERSION 3.10)

project(FunctionalTests)
enable_testing()
set(CMAKE_VERBOSE_MAKEFILE ON)

function(set_from_env_or_default VAR_NAME ENV_VAR DEFAULT_VALUE)
  if (DEFINED ENV{${ENV_VAR}} AND NOT DEFINED ${VAR_NAME})
    set(${VAR_NAME} $ENV{${ENV_VAR}} CACHE STRING "Set from environment variable ${ENV_VAR}")
  elseif(NOT DEFINED ${VAR_NAME})
    set(${VAR_NAME} ${DEFAULT_VALUE} CACHE STRING "Default value for ${VAR_NAME}")
  endif()
endfunction()

if (DEFINED ENV{GW_BASELINE_DIR} AND NOT DEFINED GW_BASELINE_DIR)
  set(GW_BASELINE_DIR $ENV{GW_BASELINE_DIR} CACHE FILEPATH "Path to global-workflow baselines")
else()
  get_filename_component(GW_BASELINE_DIR ${CMAKE_SOURCE_DIR}/../.. ABSOLUTE)
endif()

# If GW_BASELINE_DIR is not defined, tests will not be run, return
if(NOT DEFINED GW_BASELINE_DIR)
  message(WARNING "gw: GW_BASELINE_DIR is not defined. Tests will not be run.")
  return()
endif()

set_from_env_or_default(RUNTESTS RUNTESTS "${CMAKE_BINARY_DIR}/RUNTESTS")

set_from_env_or_default(MACHINE_ID MACHINE_ID "NOT_DEFINED")
if(${MACHINE_ID} STREQUAL "NOT_DEFINED")
  execute_process(
    COMMAND bash -c "source ${GW_BASELINE_DIR}/ush/detect_machine.sh && echo \$MACHINE_ID"
    OUTPUT_VARIABLE MACHINE_ID
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
endif()

set_from_env_or_default(HPC_ACCOUNT HPC_ACCOUNT "NOT_DEFINED")
if(${HPC_ACCOUNT} STREQUAL "NOT_DEFINED")
  execute_process(
    COMMAND bash -c "source ${GW_BASELINE_DIR}/ci/platforms/config.${MACHINE_ID} && echo \$HPC_ACCOUNT"
    OUTPUT_VARIABLE HPC_ACCOUNT
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
endif()

set_from_env_or_default(ICSDIR_ROOT ICSDIR_ROOT "NOT_DEFINED")
if(${ICSDIR_ROOT} STREQUAL "NOT_DEFINED")
  execute_process(
    COMMAND bash -c "source ${GW_BASELINE_DIR}/ci/platforms/config.${MACHINE_ID} && echo \$ICSDIR_ROOT"
    OUTPUT_VARIABLE ICSDIR_ROOT
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  set(ICSDIR_ROOT ${ICSDIR_ROOT} CACHE FILEPATH "Path to ICS directory")
endif()

message(STATUS "MACHINE_ID: '${MACHINE_ID}'")
message(STATUS "gw: global-workflow baselines will be used from: '${GW_BASELINE_DIR}'")
message(STATUS "gw: global-workflow tests will be run at: '${RUNTESTS}'")
message(STATUS "gw: global-workflow tests will use the allocation: '${HPC_ACCOUNT}'")
message(STATUS "gw: global-workflow tests will use ICSDIR_ROOT: '${ICSDIR_ROOT}'")

# Prepare test scripts
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/scripts/setup.sh.in
               ${CMAKE_CURRENT_BINARY_DIR}/scripts/setup.sh @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/scripts/stage.sh.in
               ${CMAKE_CURRENT_BINARY_DIR}/scripts/stage.sh @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/scripts/execute.sh.in
               ${CMAKE_CURRENT_BINARY_DIR}/scripts/execute.sh @ONLY)

function(AddFunctionalTest)

  set(prefix     ARG)
  set(novals     NOTRAPFPE NOVALGRIND)
  set(singlevals CASE JOB)
  set(multivals  TEST_DEPENDS)

  cmake_parse_arguments(${prefix}
                       "${novals}" "${singlevals}" "${multivals}"
                        ${ARGN})

  set(TEST_NAME ${ARG_CASE}_${ARG_JOB})
  set(CASE_PATH ${GW_BASELINE_DIR}/ci/cases/pr)
  set(CASE_YAML ${CASE_PATH}/${ARG_CASE}.yaml)

  add_test(NAME test_${TEST_NAME}_setup
    COMMAND ./setup.sh ${TEST_NAME} ${CASE_YAML}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts)
  set_tests_properties(test_${TEST_NAME}_setup PROPERTIES LABELS "${ARG_CASE};${ARG_JOB}")
    
  add_test(NAME test_${TEST_NAME}_stage
      COMMAND ./stage.sh ${TEST_NAME}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts)
  set_tests_properties(test_${TEST_NAME}_stage PROPERTIES DEPENDS test_${TEST_NAME}_setup LABELS "${ARG_CASE};${ARG_JOB}")
  
  add_test(NAME test_${TEST_NAME}_execute
      #TODO Need to get the idate for rocotoboot get the right cycle for the test (hard coded for now)
      #TODO Need to add a loop to check for when job has been completed
      COMMAND ./execute.sh ${TEST_NAME} ${ARG_JOB} 2021032312
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts)
  set_tests_properties(test_${TEST_NAME}_execute PROPERTIES DEPENDS test_${TEST_NAME}_stage LABELS "${ARG_CASE};${ARG_JOB}")
  
  add_test(NAME test_${TEST_NAME}_validate
      COMMAND bash -c "${GW_BASELINE_DIR}/ci/ctests/scripts/validate.sh ${TEST_NAME}")
  set_tests_properties(test_${TEST_NAME}_validate PROPERTIES DEPENDS test_${TEST_NAME}_execute LABELS "${ARG_CASE};${ARG_JOB}")
endfunction()

AddFunctionalTest(
  CASE "C48_ATM"
  JOB  "gfs_fcst_seg0"
)
