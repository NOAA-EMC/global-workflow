if (NOT DEFINED HOMEgfs)
  set(HOMEgfs ${PROJECT_SOURCE_DIR})
endif()

if (DEFINED ENV{RUNTESTS} AND NOT DEFINED RUNTESTS)
  set(RUNTESTS $ENV{RUNTESTS} CACHE FILEPATH "Path to global-workflow RUNTESTS directory")
else()
  set(RUNTESTS ${CMAKE_CURRENT_BINARY_DIR}/RUNTESTS)
endif()

if (DEFINED ENV{HPC_ACCOUNT} AND NOT DEFINED HPC_ACCOUNT)
  set(HPC_ACCOUNT $ENV{HPC_ACCOUNT} CACHE STRING "Local location of glowbal-workflow initialconditions")
endif()

if (NOT DEFINED HPC_ACCOUNT)
  message(FATAL_ERROR "HPC_ACCOUNT must be set")
  return()
endif()

if (DEFINED ENV{ISCDIR_ROOT} AND NOT DEFINED ICSDIR_ROOT)
  set(ICSDIR_ROOT $ENV{HPC_ACCOUNT} CACHE STRING "Local path to global-workflow initialconditions")
endif()
if(NOT DEFINED ICSDIR_ROOT)
  message(FATAL_ERROR "ICSDIR_ROOT must be set")
  return()
endif()

if (DEFINED ENV{STAGED_FUNCTIONAL_TEST_DIR} AND NOT DEFINED STAGED_FUNCTIONAL_TEST_DIR)
	set(STAGED_FUNCTIONAL_TEST_DIR $ENV{STAGED_FUNCTIONAL_TEST_DIR} CACHE STRING "Local path to global-workflow staged data for functional test")
endif()
if(NOT DEFINED STAGED_FUNCTIONAL_TEST_DIR)
  message(FATAL_ERROR "STATED_FUNCTIONAL_TEST_DIR must be set")
  return()
endif()

message(STATUS "MACHINE_ID: '${MACHINE_ID}'")
message(STATUS "gw: global-workflow baselines will be used from: '${HOMEgfs}'")
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
      COMMAND ./stage.sh ${TEST_NAME}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts)
  set_tests_properties(test_${TEST_NAME}_stage PROPERTIES DEPENDS test_${TEST_NAME}_setup LABELS "${ARG_CASE};${ARG_JOB}")
  
  add_test(NAME test_${TEST_NAME}_execute
      COMMAND ./execute.sh ${TEST_NAME} ${ARG_JOB} ${ARG_TEST_DATE}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts)
  set_tests_properties(test_${TEST_NAME}_execute PROPERTIES DEPENDS test_${TEST_NAME}_stage LABELS "${ARG_CASE};${ARG_JOB}")
  
  add_test(NAME test_${TEST_NAME}_validate
      COMMAND bash -c "${GW_BASELINE_DIR}/ci/ctests/scripts/validate.sh ${TEST_NAME}")
  set_tests_properties(test_${TEST_NAME}_validate PROPERTIES DEPENDS test_${TEST_NAME}_execute LABELS "${ARG_CASE};${ARG_JOB}")
endfunction()

AddJJOBTest(
  CASE "C48_ATM"
  JOB  "gfs_fcst_seg0"
  TEST_DATE "2021032312"
)
