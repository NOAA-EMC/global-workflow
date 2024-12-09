cmake_minimum_required(VERSION 3.10)

project(FunctionalTests)

enable_testing()

set(CMAKE_VERBOSE_MAKEFILE ON)

# Add a CTest to run the generate_workflows.sh script with additional arguments
set(CASE "C48_ATM")
set(RUNTESTS "${CMAKE_BINARY_DIR}/RUNTESTS" )
set(HOMEGFS ${CMAKE_CURRENT_SOURCE_DIR}/../../..)

message(STATUS "Executing: git rev-parse --short HEAD")
execute_process(
    COMMAND git rev-parse --short HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

message(STATUS "Executing: bash ${HOMEGFS}/workflow/generate_workflows.sh -V -H ${HOMEGFS} -y ${CASE}.yaml -t ${GIT_HASH} ${RUNTESTS}")
execute_process(
    COMMAND bash ${HOMEGFS}/workflow/generate_workflows.sh -V -H ${HOMEGFS} -y ${CASE}.yaml -t ${GIT_HASH} ${RUNTESTS}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

message(STATUS "Executing: ${HOMEGFS}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot_list ${RUNTESTS}")
execute_process(
    COMMAND ${HOMEGFS}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot_list ${RUNTESTS}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    OUTPUT_VARIABLE PSLOT_LIST
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

message(STATUS "Executing: ${HOMEGFS}/ci/scripts/utils/get_job_names.py --xmlfile ${RUNTESTS}/EXPDIR/${PSLOT_LIST}/${PSLOT_LIST}.xml")
execute_process(
    COMMAND ${HOMEGFS}/ci/scripts/utils/get_job_names.py --xmlfile ${RUNTESTS}/EXPDIR/${PSLOT_LIST}/${PSLOT_LIST}.xml
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    OUTPUT_FILE ${CMAKE_BINARY_DIR}/job_names.txt
)

file(READ ${CMAKE_BINARY_DIR}/job_names.txt JOB_NAMES)
message(STATUS "Job names:\n${JOB_NAMES}")

foreach(JOB IN LISTS JOB_NAMES)
    string(REGEX REPLACE "^CASE" "" JOB_MODIFIED ${JOB})
    message(STATUS "Executing: bash ${HOMEGFS}/workflow/generate_workflows.sh -V -H ${HOMEGFS} -y ${CASE}.yaml -t ${JOB_MODIFIED} ${RUNTESTS}")
    execute_process(
        COMMAND bash ${HOMEGFS}/workflow/generate_workflows.sh -V -H ${HOMEGFS} -y ${CASE}.yaml -t ${JOB_MODIFIED} ${RUNTESTS}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    )
endforeach()

