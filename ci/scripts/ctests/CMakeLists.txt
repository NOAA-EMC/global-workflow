cmake_minimum_required(VERSION 3.10)

project(FunctionalTests)

enable_testing()

# Add a CTest to run the generate_workflows.sh script with additional arguments
set(YAMLS "C48_ATM.yaml" CACHE STRING "YAML files separated by spaces")
set(RUNTESTS "${CMAKE_BINARY_DIR}/RUNTESTS" )
set(HOMEgfs ${CMAKE_CURRENT_SOURCE_DIR}/../../..)

# Get the Git hash
execute_process(
    COMMAND git rev-parse --short HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(NAME CreateExperiment
	COMMAND bash ${HOMEgfs}/workflow/generate_workflows.sh -H ${HOMEgfs} -y ${YAMLS} -t ${GIT_HASH} ${RUNTESTS}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

execute_process(NAME GetPslotList
    COMMAND ${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.py get_pslot_list ${RUNTESTS}
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    OUTPUT_VARIABLE PSLOT_LIST
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(NAME GetJobNames
    OUTPUT ${CMAKE_BINARY_DIR}/job_names.txt
    COMMAND ${HOMEgfs}/ci/scripts/utils/git_job_names.py --xmlfile ${RUNTESTS}/EXPDIR/${PSLOT_LIST}/${PSLOT_LIST}.xml
    OUTPUT_VARIABLE JOB_NAMES
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

# Output the job names
message(STATUS "Job names: ${JOB_NAMES}")