cmake_minimum_required(VERSION 3.10)

project(FunctionalTests)

enable_testing()

set(CMAKE_VERBOSE_MAKEFILE ON)

set(CASE_LIST "C48_ATM" "C48_S2SW")
set(RUNTESTS "${CMAKE_BINARY_DIR}/RUNTESTS" )
set(HOMEgfs ${CMAKE_CURRENT_SOURCE_DIR}/../../..)
set(CASE_PATH ${HOMEgfs}/ci/cases/pr)
file(REMOVE_RECURSE ${RUNTESTS})

set(CREATE_EXPERIMENT_CMD "${HOMEgfs}/workflow/create_experiment.py")

message(STATUS "Executing: git rev-parse --short HEAD")
execute_process(
    COMMAND git rev-parse --short HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

function(GenerateExperiments CASE)

    execute_process(
        COMMAND ${CREATE_EXPERIMENT_CMD} -yaml ${CASE_PATH}/${CASE}.yaml
        ENVIRONMENT "pslot=${CASE}_${GIT_HASH}" "RUNTESTS=${RUNTESTS}"
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    )
    
    message(STATUS "Executing: ${HOMEGFS}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot_list ${RUNTESTS}")
    execute_process(
        COMMAND ${HOMEGFS}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot_list ${RUNTESTS}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        OUTPUT_VARIABLE PSLOT_LIST
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    string(REGEX MATCHALL "${CASE}_[^;]+" PSLOT_LIST "${PSLOT_LIST}")
    message(STATUS "Filtered PSLOT list: ${PSLOT_LIST}\n\n")
    
    message(STATUS "Executing: ${HOMEGFS}/ci/scripts/utils/get_job_names.py --xml ${RUNTESTS}/EXPDIR/${PSLOT_LIST}/${PSLOT_LIST}.xml")
    execute_process(
        COMMAND ${HOMEgfs}/ci/scripts/utils/get_job_names.py --xml ${RUNTESTS}/EXPDIR/${PSLOT_LIST}/${PSLOT_LIST}.xml
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        OUTPUT_FILE ${CMAKE_BINARY_DIR}/${CASE}_jobs.txt
    )
    
    file(STRINGS ${CMAKE_BINARY_DIR}/${CASE}_jobs.txt JOB_NAMES)
    message(STATUS "Job names:\n${JOB_NAMES}")
    
    foreach(JOB IN LISTS JOB_NAMES)
        string(REGEX REPLACE "^${CASE}_" "" JOB_MODIFIED ${JOB})
        message(STATUS "Executing: ${CREATE_EXPERIMENT_CMD} -yaml ${CASE_PATH}/${CASE}.yaml") 
        execute_process(
            COMMAND ${CREATE_EXPERIMENT_CMD} -yaml ${CASE_PATH}/${CASE}.yaml
            ENVIRONMENT "pslot=${CASE}_${JOB_MODIFIED}_${GIT_HASH}" "RUNTESTS=${RUNTESTS}"
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        )
    endforeach()

endfunction()

foreach(CASE IN LISTS CASE_LIST)
    GenerateExperiments(${CASE})
endforeach()
