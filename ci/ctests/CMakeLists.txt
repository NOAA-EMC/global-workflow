cmake_minimum_required(VERSION 3.10)

project(FunctionalTests)

enable_testing()

set(CMAKE_VERBOSE_MAKEFILE ON)

set(CASE_LIST "C48_ATM" "C48_S2SW")
set(ENV{RUNTESTS} "${CMAKE_BINARY_DIR}/RUNTESTS")
set(HOMEgfs ${CMAKE_CURRENT_SOURCE_DIR}/../..)
set(CASE_PATH ${HOMEgfs}/ci/cases/pr)
file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/RUNTESTS)

set(CREATE_EXPERIMENT_CMD "${HOMEgfs}/workflow/create_experiment.py")

message(STATUS "Executing: git rev-parse --short HEAD")
execute_process(
    COMMAND git rev-parse --short HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND  ln -s ${HOMEgfs} global-workflow
)

function(GenerateExperiments CASE)

    execute_process(
        COMMAND bash -c "export pslot=${CASE}_${GIT_HASH}; ${CREATE_EXPERIMENT_CMD} --overwrite --yaml ${CASE_PATH}/${CASE}.yaml"
    )
    
    message(STATUS "Executing: ${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot_list RUNTESTS")
    execute_process(
        COMMAND ${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot_list RUNTESTS
        OUTPUT_VARIABLE PSLOT_LIST
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    string(REGEX MATCHALL "${CASE}_[^;]+" PSLOT_LIST "${PSLOT_LIST}")
    message(STATUS "Filtered PSLOT list: ${PSLOT_LIST}\n\n")
    
    message(STATUS "Executing: ${HOMEgfs}/ci/ctests/scripts/get_job_names.py --xml RUNTESTS/EXPDIR/${PSLOT_LIST}/${PSLOT_LIST}.xml")
    execute_process(
        COMMAND ${HOMEgfs}/ci/ctests/scripts/get_job_names.py --xml RUNTESTS/EXPDIR/${PSLOT_LIST}/${PSLOT_LIST}.xml
        OUTPUT_FILE ${CMAKE_BINARY_DIR}/${CASE}_jobs.txt
    )
    
    file(STRINGS ${CMAKE_BINARY_DIR}/${CASE}_jobs.txt JOB_NAMES)
    message(STATUS "Job names:\n${JOB_NAMES}")
    
    foreach(JOB IN LISTS JOB_NAMES)
        string(REGEX REPLACE "^${CASE}_" "" JOB_MODIFIED ${JOB})
        message(STATUS "Executing: ${CREATE_EXPERIMENT_CMD} --yaml ${CASE_PATH}/${CASE}.yaml") 
        execute_process(
            COMMAND bash -c "export pslot=${CASE}_${JOB_MODIFIED}_${GIT_HASH}; ${CREATE_EXPERIMENT_CMD} --yaml ${CASE_PATH}/${CASE}.yaml"
        )
    endforeach()

endfunction()

function(AddRocotoBootTest CASE_NAME JOB_NAME)
    add_test(NAME jjob_test_${CASE_NAME}_${JOB_NAME}
             COMMAND bash -c "rocotoboot -w RUNTESTS/EXPDIR/${CASE_NAME}_${GIT_HASH}/workflow.xml -d RUNTESTS/EXPDIR/${CASE_NAME}_${GIT_HASH}/workflow.db -t ${JOB_NAME}")
endfunction()

add_test( NAME BuildAll
          COMMAND bash -c "cd ${HOMEgfs}/sorc; ./build_all.sh"
)

foreach(CASE IN LISTS CASE_LIST)
    GenerateExperiments(${CASE})
    file(STRINGS ${CMAKE_BINARY_DIR}/${CASE}_jobs.txt JOB_NAMES)
    foreach(JOB IN LISTS JOB_NAMES)
        AddRocotoBootTest(${CASE} ${JOB})
    endforeach()
endforeach()
