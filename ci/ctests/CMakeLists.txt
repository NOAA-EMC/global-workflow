cmake_minimum_required(VERSION 3.10)

project(FunctionalTests)

enable_testing()

set(CMAKE_VERBOSE_MAKEFILE ON)

set(ENV{RUNTESTS} "${CMAKE_BINARY_DIR}/RUNTESTS")
get_filename_component(HOMEgfs ${CMAKE_SOURCE_DIR}/../.. ABSOLUTE)
set(CASE_PATH ${HOMEgfs}/ci/cases/pr)
file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/RUNTESTS)

set(CREATE_EXPERIMENT_CMD "${HOMEgfs}/workflow/create_experiment.py --overwrite")

message(STATUS "Executing: git rev-parse --short HEAD")
execute_process(
    COMMAND git rev-parse --short HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

function(GenerateExperiments CASE)
    # Read the job list file for this case
    set(JOB_LIST_FILE "${CMAKE_BINARY_DIR}/${CASE}_jobs.txt")
    
    if(EXISTS ${JOB_LIST_FILE})
        file(STRINGS ${JOB_LIST_FILE} JOB_NAMES)
        message(STATUS "Processing jobs from ${JOB_LIST_FILE}:\n${JOB_NAMES}")
        
        foreach(JOB IN LISTS JOB_NAMES)
            string(REGEX REPLACE "^${CASE}_" "" JOB_MODIFIED ${JOB})
            message(STATUS "Executing: ${CREATE_EXPERIMENT_CMD} --yaml ${CASE_PATH}/${CASE}.yaml") 
            execute_process(
                COMMAND bash -c "export pslot=${CASE}_${JOB_MODIFIED}_${GIT_HASH}; ${CREATE_EXPERIMENT_CMD} --yaml ${CASE_PATH}/${CASE}.yaml"
            )
        endforeach()
    else()
        message(WARNING "Job list file ${JOB_LIST_FILE} does not exist.")
    endif()
endfunction()

function(AddRocotoBootTest CASE_NAME JOB_NAME)
    add_test(NAME jjob_test_${CASE_NAME}_${JOB_NAME}_StageData
             COMMAND bash -c "${HOMEgfs}/ci/ctests/scripts/stage_data.sh ${CASE_NAME} ${JOB_NAME} ${GIT_HASH}")
    add_test(NAME jjob_test_${CASE_NAME}_${JOB_NAME}
             COMMAND bash -c "rocotoboot -w RUNTESTS/EXPDIR/${CASE_NAME}_${GIT_HASH}/workflow.xml -d RUNTESTS/EXPDIR/${CASE_NAME}_${GIT_HASH}/workflow.db -t ${JOB_NAME}")
    set_tests_properties(jjob_test_${CASE_NAME}_${JOB_NAME} PROPERTIES DEPENDS jjob_test_${CASE_NAME}_${JOB_NAME}_StageData)         
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
