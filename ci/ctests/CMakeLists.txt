cmake_minimum_required(VERSION 3.10)

project(FunctionalTests)
enable_testing()
set(CMAKE_VERBOSE_MAKEFILE ON)

# Set environment variables and paths
get_filename_component(HOMEgfs ${CMAKE_SOURCE_DIR}/../.. ABSOLUTE)
set(CASE_PATH ${HOMEgfs}/ci/cases/pr)
set(CREATE_EXPERIMENT_CMD "${HOMEgfs}/workflow/create_experiment.py --overwrite")

# Get the current git commit hash
message(STATUS "Executing: git rev-parse --short HEAD")
execute_process(
    COMMAND git rev-parse --short HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Function to add Rocoto boot test for a given case and job
function(AddFunctionalTest CASE JOB)
    add_test(NAME create_expirment_${JOB}
        COMMAND bash -c "export RUNTESTS=${CMAKE_BINARY_DIR}/RUNTESTS; export pslot=${CASE}_${JOB}_${GIT_HASH}; ${CREATE_EXPERIMENT_CMD} --yaml ${CASE_PATH}/${CASE}.yaml")
    set_tests_properties(create_experiment_${JOB} PROPERTIES LABELS "${CASE};${JOB}")
    
    add_test(NAME stage_${JOB}
        COMMAND bash -c "${HOMEgfs}/ci/ctests/scripts/stage_data.sh ${CASE_NAME} ${JOB_NAME} ${GIT_HASH}")
    set_tests_properties(stage_${JOB} PROPERTIES DEPENDS create_experiment_${JOB} LABELS "${CASE};${JOB}")
    
    add_test(NAME run_${JOB}
        #COMMAND bash -c "rocotoboot -w RUNTESTS/EXPDIR/${CASE_NAME}_${GIT_HASH}/workflow.xml -d RUNTESTS/EXPDIR/${CASE_NAME}_${GIT_HASH}/workflow.db -t ${JOB_NAME}")
        COMMAND bash -c "echo 'submitting running of job ${JOB_NAME}'")
    set_tests_properties(run_${JOB} PROPERTIES DEPENDS stage_${JOB} LABELS "${CASE};${JOB}")
    
    add_test(NAME validate_${JOB}
        COMMAND bash -c "${HOMEgfs}/ci/ctests/scripts/validate.sh ${CASE_NAME} ${JOB_NAME} ${GIT_HASH}")
    set_tests_properties(validate_${JOB} PROPERTIES DEPENDS run_${JOB} LABELS "${CASE};${JOB}")
endfunction()

# Split CASE_LIST into individual cases
string(REPLACE " " ";" CASE_LIST_SPLIT ${CASE_LIST})

# Loop over each case in CASE_LIST_SPLIT
foreach(CASE IN LISTS CASE_LIST_SPLIT)
    # Get JOB_NAMES for this CASE from input variable
    string(TOUPPER ${CASE} CASE_UPPER)
    set(JOB_NAMES_VAR "JOB_NAMES_${CASE_UPPER}")
    if(DEFINED ${JOB_NAMES_VAR})
        string(REPLACE " " ";" JOB_NAMES ${${JOB_NAMES_VAR}})
    else()
        message(WARNING "No JOB_NAMES provided for ${CASE}")
        continue()
    endif()
    foreach(JOB IN LISTS JOB_NAMES)
        AddFunctionalTest(${CASE} ${JOB})
    endforeach()
endforeach()