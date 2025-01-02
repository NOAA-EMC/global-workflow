cmake_minimum_required(VERSION 3.10)

project(FunctionalTests)
enable_testing()
set(CMAKE_VERBOSE_MAKEFILE ON)

# Set environment variables and paths
get_filename_component(HOMEgfs ${CMAKE_SOURCE_DIR}/../.. ABSOLUTE)
set(CASE_PATH ${HOMEgfs}/ci/cases/pr)
set(CREATE_EXPERIMENT_CMD "${HOMEgfs}/workflow/create_experiment.py --overwrite")

# Get the current git commit hash
#message(STATUS "Executing: git rev-parse --short HEAD")
#execute_process(
#    COMMAND git rev-parse --short HEAD
#    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
#    OUTPUT_VARIABLE GIT_HASH
#    OUTPUT_STRIP_TRAILING_WHITESPACE
#)

# Function to add Rocoto boot test for a given case and job
function(AddFunctionalTest CASE JOB)
    set(TEST_NAME ${CASE}_${JOB})
    add_test(NAME ${TEST_NAME}_create_experiment
        COMMAND bash -c "export RUNTESTS=${CMAKE_BINARY_DIR}/RUNTESTS; export pslot=${TEST_NAME}; ${CREATE_EXPERIMENT_CMD} --yaml ${CASE_PATH}/${CASE}.yaml")
    set_tests_properties(${TEST_NAME}_create_experiment PROPERTIES LABELS "${CASE};${JOB}")
    
    add_test(NAME ${TEST_NAME}_stage
        COMMAND bash -c "${HOMEgfs}/ci/ctests/scripts/stage_data.py --build_dir ${CMAKE_BINARY_DIR} --case ${CASE} --job ${JOB}")
    set_tests_properties(${TEST_NAME}_stage PROPERTIES DEPENDS ${TEST_NAME}_create_experiment LABELS "${CASE};${JOB}")
    
    add_test(NAME ${TEST_NAME}_execute
        #COMMAND bash -c "rocotoboot -w RUNTESTS/EXPDIR/${CASE_NAME}_${GIT_HASH}/workflow.xml -d RUNTESTS/EXPDIR/${CASE_NAME}_${GIT_HASH}/workflow.db -t ${JOB_NAME}")
        COMMAND bash -c "echo 'submitting running of job ${JOB_NAME}'")
    set_tests_properties(${TEST_NAME}_execute PROPERTIES DEPENDS ${TEST_NAME}_stage LABELS "${CASE};${JOB}")
    
    add_test(NAME ${TEST_NAME}_validate
        COMMAND bash -c "${HOMEgfs}/ci/ctests/scripts/validate.sh ${CASE} ${JOB}")
    set_tests_properties(${TEST_NAME}_validate PROPERTIES DEPENDS ${TEST_NAME}_execute LABELS "${CASE};${JOB}")
endfunction()

AddFunctionalTest("C48_ATM" "gfs_fcst_000")
AddFunctionalTest("C48_ATM" "gfs_atmos_pro")
AddFunctionalTest("C48_S2SW" "gfs_fcst_000")
