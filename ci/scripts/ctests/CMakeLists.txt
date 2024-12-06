cmake_minimum_required(VERSION 3.10)

project(FunctionalTests)

enable_testing()

# Add a custom build step to run a script
add_custom_target(BuildScript
    COMMAND bash ${CMAKE_SOURCE_DIR}/sorc/build_all.sh
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Running custom build script"
)

# Add a CTest to run the generate_workflows.sh script with additional arguments
set(YAMLS "C48_ATM.yaml" CACHE STRING "YAML files separated by spaces")
set(RUNTESTS "${CMAKE_BINARY_DIR}/RUNTESTS" )

# Get the Git hash
execute_process(
    COMMAND git rev-parse --short HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

add_test(NAME CreateExperiment
         COMMAND bash ${CMAKE_SOURCE_DIR}/workflow/generate_workflows.sh -y ${YAMLS} -t ${GIT_HASH} ${RUNTESTS})
