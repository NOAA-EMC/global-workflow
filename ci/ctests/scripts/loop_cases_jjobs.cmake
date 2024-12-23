
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