#! /usr/bin/env bash

nb_copy() {
    #
    # TODO - Extend this to take multiple arguments for file_in (like cp)
    #
    # Copy a file if it exists, print a warning otherwise but don't
    #   error.
    #
    # Syntax
    #   nb_copy file_in file_out
    #
    #   Arguments
    #     file_in: the file to copy
    #     file_out: the destination of the copy
    #
    #   Environment variables
    #     NCP: Command to use to copy (default: cp)
    #
    local file_in="${1}"
    local file_out="${2}"
    if [[ -f ${file_in} ]]; then
        ${NCP:-cp} ${file_in} ${file_out}
    else
        echo "WARNING: No file ${file_in} found (pwd: $(pwd))"
    fi
}
