#! /usr/bin/env bash

#---------------------------------------------------------
# cpfsd.sh
#
# A wrapper around cpfs that automatically creates the
# destination directory if it does not already exist
# before copying the file.
#
# USAGE: cpfsd <source> <destination>
#   source      - path to the source file
#   destination - path to the destination file
#---------------------------------------------------------

dest="${!#}"
dest_dir=$(dirname "${dest}")
if [[ ! -d "${dest_dir}" ]]; then
    echo "INFO: Directory ${dest_dir} does not exist, creating..."
    mkdir -p "${dest_dir}"
    err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "ERROR: Failed to create destination directory ${dest_dir}"
        exit "${err}"
    fi
fi
cpfs "$@"
err=$?
if [[ ${err} -ne 0 ]]; then
    echo "ERROR: cpfs failed to copy to ${dest}"
    exit "${err}"
fi
