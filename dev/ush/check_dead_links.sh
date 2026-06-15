#!/bin/bash

#######################################
# Check a directory for dead links
# Usage:
#   ./check_dead_links.sh <target_directory>
# Outputs:
#   number and list of dead links only if found
# Returns:
#   0 if check completes, 1 for invalid usage
#######################################

# Check if exactly one argument was provided
if [[ $# -ne 1 ]]; then
    echo "Error: $0 accepts a single argument."
    echo "Usage: $0 <target_directory>"
    exit 1
fi

# Check that the argument is an existing directory
if [[ ! -d "$1" ]]; then
    echo "Error: '$1' is not a directory or does not exist."
    echo "Usage: $0 <target_directory>"
    exit 1
fi

# Find dead links in the target directory
dead_links=$(find "$1" -xtype l)

# Check if the dead_links variable is non-empty
if [[ -n "${dead_links}" ]]; then
    # count dead links
    count_dead_links=$(echo "${dead_links}" | wc -l)

    echo "Found ${count_dead_links} dead links in $1:"
    echo "${dead_links}"
fi

# Success if script completes
exit 0
