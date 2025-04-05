#! /bin/env bash

basename_list() {
	#
	# Take a list of paths, determines the base name, then 
	#   prepends it to a base path.
	#
	# Syntax:
	#     basename_list base file_in*
	#
	# Arguments:
	#     base:    Common root directory of all paths in list
	#     file_in: List of paths relative to $base/
	#
	# Returns:
	#     List of paths constructed by prepending $base to each 
	#       item in $file_in
	#
	base="${1}"
	list=""

	for file_in in "${@:2}"; do
		list="$list ${base}$(basename $file_in)"
	done
	echo $list
}
