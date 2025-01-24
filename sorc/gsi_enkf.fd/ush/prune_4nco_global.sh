#!/bin/bash

# This script removes or restores directories and/or files found in
# the rlist below from the current working directory.   This script
# is intended to be executed when NCO implements GFS DA updates.
# NCO does not want package installations for operations to include
# non-operational code.   This script removes directories and files
# not used by operations from the installation directory.
#
# Two modes are supported:  prune, restore
#   prune:  use git rm -r to remove directories and files.
#           The directories and files to remove are found
#           in rlist below
#   restore:  use git RESET head and git checkout to restore
#             removed directories and files
#

function version { echo "$@" | awk -F. '{ printf("%d%03d%03d%03d\n", $1,$2,$3,$4); }'; }

set -ex

mode=$1

# Check mode and set string
if [[ "$mode" = "prune" ]]; then
    string="rm -r"
elif [[ "$mode" = "restore" ]]; then
    git_ver=$(git version | cut -d" " -f3)
    if [ $(version $git_ver) -lt $(version "2.23.0") ]; then
	use_checkout="YES"
	string="checkout"
    else
	use_checkout="NO"
	string="restore"
    fi
else
    echo " "
    echo "***ERROR*** invalid mode= $mode"
    echo " valid modes are prune or restore"
    echo " "
    exit
fi


# Set root directory
readonly topdir=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
cd $topdir

echo " "
echo "Execute git $string in $topdir"
echo " "


# Process top level directories
cd $topdir
rlist="regression src/GSD unit-tests"
for type in $rlist; do
    if [[ "$mode" = "prune" ]]; then
	if [ -e $type ]; then
	    git $string ${type}*
	    rc=$?
	    if [[ $rc -ne 0 ]]; then
		echo "***ERROR*** git $string ${type}"
		exit
	    fi
	fi
    elif [[ "$mode" = "restore" ]]; then
	if [[ "$use_checkout" = "YES" ]]; then
	    git reset HEAD ${type}*
            git checkout ${type}*
	    rc=$?
	else
	    git restore --staged ${type}*
	    git restore ${type}*
	    rc=$?
	fi
	if [[ $rc -ne 0 ]]; then
            echo "***ERROR*** restore failed for ${type}"
            exit
        fi
    fi
done


# Process doc directories and files
cd $topdir/doc
rlist="EnKF_user_guide GSI_user_guide README.discover Release_Notes.fv3gfs_da.v15.0.0.txt Release_Notes.gfsda.v16.0.0.txt"
for type in $rlist; do
    if [[ "$mode" = "prune" ]]; then
	if [ -e $type ]; then
	    git $string ${type}*
	    rc=$?
	    if [[ $rc -ne 0 ]]; then
		echo "***ERROR*** git $string ${type}"
		exit
	    fi
	fi
    elif [[ "$mode" = "restore" ]]; then
        if [[ "$use_checkout" = "YES" ]]; then
            git reset HEAD ${type}*
            git checkout ${type}*
            rc=$?
        else
            git restore --staged ${type}*
            git restore ${type}*
            rc=$?
        fi
        if [[ $rc -ne 0 ]]; then
            echo "***ERROR*** restore failed for ${type}"
            exit
        fi
    fi
done


# Process ush directories and files
cd $topdir/ush
rlist="sub"
for type in $rlist; do
    if [[ "$mode" = "prune" ]]; then
	if [ -e $type ]; then
	    git $string ${type}*
	    rc=$?
	    if [[ $rc -ne 0 ]]; then
		echo "***ERROR*** git $string ${type}"
		exit
	    fi
	fi
    elif [[ "$mode" = "restore" ]]; then
        if [[ "$use_checkout" = "YES" ]]; then
            git reset HEAD ${type}* 
            git checkout ${type}*
            rc=$?
        else
            git restore --staged ${type}*
            git restore ${type}*
            rc=$?
        fi
        if [[ $rc -ne 0 ]]; then
            echo "***ERROR*** restore failed for ${type}"
            exit
        fi
    fi
done
