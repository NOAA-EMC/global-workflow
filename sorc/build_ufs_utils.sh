#! /usr/bin/env bash
set -eux

script_dir=$(dirname ${BASH_SOURCE[0]})
cd ${script_dir}/ufs_utils.fd

./build_all.sh

exit

