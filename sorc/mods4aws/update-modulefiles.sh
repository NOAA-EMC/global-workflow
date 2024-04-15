#!/bin/bash

set -x

#machine=azure
machine=aws

for item in gdas gfs_utils gsi_enkf gsi_monitor gsi_utils \
            ufs_model ufs_utils upp verif-global
do
  tf=${item}.${machine}.tar
  if [ -f ${tf} ]
  then
    dir=${item}.fd
    if [ -d ${dir} ]
    then
      cd ${dir}
      tar xvf ../mods4${machine}/${tf}
      cd ..
    fi
  fi
done

