#!/bin/bash

set -x

#machine=azure
machine=aws

#srcdir=/contrib/Wei.Huang/debug/global-workflow-cloud
srcdir=/contrib/Wei.Huang/src/global-workflow-cloud

for item in gdas gfs_utils gsi_enkf gsi_monitor gsi_utils \
            ufs_model ufs_utils upp verif-global
do
  tf=${item}.${machine}.tar
  if [ -f ${srcdir}/sorc/mods4${machine}/${tf} ]
  then
    dir=${srcdir}/sorc/${item}.fd
    if [ -d ${dir} ]
    then
      cd ${dir}
      tar xvf ../mods4${machine}/${tf}
    fi
  fi
done

