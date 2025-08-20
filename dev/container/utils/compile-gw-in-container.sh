#!/bin/bash

 gw_sorc_dir=$1
 cd ${gw_sorc_dir}
 ./build_all.sh gfs sfs gefs
 ./link_workflow.sh

