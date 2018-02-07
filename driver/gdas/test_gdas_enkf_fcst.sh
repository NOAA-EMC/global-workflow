#!/bin/sh --login

#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -o gdas_enkf_fcst.o%J
#BSUB -e gdas_enkf_fcst.o%J
#BSUB -J gdas_enkf_fcst
#BSUB -q devonprod
#BSUB -M 3072
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -W 01:00
#BSUB -cwd /gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/work/gdas.v14.1.0/driver

set -x

# 20 nodes = 18 compute nodes + 2 i/o nodes
#   set WRT_GROUP=2 for 2 i/o nodes (see ../parm/gdas_enkf_fcst.parm)
#   set WRTPE_PER_GROUP=4 to match ptile
export NODES=20
export ntasks=80
export ptile=4
export threads=6

export CDATE=2017011900
export ENSGRP=1

export grp=$ENSGRP
if [[ $grp -lt 10 ]]; then
   export grp=0$grp
fi


#############################################################
# Specify whether the run is production or development
#############################################################
export RUN_ENVIR=para
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export job=gdas_enkf_fcst_grp${grp}_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=/gpfs/hps/stmp/$LOGNAME/test


#############################################################
# Specify versions
#############################################################
export gdas_ver=v14.1.0
export global_shared_ver=v14.1.0
export grib_util_ver=1.0.3
export prod_util_ver=1.0.6


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/sh
module load grib_util/$grib_util_ver
module load prod_util/$prod_util_ver
module load craype-hugepages4M

module list


#############################################################
# WCOSS_C environment settings
#############################################################
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=2G
export OMP_NUM_THREADS=$threads
export FORT_BUFFERED=true
export NTHREADS_EFCS=$threads


#############################################################
# Set user specific variables
#############################################################
export NWTEST=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/work
export PARA_CONFIG=$NWTEST/gdas.${gdas_ver}/driver/para_config.gdas_enkf_fcst
export JOBGLOBAL=$NWTEST/gdas.${gdas_ver}/jobs


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ENKF_FCST

exit
