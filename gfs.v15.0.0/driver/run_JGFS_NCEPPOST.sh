#!/bin/sh 

#BSUB -a poe
#BSUB -P GFS-T2O
#BSUB -eo gfspost1.dayfile.%J
#BSUB -oo gfspost1.dayfile.%J
#BSUB -J gfspost1
#BSUB -network type=sn_all:mode=US
#BSUB -q "debug2"  
#BSUB -n 24
#BSUB -R span[ptile=8]
#BSUB -R affinity[core(3)]
#BSUB -x
#BSUB -W 00:15

#############################################################
#  Function been tested:            GFS master pgb file for a giving hour.
#
#  Calling sequence:                run_JGFS_NCEPPOST.sh -> JGFS_NCEPPOST -> exgfs_nceppost.sh.ecf -> global_nceppost.sh -> ncep_post
#
#  Initial condition:               CDATE=2016020900 (where /global/noscrub/emc.glopara/com/gfs/para/gfs.${PDY}${cyc} has data
#                                   post_times="12" (Which hour of the master grib2 file to generate)
#                                   GRIBVERSION=${GRIBVERSION:-'grib2'} (Grib2 data for the master pgb)
#                                   
#
#  Usage:                           bsub<run_JGFS_NCEPPOST.sh (modify JGFS_NCEPPOST to keep DATA for review)
#
#  Data_In:                         /global/noscrub/emc.glopara/com/gfs/para/gfs.${PDY}${cyc}
#
#  Data_Out:                        /ptmpd2/Lin.Gan/nceppostgfs_${PDY}${cyc}
#
#  Result verification:             cmp ${DATA}/gfs.t00z.master.grb2f12 ${Data_Out}/gfs.t00z.master.grb2f12
#############################################################

set -x
export OMP_NUM_THREADS=3
export MP_MPILIB=mpich2
export MP_EUILIB=us
export MP_LABELIO=yes
export MP_COMPILER=intel
export FOR_DISABLE_STACK_TRACE=true

export MP_EUIDEVELOP=min
export KMP_STACKSIZE=2048m
export MPICH_ALLTOALL_THROTTLE=0
export MP_SINGLE_THREAD=yes
export MP_EAGER_LIMIT=65536
export MP_USE_BULK_XFER=no
export MP_COLLECTIVE_OFFLOAD=no
export MP_SHARED_MEMORY=yes
####################################
# Loading module
####################################
module load ibmpe ics lsf prod_util grib_util
# module load prod_util

####################################
# Specify whether the run is production or development
####################################
export LD_LIBRARY_PATH=/nwprod/lib:$LD_LIBRARY_PATH
export envir=${envir:-prod}

####################################
# Specify version numbers
####################################
export post_ver=${post_ver:-v7.0.0}
export crtm_ver=${crtm_ver:-v2.0.6}
export gsm_ver=${gsm_ver:-v13.0.0}
export gfs_ver=${gfs_ver:-v13.0.0}
export NWROOT=${NWROOT:-/nwprod2}
export COMROOT=${COMROOT:-/com}
export GESROOT=${GESROOT:-/nwges}

export pid=$$

export DATA_IN=/ptmpd2/$USER

#### export CDATE=2016020900
export CDATE=2016022600

export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export DATA=$DATA_IN/nceppostgfs_${PDY}

export NET=gfs
export RUN=gfs

####################################
# Reference to testing location of super structure
####################################
export HOMEglobal=/global/save/Lin.Gan/prpost7/NCO/ncep_post_test/post_trunk_regression_test/svntags/global_shared.v13.0.0
export HOMECRTM=${HOMECRTM:-/nw${envir}/lib/crtm/$crtm_ver}
export FIXCRTM=${FIXCRTM:-$HOMECRTM/sorc/fix}
export HOMEgfs=/global/save/Lin.Gan/prpost7/NCO/ncep_post_test/post_trunk_regression_test/svntags/gfs.v13.0.0
#### export COMIN=/global/save/Lin.Gan/prpost7/NCO/ncep_post_test/post_trunk_regression_test/data_in
export COMIN=/global/noscrub/emc.glopara/com/gfs/para/gfs.20160226

export COMOUT=$DATA

export POSTGPEXEC=$HOMEglobal/exec/ncep_post

####################################
# Initial condition
####################################
export post_times="21"
export IDRT=0
export res=0p25
export GRIBVERSION=${GRIBVERSION:-'grib2'}

export REMOVE_DATA="NO"

${HOMEgfs}/jobs/JGFS_NCEPPOST

date
exit
