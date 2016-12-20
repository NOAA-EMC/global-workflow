#!/bin/sh 

#BSUB -a poe
#BSUB -P GFS-T2O
#BSUB -eo gdaspost1.dayfile.%J
#BSUB -oo gdaspost1.dayfile.%J
#BSUB -J gdaspost1
#BSUB -network type=sn_all:mode=US
#BSUB -q "debug2"  
#BSUB -n 24
#BSUB -R span[ptile=8]
#BSUB -R affinity[core(3)]
#BSUB -x
#BSUB -W 00:25

#############################################################
#  Function been tested:            GDAS master pgb file. 
#
#  Calling sequence:                run_JGDAS_NCEPPOST.sh -> JGDAS_NCEPPOST -> exgfs_nceppost.sh.ecf -> global_nceppost.sh -> ncep_post
#
#  Initial condition:               CDATE=2016020900 (where /global/noscrub/emc.glopara/com/gfs/para/gfs.${PDY}${cyc} has data
#
#  Usage:                           bsub<run_JGDAS_NCEPPOST.sh (modify JGDAS_NCEPPOST to keep DATA for review)
#
#  Data_In:                         /global/noscrub/emc.glopara/com/gfs/para/gdas.${PDY}${cyc}
#
#  Data_Out:                        /ptmpd2/Lin.Gan/nceppostgdas_${PDY}${cyc}
#
#  Result verification:             cmp ${DATA}/gdas1.t00z.master.grb2f00 ${Data_Out}/gdas1.t00z.master.grb2f00
#############################################################

set -x
export OMP_NUM_THREADS=3
export MP_MPILIB=mpich2
export MP_EUILIB=us
export MP_LABELIO=yes
export MP_COMPILER=intel
export FOR_DISABLE_STACK_TRACE=true

####################################
# Loading module
####################################
module load ibmpe ics lsf prod_util grib_util
# module load ibmpe ics lsf grib_util

####################################
#Specify whether the run is production or development
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
export CDATE=2016022500

export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export DATA=$DATA_IN/nceppostgdas_${PDY}

#export job=gdas_nceppost_${cyc}

export NET=gfs
export RUN=gdas

####################################
# Reference to testing location of super structure
####################################
export HOMEglobal=/global/save/Lin.Gan/prpost7/NCO/ncep_post_test/post_trunk_regression_test/svntags/global_shared.v13.0.0
export HOMECRTM=${HOMECRTM:-/nw${envir}/lib/crtm/$crtm_ver}
export FIXCRTM=${FIXCRTM:-$HOMECRTM/sorc/fix}
export HOMEgfs=/global/save/Lin.Gan/prpost7/NCO/ncep_post_test/post_trunk_regression_test/svntags/gfs.v13.0.0
export HOMEgdas=/global/save/Lin.Gan/prpost7/NCO/ncep_post_test/post_trunk_regression_test/svntags/gdas.v13.0.0
#### export COMIN=/global/save/Lin.Gan/prpost7/NCO/ncep_post_test/post_trunk_regression_test/data_in
export COMIN=/global/noscrub/emc.glopara/com/gfs/para/gdas.20160225
export COMOUT=$DATA

export POSTGPEXEC=$HOMEglobal/exec/ncep_post

export post_times="09"

${HOMEgdas}/jobs/JGDAS_NCEPPOST

date
exit
