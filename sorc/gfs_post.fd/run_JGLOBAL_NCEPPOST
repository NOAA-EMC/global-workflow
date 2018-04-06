#!/bin/sh

#BSUB -o STDDIR/test_post_RRR_CURRENTDATE.o%J
#BSUB -e STDDIR/test_post_RRR_CURRENTDATE.o%J
#BSUB -J test_post
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -W 00:30
#BSUB -q debug
#BSUB -P GFS-T2O
#BSUB -M 1000
#BSUB -cwd STDDIR

set -x

# specify computation resource
export NODES=8
export ntasks=96
export ptile=12
export threads=1
export MP_LABELIO=yes
export OMP_NUM_THREADS=$threads
export APRUN="aprun -j 1 -n${ntasks} -N${ptile} -d${threads} -cc depth"

# specify user's own post working directory for testing
#export svndir=/u/Wen.Meng/noscrubc/ncep_post/post_fv3gfs_flat



############################################
# Loading module
############################################
. $MODULESHOME/init/ksh
module load PrgEnv-intel ESMF-intel-haswell/3_1_0rp5 cfp-intel-sandybridge iobuf craype-hugepages2M craype-haswell
#module load cfp-intel-sandybridge/1.1.0
module use /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_envir
#module load prod_util
module load prod_util/1.0.23
module load grib_util/1.0.3
##module load crtm-intel/2.2.4
module list

# specify version numbers
#export hwrf_ver=v11.0.5

# specify PDY (the cycle start yyyymmdd) and cycle
export CDATE=CURRENTDATE
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export cycle=t${cyc}z


# specify the directory environment for executable, it's either para or prod
export envir=prod

####################################
# Specify RUN Name and model
####################################
export NET=gfs
export RUN=RRR
#export RUN=gfs

# set up running dir
export job=${RUN}_post_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
mkdir -p /gpfs/hps3/ptmp/$LOGNAME/nceppost/logs
export jlogfile=/gpfs/hps3/ptmp/$LOGNAME/nceppost/logs/jlogfile.${job}.${pid}

export DATA=/gpfs/hps/stmp/$LOGNAME/test/$jobid
mkdir -p $DATA
cd $DATA
rm -f ${DATA}/*

####################################
# SENDSMS  - Flag Events on SMS
# SENDCOM  - Copy Files From TMPDIR to $COMOUT
# SENDDBN  - Issue DBNet Client Calls
# RERUN    - Rerun posts from beginning (default no)
# VERBOSE  - Specify Verbose Output in global_postgp.sh
####################################
export SENDCOM=YES
export SENDDBN=NO
export RERUN=NO
export VERBOSE=YES

export HOMEgfs=${svndir}

##############################################
# Define COM directories
##############################################
export COMIN=$COMINP/${RUN}.${PDY}/${cyc}
# specify my own COMOUT dir to mimic operations
export COMOUT=/gpfs/hps3/ptmp/$LOGNAME/com2/gfs/test/${RUN}.$PDY/${cyc}
mkdir -p $COMOUT

# specify variables if testing post in non gfs structure environment
#export FIXglobal=/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/master/global_shared.v15.0.0/fix
export FIXgfs=/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/master/global_shared.v15.0.0/fix
export nemsioget=/gpfs/hps3/emc/global/noscrub/emc.glopara/git/fv3gfs/master/global_shared.v15.0.0/exec/nemsio_get
#export POSTGRB2TBL=${HOMEglobal}/parm/params_grib2_tbl_new
export POSTGRB2TBL=/gpfs/hps/nco/ops/nwprod/lib/g2tmpl/v1.5.0/src/params_grib2_tbl_new
export POSTGPEXEC=${HOMEgfs}/exec/ncep_post
export PARMpost=${HOMEgfs}/parm

#export PGBF=NO
export PGB1F=YES
export FLXF=NO
export GOESF=NO
export GTGF=NO
export KEEPDATA=NO

####################################
# Specify Forecast Hour Range
####################################

#if [ $RUN = gdas ]; then
#    #export allfhr="anl 00 03 06 09"
#    export allfhr="anl 000 006"
#elif [ $RUN = gfs ]; then
#    #export allfhr="anl 00 01 06 12 60 120 180 240 252 384"
#    export allfhr="000"
#fi

#############################################################

for post_times in $allfhr; do

    export post_times

    date

    $HOMEgfs/jobs/J_NCEPPOST

    echo $?

    date

done

#############################################################
