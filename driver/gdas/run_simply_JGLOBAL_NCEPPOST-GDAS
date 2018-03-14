#!/bin/sh

#BSUB -o /gpfs/hps3/stmp/emc.glopara/ttt/simply_post/tmp/outputs/test_gdas_post_simply.o%J
#BSUB -J test_simply_gdas_post
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -W 01:30
#BSUB -q devhigh
#BSUB -P GFS-T2O
#BSUB -M 1000
#BSUB -cwd /gpfs/hps3/stmp/emc.glopara/ttt/simply_post/tmp/outputs

set -x

############################################
# specify computation resource
############################################
export NODES=6
export ntasks=72
export ptile=12
export threads=1

############################################
# Loading module
############################################
. $MODULESHOME/init/ksh
module load PrgEnv-intel ESMF-intel-haswell/3_1_0rp5 cfp-intel-sandybridge iobuf craype-hugepages2M craype-haswell
module use /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_envir
module load prod_util/1.0.23
module load grib_util/1.0.3
module list

############################################
# specify PDY (the cycle start yyyymmdd) and cycle
############################################
export CDATE=2018030600
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export cycle=t${cyc}z

############################################
# specify the directory environment for executable, it's either para or prod
############################################
export envir=prod

####################################
# Specify RUN Name and model
####################################
export NET=gfs
export RUN=gdas
export CDUMP=gdas
export RUN_ENVIR=emc

############################################
# set up running dir
############################################
export job=${RUN}_post_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}

############################################
# outputs location is the job output
############################################
mkdir -p /gpfs/hps3/stmp/emc.glopara/ttt/simply_post/tmp/outputs

############################################
# Set DATA location
############################################
export DATA=/gpfs/hps3/stmp/emc.glopara/ttt/simply_post/DATA/$jobid
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

##############################################
# Define COM directories
##############################################
export COMROOT=/gpfs/hps3/stmp/emc.glopara/ttt/simply_post
export COMINP=${COMROOT}/com/gfs/prod
export ROTDIR=$COMINP
export COMIN=$COMINP/${RUN}.${PDY}/${cyc}

############################################
# specify user's own post working directory for testing
############################################
export svndir=/gpfs/hps3/ptmp/emc.glopara/ncep_post_simp/public_release_v1/fv3gfs
export HOMEgfs=${svndir}
export FIXgfs=$HOMEgfs/fix
export nemsioget=$HOMEgfs/exec/nemsio_get
export POSTGRB2TBL=/gpfs/hps/nco/ops/nwprod/lib/g2tmpl/v1.5.0/src/params_grib2_tbl_new
export POSTGPEXEC=${HOMEgfs}/exec/gfs_ncep_post
export CTLFILE=${HOMEgfs}/parm/post
export PARMpost=${HOMEgfs}/parm/post
export EXPDIR=${HOMEgfs}/parm/config
export PGB1F=YES
export FLXF=NO
export GOESF=NO
export GTGF=NO
export KEEPDATA=NO

####################################
# Specify Forecast Hour Range
####################################

if [ $RUN = gdas ]; then
    export allfhr="anl 000"
elif [ $RUN = gfs ]; then
    export allfhr="anl 000"
fi

####################################
# Execute J-Job
####################################
for post_times in $allfhr; do

    export post_times

    date

    $HOMEgfs/jobs/JGLOBAL_NCEPPOST

    echo $?

    date

done

