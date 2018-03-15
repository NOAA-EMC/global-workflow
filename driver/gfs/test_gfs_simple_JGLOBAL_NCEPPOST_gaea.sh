#!/bin/sh

#PBS -o /lustre/f1/Terry.McGuinness/ROTDIR/simple_gfs_post_test.out
#PBS -e /lustre/f1/Terry.McGuinness/ROTDIR/simple_gfs_post_test.err
#PBS -N test_simply_gfs_post
#PBS -l walltime=4:00:00
#PBS -q eslogin
#PBS -A ncep
#PBS -l nodes=12:ppn=6
#PBS -extsched 'CRAYLINUX[]'
#PBS -l partition=sjet
#PBS -S /bin/bash     
#export MPI_TYPE_DEPc'

set -x

myROTDIR=/lustre/f1/Terry.McGuinness/ROTDIR

############################################
# specify computation resource
############################################
export NODES=6
export ntasks=72
export ptile=12
export threads=1

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
export RUN=gfs
export CDUMP=gfs
export RUN_ENVIR=emc

############################################
# set up running dir
############################################
export job=${RUN}_post_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}

############################################
# Set DATA location
############################################
export DATA=$myROTDIR/DATA/$jobid
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
# TODO check COMINP
export COMINP=$myROTDIR
export ROTDIR=$COMINP
export COMIN=$COMINP/${RUN}.${PDY}/${cyc}
export COMOUT=$COMIN/COMOUT

############################################
# specify user's own post working directory for testing
############################################
export svndir=/lustre/f1/Terry.McGuinness/fv3gfs_release_branch
export HOMEgfs=${svndir}
export FIXgfs=$HOMEgfs/fix
export nemsioget=$HOMEgfs/exec/nemsio_get
# TODO check if we need POSTGRB2TBL
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
    #export allfhr="anl 00 03 06 09"
    export allfhr="anl 000 006"
elif [ $RUN = gfs ]; then
    #export allfhr="anl 00 01 06 12 60 120 180 240 252 384"
    export allfhr="anl 000"
fi

#############################################################

for post_times in $allfhr; do

    export post_times

    date

    $HOMEgfs/jobs/JGLOBAL_NCEPPOST

    echo $?

    date

done
