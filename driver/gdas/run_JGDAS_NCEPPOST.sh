#!/bin/sh

#BSUB -o out_gdas_nemsio_p25_para_mpiio.%J
#BSUB -e err_gdas_nemsio_p25_para_mpiio.%J
#BSUB -J NEMSPOST 
#BSUB -extsched 'CRAYLINUX[]' -R '1*{select[craylinux && !vnode]} + 96*{select[craylinux && vnode]span[ptile=24] cu[type=cabinet]}'
#BSUB -W 00:40
#BSUB -q dev
#BSUB -P GFS-T2O
#BSUB -M 1000
#BSUB -cwd /gpfs/hps/emc/global/noscrub/Hui-Ya.Chuang/nems_sample_output_T1534

set -x

# specify user's own post working directory for testing
export svndir=/gpfs/hps/emc/global/noscrub/Hui-Ya.Chuang/post_trunk
export MP_LABELIO=yes
export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=2048M
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered

############################################
# Loading module
############################################
. $MODULESHOME/init/ksh
module load PrgEnv-intel ESMF-intel-haswell/3_1_0rp5 cfp-intel-sandybridge iobuf craype-hugepages2M craype-haswell
#module load cfp-intel-sandybridge/1.1.0
module use /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_envir
#module load prod_util
module load prod_util/1.0.4
module load grib_util/1.0.3

# specify PDY (the cycle start yyyymmdd) and cycle
export PDY=20170212
export cyc=00
export cycle=t${cyc}z


# specify the directory environment for executable, it's either para or prod
export envir=prod

# set up running dir

export user=`whoami`
export DATA=/gpfs/hps/ptmp/${user}/gdas.${PDY}${cyc}_nemsio_mpiio
mkdir -p $DATA
cd $DATA
rm -f ${DATA}/*

####################################
# Specify RUN Name and model
####################################
export NET=gfs
#export RUN=gdas

####################################
# Determine Job Output Name on System
####################################
#export pgmout="OUTPUT.${pid}"
#export pgmerr=errfile

####################################
# SENDSMS  - Flag Events on SMS
# SENDCOM  - Copy Files From TMPDIR to $COMOUT
# SENDDBN  - Issue DBNet Client Calls
# RERUN    - Rerun posts from beginning (default no)
# VERBOSE  - Specify Verbose Output in global_postgp.sh
####################################
export SAVEGES=NO
export SENDSMS=NO
export SENDCOM=YES
export SENDDBN=NO
export RERUN=NO
export VERBOSE=YES

export HOMEglobal=${svndir}
export HOMEgfs=${svndir}          
export HOMEgdas=${svndir}
                                                                                               
##############################################
# Define COM directories
##############################################
export COMIN=/gpfs/hps/emc/global/noscrub/Hui-Ya.Chuang/para_look_alike/gdas.${PDY}
# specify my own COMOUT dir to mimic operations
export COMOUT=$DATA
mkdir -p $COMOUT

date

#export OUTTYP=4
# need to set FIXglobal to global share superstructure if testing post in non
# super structure environement
export FIXglobal=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17_final/global_shared.v14.1.0/fix
export APRUN="aprun -j 1 -n24 -N8 -d1 -cc depth"
export nemsioget=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17_final/global_shared.v14.1.0/exec/nemsio_get

export KEEPDATA=YES
#export POSTGRB2TBL=$HOMEglobal/parm/params_grib2_tbl_new
$HOMEgfs/jobs/JGDAS_NCEPPOST

#############################################################

date

echo $?



