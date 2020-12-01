#!/bin/sh

#BSUB -J gfs_wafs_gcip
#BSUB -cwd /gpfs/hps/ptmp/Yali.Mao
#BSUB -oo /gpfs/hps/ptmp/Yali.Mao/gfs_wafs_gcip.o%J
#BSUB -eo /gpfs/hps/ptmp/Yali.Mao/gfs_wafs_gcip.o%J
#BSUB -L /bin/sh
#BSUB -q dev
#BSUB -W 01:00
#BSUB -P GFS-T2O
#BSUB -M 1024
#BSUB -extsched 'CRAYLINUX[]' -R '1*{select[craylinux && !vnode]} + 1*{select[craylinux && vnode]span[ptile=24] cu[type=cabinet]}'

############################################
# Loading module
############################################
. $MODULESHOME/init/ksh
module load PrgEnv-intel ESMF-intel-haswell/3_1_0rp5 cfp-intel-sandybridge iobuf craype-hugepages2M craype-haswell
#module load cfp-intel-sandybridge/1.1.0
module use /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_envir
module load prod_util
module load prod_util/1.0.4
module load grib_util/1.0.3

module use /gpfs/hps/nco/ops/nwtest/modulefiles
module load dumpjb/4.0.0

set -xa

export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered 

####export OMP_NUM_THREADS=1
####export MP_MPILIB=mpich2
####export MP_EUILIB=us
####export MP_LABELIO=yes
####export MP_COMPILER=intel

############################################
# required exports!
############################################
### envir: prod/dev/para/bkup/ges/test bkup2/ges2/para2/prod2/prd_p6
export envir=test

export cyc=${cyc:-00}

# gfs_wafs_gcip for generating global icing analysis for every 3 hours
export job=gfs_wafs_gcip_${cyc}

############################################
# set NET RUN
############################################
### NET: gfs/para/parallel-test/nam/rap/ dump/ens/nawips/nesdis/
export NET=gfs
export RUN=gfs


############################################
# Specify versions
############################################
export gfs_ver=v4.0.0
export obsproc_dump_ver=v4.0.0
export obsproc_bufr_dumplist_ver=v1.3.0

export NWROOT=/gpfs/hps/nco/ops/nwtest
# define the correct location of dump tools, do not mix with NWROOT of parallel GFS
export obsproc_dump_ver=${obsproc_dump_ver:-v4.0.0}
export HOMEobsproc_dump=${HOMEobsproc_dump:-$NWROOT/obsproc_dump.${obsproc_dump_ver}}
export obsproc_shared_bufr_dumplist_ver=${obsproc_shared_bufr_dumplist_ver:-v1.3.0}
export HOMEobsproc_shared_bufr_dumplist=${HOMEobsproc_shared_bufr_dumplist:-$NWROOT/obsproc_shared/bufr_dumplist.${obsproc_shared_bufr_dumplist_ver}}

############################################
# user defined
############################################
export pid=${pid:-$$}
export jobid=${job}.${pid}

#needs to be the current date to make sure comparison is good at $PCOM
#export PDY=`$NDATE -24 | cut -c 1-8`
export PDY=`cut -c 7-14 $COMROOT/date/t00z`
export PDY=20170329

USER=`whoami`

############################################
# SENDCOM=YES--Copy output file to /com
# SENDECF=YES--Allow to talk back to ECF
# SENDDBN=YES--Alert output file to TOC
# KEEPDATA=NO--Remove temporary working
############################################
export SENDCOM=YES
export SENDDBN=NO
#export SENDDBN_NTC=NO
#export SENDDBN_GB2=NO
export KEEPDATA=YES
 
############################################
# Define ROOT directories
############################################
export COMROOT=/gpfs/hps/emc/global/noscrub/Yali.Mao/datainput/com2
export COMROOT=/gpfs/hps/ptmp/Yali.Mao/com2
#export NWROOTp1=/nwprod
export PCOMROOT=/gpfs/hps/ptmp/${USER}/pcom
export NWROOT=/gpfs/hps/emc/global/noscrub/Yali.Mao/project
export DATAROOT=/gpfs/hps/ptmp/${USER}
if [[ `hostname` =~ "^l" ]] ; then ! LUNA/TIDE
  # for satellite data
  export DCOMROOT=/gpfs/tp1/nco/ops/dcom
  # for radar data
  export COMROOTp1=/gpfs/tp1/nco/ops/com
else                               ! SURGE/GYRE
  # for satellite data
  export DCOMROOT=/gpfs/gp1/nco/ops/dcom
  # for radar data
  export COMROOTp1=/gpfs/gp1/nco/ops/com
fi
# For canned data:
#export DCOMROOT=/gpfs/hps/emc/global/noscrub/Yali.Mao/datainput/dcom
#export COMROOTp1=/gpfs/hps/emc/global/noscrub/Yali.Mao/datainput/com

############################################
# Define DATA PCOM, COMOUT and COMIN
############################################
export DATA=${DATA:-${DATAROOT}/${jobid}}
export PCOM=${PCOM:-$PCOMROOT/wafs}
export COMOUT=/gpfs/hps/ptmp/${USER}/gcip.$PDY

export COMINgfs=$COMROOT/gfs/$envir/gfs.$PDY
export COMINsat=${COMINsat:-$DCOMROOT/us007003/$PDY/mcidas}
export COMINradar=${COMINradar:-$COMROOTp1/hourly/prod/radar.$PDY}

export jlogfile=/$DATA/jlogfile.${jobid}

################################
# Set up the HOME directory
################################
export HOMEgfs=${HOMEgfs:-${NWROOT}/wafs_cray.${gfs_ver}}
export HOMEgfs=/gpfs/hps/emc/global/noscrub/Yali.Mao/project/wafs_trunk

############################################
# run the job
#############################################
export SHOUR=00
export EHOUR=03
export FHINC=03

sh $HOMEgfs/jobs/JGFS_WAFS_GCIP

#############################################
# compare the output (optional)
#############################################
if [ $USER = 'Yali.Mao' ] ; then
  echo ""
fi
exit