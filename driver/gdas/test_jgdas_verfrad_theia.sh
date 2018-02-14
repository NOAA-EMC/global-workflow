#!/bin/ksh

#PBS -o gdas_verfrad.log
#PBS -e gdas_verfrad.err
#PBS -N gdas_verfrad
#PBS -A glbss
#PBS -l procs=1,walltime=0:10:00
#PBS -V

set -x

export PDATE=${PDATE:-2017020606}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=${DATAROOT:-/scratch4/NCEPDEV/da/noscrub/Edward.Safford/test_data}
export COMROOT=${COMROOT:-/scratch4/NCEPDEV/stmp3/$LOGNAME/com}
export STMP_USER=${STMP_USER:-/scratch4/NCEPDEV/stmp3/$LOGNAME}

#############################################################
# Specify versions
#############################################################
export gfs_ver=v15.0.0


#############################################################
# Add nwpara tools to path
#############################################################
NWPROD=${NWPROD:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util}
NWPRODush=${NWPRODush:=${NWPROD}/ush}
NWPRODexec=${NWPRODexec:=${NWPROD}/exec}
export PATH=${PATH}:${NWPRODush}:${NWPRODexec}

#############################################################
# Set user specific variables
#############################################################

export RADMON_SUFFIX=${RADMON_SUFFIX:-testrad}
export NWTEST=${NWTEST:-/scratch4/NCEPDEV/da/noscrub/${LOGNAME}/gfs.${gfs_ver}}
export HOMEgfs=${HOMEgfs:-${NWTEST}}
export JOBGLOBAL=${JOBGLOBAL:-${HOMEgfs}/jobs}
export HOMEradmon=${HOMEradmon:-${NWTEST}}
export COM_IN=${COM_IN:-${DATAROOT}}
export TANKverf=${TANKverf:-${COMROOT}/${RADMON_SUFFIX}}

export SUB=${SUB:-/apps/torque/default/bin/qsub}
export NDATE=${NDATE:-ndate}


#######################################################################
#  theia specific hacks for no prod_utils module & no setpdy.sh script
#######################################################################
export MY_MACHINE=theia
prevday=`$NDATE -24 $PDATE`
export PDYm1=`echo $prevday | cut -c1-8`
ln -s ${NWPRODush}/startmsg.sh ${STMP_USER}/startmsg
ln -s ${NWPRODush}/postmsg.sh ${STMP_USER}/postmsg
ln -s ${NWPRODush}/prep_step.sh ${STMP_USER}/prep_step
ln -s ${NWPRODush}/err_chk.sh ${STMP_USER}/err_chk
export PATH=$PATH:${STMP_USER}
export utilscript=${utilscript:-${NWPRODush}}		# err_chk calls postmsg.sh
							#   directly so need to override
							#   utilscript location for theia
#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VERFRAD

exit

