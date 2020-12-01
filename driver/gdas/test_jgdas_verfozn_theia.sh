#!/bin/ksh

#PBS -o gdas_verfozn.log
#PBS -e gdas_verfozn.err
#PBS -N gdas_verfozn
#PBS -A glbss
#PBS -l procs=1,walltime=0:10:00
#PBS -V

set -x

export PDATE=${PDATE:-2017071806}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfozn.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=${DATAROOT:-/scratch4/NCEPDEV/da/noscrub/Edward.Safford/ozn_test_data}
export COMROOT=${COMROOT:-/scratch4/NCEPDEV/stmp3/$LOGNAME/com}


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

export OZNMON_SUFFIX=${OZNMON_SUFFIX:-testozn}
export NWTEST=${NWTEST:-/scratch4/NCEPDEV/da/noscrub/${LOGNAME}/gfs.${gfs_ver}}
export HOMEgfs_ozn=${HOMEgfs_ozn:-${NWTEST}}
export JOBGLOBAL=${JOBGLOBAL:-${HOMEgfs_ozn}/jobs}
export HOMEoznmon=${HOMEoznmon:-${NWTEST}}
export COM_IN=${COM_IN:-${DATAROOT}}
export OZN_TANKDIR=${OZN_TANKDIR:-${COMROOT}/${OZNMON_SUFFIX}}

export SUB=${SUB:-/apps/torque/default/bin/qsub}
export NDATE=${NDATE:-ndate}


#######################################################################
#  theia specific hacks for no prod_utils module & no setpdy.sh script
#######################################################################
export MY_MACHINE=theia
prevday=`$NDATE -24 $PDATE`
export PDYm1=`echo $prevday | cut -c1-8`
ln -s ${NWPRODush}/startmsg.sh ${COMROOT}/startmsg
ln -s ${NWPRODush}/postmsg.sh ${COMROOT}/postmsg
ln -s ${NWPRODush}/prep_step.sh ${COMROOT}/prep_step
ln -s ${NWPRODush}/err_chk.sh ${COMROOT}/err_chk
export PATH=$PATH:${COMROOT}
export utilscript=${utilscript:-${NWPRODush}}		# err_chk calls postmsg.sh
							#   directly so need to override
							#   utilscript location for theia
#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VERFOZN

exit

