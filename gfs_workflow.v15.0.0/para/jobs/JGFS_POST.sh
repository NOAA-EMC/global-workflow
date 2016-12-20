#!/bin/ksh
###BSUB -L /bin/sh
###BSUB -P GFS-T2O
###BSUB -e /gpfs/hps/ptmp/Fanglin.Yang/fv3/log.nceppost                             
###BSUB -o /gpfs/hps/ptmp/Fanglin.Yang/fv3/log.nceppost                             
###BSUB -J nceppost                  
###BSUB -q dev     
###BSUB -M 3072
###BSUB -W 10:00
###BSUB -cwd /gpfs/hps/ptmp/Fanglin.Yang/fv3
###BSUB -extsched 'CRAYLINUX[]' 
set -x

export machine=${machine:-WCOSS_C}
##if [ $machine = WCOSS_C ]; then 
## . $MODULESHOME/init/sh 2>>/dev/null
## module load PrgEnv-intel cray-mpich prod_util prod_envir grib_util/1.0.3 2>>/dev/null
## module load cfp-intel-sandybridge crtm-intel/2.2.4 g2tmpl-intel/1.4.0 2>>/dev/null
##fi

#--------------------------------------------------------------------
#--Fanglin Yang, December 2016
#  Running NCEP Unified Post to process FV3GFS forecasts.
#--------------------------------------------------------------------

export CDATE=${CDATE:-2016100300}
export CDUMP=${CDUMP:-gfs}
export CASE=${CASE:-C192}              ;#C48 C96 C192 C384 C768 C1152 C3072
export GG=${master_grid:-0p25deg}      ;#1deg 0p5deg 0p25deg 0p125deg
export FHMAX=${FHMAX:-240}
export FHOUT=${FHOUT:-6}
export NFCST=${NFCST:-$((FHMAX/FHOUT+1))} ;#number of forecatsts included in netCDF file
export fdiag=${fdiag:-none}               ;#specified forecast output hours 

export PSLOT=${PSLOT:-fv3gfs}
export PTMP=${PTMP:-/gpfs/hps/ptmp}
export COMROT=${COMROT:-$PTMP/$LOGNAME/pr${PSLOT}}
export BASEDIR=${BASEDIR:-/gpfs/hps/emc/global/noscrub/Fanglin.Yang/svn/gfs/fv3gfs/gfs_workflow.v15.0.0/para}
export BASE_GSM=${BASE_GSM:-/gpfs/hps/emc/global/noscrub/Fanglin.Yang/svn/gfs/fv3gfs/global_shared.v15.0.0}

#export NODES=4
#export max_core=${pe_node:-24}
#export thread=${thread:-1}
##export npe_node=$((max_core/thread))
#export npe_node_po=8                     
#export npes=$((NODES*npe_node_po))
#export APRUN_loc="aprun -n $npes -N $npe_node_po -j 1 -d $thread -cc depth"
export APRUN=${APRUN_NP:-""}
#-------------------------------------------------------------------

export PDY=`echo $CDATE|cut -c 1-8`
export cyc=`echo $CDATE|cut -c 9-10`
export cycle=t${cyc}z

####################################
# Specify RUN Name and model
####################################
export envir=prod
export NET=fv3
export RUN=gfs

export DATA=${DATA:-${DATATMP:-$PTMP/$LOGNAME/${PSLOT}post}}                             
export COMIN=$DATA
export COMOUT=$COMROT
if [ ! -s $DATA ]; then mkdir -p $DATA ; fi
cd $DATA ||exit 8
rm -f ${DATA}/*

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

####################################
# Specify Execution Areas
####################################
export ver=v15.0.0
export nemsioget=${nemsioget:-/gpfs/hps/emc/global/noscrub/emc.glopara/bin/nemsio_get}
export HOMEglobal=${BASE_POST:-/gpfs/hps/emc/global/noscrub/Fanglin.Yang/NGGPS/post/trunk}                     
export EXECglobal=$HOMEglobal/exec
export USHglobal=$HOMEglobal/ush
export USHgfs=$HOMEglobal/ush
export PARMglobal=$HOMEglobal/parm
export FIXglobal=$BASE_GSM/fix
export NWROOTprod=$NWROOT
export ens=NO



####################################
# Specify Special Post Vars
####################################
export IGEN_ANL=81
export IGEN_FCST=96
export POSTGPVARS="KPO=47,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,"


if [ $fdiag = none ]; then
 fdiag=$( for (( num=1; num<=$NFCST; num++ )); do printf "%d," $(((num-1)*FHOUT)); done )
fi
#---------------------------------------------------
for fhour in $(echo $fdiag | sed "s?,? ?g"); do
#---------------------------------------------------
export post_times=$(printf "%03d" $fhour)

#######################################
# Specify Restart File Name to Key Off
#######################################
# export restart_file=${COMIN}/${RUN}.t${cyc}z.logf
# export restart_file=${COMIN}/${RUN}.t${cyc}z.sf 
  export restart_file=$COMROT/${CASE}_nemsio${GG}.${PDY}${cyc}_FHR

 ln -fs $COMROT/${CASE}_nemsio${GG}.${PDY}${cyc}_FHR${post_times}  $COMIN/${RUN}.t${cyc}z.atmf${post_times}.nemsio 
 ln -fs $COMROT/${CASE}_nemsio${GG}.${PDY}${cyc}_FHR${post_times}  $COMIN/${RUN}.t${cyc}z.flxf${post_times}.nemsio
 export NEMSINP=$COMIN/${RUN}.t${cyc}z.atmf${post_times}.nemsio
 export FLXINP=$COMIN/${RUN}.t${cyc}z.flxf${post_times}.nemsio

####################################
# Specify Timeout Behavior of Post
# SLEEP_TIME - Amount of time to wait for
#              a restart file before exiting
# SLEEP_INT  - Amount of time to wait between
#              checking for restart files
####################################
export SLEEP_TIME=900
export SLEEP_INT=5

export OUTTYP=4
export GRIBVERSION=grib2
export res=$GG

export IDRT=0
if [ $IDRT -eq 0 ] ; then
# 0.125 deg
  if [ $res = "0p125deg" ] ; then
    export LONB=2880
    export LATB=1440
# 0.25 deg
  elif [ $res = "0p25deg" ] ; then
    export LONB=1440
    export LATB=720
# 0.5 deg
  elif [ $res = "0p50deg" ] ; then
    export LONB=720
    export LATB=360
  fi
fi

export POSTGPEXEC=${POSTGPEXEC:-$HOMEglobal/exec/ncep_post}
export POSTGPSH=${POSTGPSH:-$HOMEglobal/ush/global_nceppost.sh}
export POSTGRB2TBL=${POSTGRB2TBL:-/gpfs/hps/nco/ops/nwprod/lib/g2tmpl/v1.3.0/src/params_grib2_tbl_new}     
export MODEL_OUT_FORM=binarynemsiompiio

$HOMEglobal/scripts/exgfs_nceppost.sh.ecf

#--rename master grib2 following parallel convention
export PGBOUT2_ops=$COMOUT/${RUN}.${cycle}.master.grb2f${post_times}
export PGBOUT2_ops_idx=$COMOUT/${RUN}.${cycle}.master.grb2if${post_times}
export PGBOUT2=$COMOUT/pgrbm${post_times}.gfs.${CDATE}.grib2
export PGBOUT2_idx=$COMOUT/pgrbm${post_times}.gfs.${CDATE}.grib2.idx
mv $PGBOUT2_ops $PGBOUT2
mv $PGBOUT2_ops_idx $PGBOUT2_idx

########################
# call down-stream jobs 
########################
#export APRUN_DWN="aprun -n 32 -N 8 -j 1 -d 3 cfp"
export APRUN_DWN=${APRUN_DWN:-""}
export GFSDOWNSH=${GFSDOWNSH:-$BASEDIR/ush/gfs_downstream_nems.sh}
export GFSDWNSH=${GFSDWNSH:-$BASEDIR/ush/gfs_dwn_nems.sh}
export PARM_SIB=${PARM_SIB:-$BASE_GSM/parm}

export FH=`expr $post_times + 0 `
if [ $FH -lt 10 ]; then export FH=0$FH; fi
if [ $post_times = anl ]; then export FH=-1 ; fi

$GFSDOWNSH
echo $?

sleep 10
rm -f ${DATA}/*
#----------------
done
#----------------

exit
