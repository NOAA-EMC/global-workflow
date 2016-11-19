#!/bin/sh
#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -e /gpfs/hps/ptmp/Fanglin.Yang/fv3/log.nceppost                             
#BSUB -o /gpfs/hps/ptmp/Fanglin.Yang/fv3/log.nceppost                             
#BSUB -J nceppost                  
#BSUB -q dev     
#BSUB -M 3072
#BSUB -W 10:00
#BSUB -cwd /gpfs/hps/ptmp/Fanglin.Yang/fv3
#BSUB -extsched 'CRAYLINUX[]' 
set -x

export machine=WCOSS_C
 . $MODULESHOME/init/sh
module load PrgEnv-intel
module load cray-mpich 
module load prod_util    
module load prod_envir   
module load grib_util/1.0.3
module load cfp-intel-sandybridge
module load crtm-intel/2.2.4
module load g2tmpl-intel/1.4.0

export KMP_AFFINITY=disabled
export OMP_STACKSIZE=2048M
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered
#--------------------------------------------------------------------

export CDATE=${CDATE:-2016100300}
export CDUMP=${CDUMP:-gfs}
export CASE=${CASE:-C768}         ;#C48 C96 C192 C384 C768 C1152 C3072
export NFCST=${NFCST:-40}         ;#number of forecatsts included in nemsio file
export GG=${GG:-0p25deg}          ;#1deg 0p5deg 0p25deg 0p125deg

export PTMP=${PTMP:-/gpfs/hps/ptmp}
export input_dir=${input_dir:-$PTMP/$LOGNAME/fv3/nemsio}
export output_dir=${output_dir:-$PTMP/$LOGNAME/fv3/grib}
export home_dir=${home_dir:-/gpfs/hps/emc/global/noscrub/$LOGNAME/NGGPS}
if [ ! -s $output_dir ]; then mkdir -p $output_dir ;fi

export NODES=4
export max_core=${max_core:-24}
export thread=${thread:-1}
#export npe_node=$((max_core/thread))
export npe_node=8                     
export npes=$((NODES*npe_node))
export APRUN="aprun -n $npes -N $npe_node -j 1 -d $thread -cc depth"
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

export DATA=$PTMP/$LOGNAME/$NET/post/${CASE}_${CDATE}
mkdir -p $DATA
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
export ver=v14.1.0
export q3fy17=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17
export nemsioget=$q3fy17/global_shared.$ver/exec/nemsio_get
#export HOMEglobal=${HOMEglobal:-$q3fy17/global_shared.$ver}
export HOMEglobal=${HOMEglobal:-/gpfs/hps/emc/global/noscrub/Fanglin.Yang/NGGPS/post/trunk}                     
export EXECglobal=$HOMEglobal/exec
export USHglobal=$HOMEglobal/ush
export USHgfs=$HOMEglobal/ush
export FIXglobal=$HOMEglobal/fix
export PARMglobal=$HOMEglobal/parm
export NWROOTprod=$NWROOT
export ens=NO
export COMIN=$DATA
export COMOUT=$output_dir


export SHOUR=${SHOUR:-006}
export FHOUR=${FHOUR:-240}
export FHINC=${FHINC:-06}

export post_times=$SHOUR
#-----------------------------------
while [ $post_times -le $FHOUR ]; do
#-----------------------------------

####################################
# Specify Special Post Vars
####################################
export IGEN_ANL=81
export IGEN_FCST=96

export POSTGPVARS="KPO=47,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,"

#######################################
# Specify Restart File Name to Key Off
#######################################
# export restart_file=${COMIN}/${RUN}.t${cyc}z.logf
# export restart_file=${COMIN}/${RUN}.t${cyc}z.sf 
  export restart_file=$input_dir/${CASE}_nemsio${GG}.${PDY}${cyc}_FHR

 ln -fs $input_dir/${CASE}_nemsio${GG}.${PDY}${cyc}_FHR${post_times}  $COMIN/${RUN}.t${cyc}z.atmf${post_times}.nemsio 
 ln -fs $input_dir/${CASE}_nemsio${GG}.${PDY}${cyc}_FHR${post_times}  $COMIN/${RUN}.t${cyc}z.flxf${post_times}.nemsio
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

export POSTGPEXEC=$HOMEglobal/exec/ncep_post
export POSTGPSH=$HOMEglobal/ush/global_nceppost.sh

export POSTGRB2TBL=/gpfs/hps/nco/ops/nwprod/lib/g2tmpl/v1.3.0/src/params_grib2_tbl_new     
export MODEL_OUT_FORM=binarynemsiompiio

$HOMEglobal/scripts/exgfs_nceppost.sh.ecf

########################
# call down-stream jobs 
########################
export nknd=1
export tasksp_1=32
export APRUN_DWN='aprun -j 1 -n 24 -N 8 -d 3 cfp'
export downset=1
export GFSDOWNSH=$q3fy17/gfs_workflow.$ver/para/ush/gfs_downstream_nems.sh
export GFSDWNSH=$q3fy17/gfs_workflow.$ver/para/ush/gfs_dwn_nems.sh
export PARM_SIB=$q3fy17/global_shared.$ver/parm
export PGBOUT2=$COMOUT/${RUN}.${cycle}.master.grb2f${post_times}
export FH=`expr $post_times + 0 `
if [ $FH -lt 10 ]; then export FH=0$FH; fi
if [ $post_times = anl ]; then export FH=-1 ; fi

$GFSDOWNSH
echo $?

export post_times=`expr $post_times +  $FHINC`
if [ $post_times -le 99 ]; then
 export post_times=0${post_times}
fi

#----------------
done
#----------------

exit
