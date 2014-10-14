#!/bin/ksh

## Below are LoadLeveler (IBM queueing system) commands
#@ error=$(job_name).e$(jobid)
#@ job_type=parallel
#@ class=devhigh
#@ group=devonprod
#@ account_no = GDAS-T2O
#@ job_name=get_enkf_ic
#@ network.MPI=sn_all,shared,us
#@ tasks_per_node=1
#@ node = 1
#@ node_usage=not_shared
#@ task_affinity = core(32)
#@ parallel_threads = 32
#@ node_resources = ConsumableMemory (110 GB)
#@ wall_clock_limit = 3:00:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ restart=no
#@ queue

## Below are PBS (Linux queueing system) commands
#PBS -o get_enkf_ic.e${jobid}
#PBS -N get_enkf_ic
#PBS -q service
#PBS -l walltime=12:00:00
#PBS -l nodes=1:ppn=1
#PBS -j eo
#PBS -A ada
#PBS -V

 
set -ax

# Set user parameters
CDATE=2012061218      # date for which to get initial conditions (ic)
                      # For CDATE <  2012052100, get ensemble ic from operational GEFS
                      # For CDATE >= 2012052100, get ensemble ic from operational EnKF


machine=IBMP6         # platform (only two machine currently supported)
                      #  machine = IBMP6 for NCEP CCS (cirrus/stratus)
                      #  machine = ZEUS for NOAA R&D machine


SAVDIR=ensic.$CDATE   # name of directory to which ics are copied
                      # full path is OUTDIR=$PRETMP/ptmp/$LOGNAME/$SAVDIR


NMEM_ENKF=80          # number of ensemble members (do not change)


# Specific spectral / grid dimension to which to chgres ensemble members
#  NOTE:  This script does NOT run chgres for CDATE >= 2012052100
#     
JCAP_ENKF=254         # spectral truncation
LEVS_ENKF=64          # number of vertical levels
LONB_ENKF=768         # number of longitude points
LATB_ENKF=384         # number of gaussian latitude points


#---------- Users should not need to edit anything below this line ----------#

# Set paths
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
if [ $machine = IBMP6 ] ; then
  PRETMP=""
elif [ $machine = ZEUS ]; then
  PREPTMP=/scratch2/portfolios/NCEPDEV
else
  echo "***WARNING*** $machine not supported"
  exit 
fi
OUTDIR=$PRETMP/ptmp/$LOGNAME/$SAVDIR
TMPDIR=$PRETMP/stmp/$LOGNAME/get_enkf_ic.$CDATE

# Set script variables
export NCP=${NCP:-cp}

if [ $machine = IBMP6 ] ; then
  export MEMORY_AFFINITY=MCM
  export MP_SHARED_MEMORY=yes
  export XLFRTEOPTS="nlwidth=80"
  export MP_LABELIO=yes
  export MP_INFOLEVEL=1
  export CHGRESSH=${CHGRESSH:-/nwprod/ush/global_chgres.sh}
  export NDATE=${NDATE:-/nwprod/util/exec/ndate}
  export HTAR=${HTAR:-/usr/bin/htar}
elif [ $machine = ZEUS ]; then
  /bin/ksh --login
  module load intel
  module load mpt
  module load hpss
  export paradir="/scratch2/portfolios/NCEPDEV/global/save"
  export HOMEDIR=${HOMEDIR:-$paradir/Shrinivas.Moorthi/para}
  export EXECDIR=$HOMEDIR/exec
  export FIXGLOBAL=$HOMEDIR/fix/fix_am
  export USHDIR=$HOMEDIR/ush
  export CHGRESEXEC=$EXECDIR/global_chgres
  export CHGRESSH=$USHDIR/global_chgres_uf_gaea.sh
  export NDATE=${NDATE:-$UTILDIR/exec/ndate}
  export HTAR=${HTAR:-/apps/hpss/htar}
fi

# Set, create, cd to temporary work directory.  Make OUTDIR
rm -rf $TMPDIR
mkdir -p $TMPDIR
cd $TMPDIR

mkdir -p $OUTDIR


# Set date and GDAS/GEFS paths
sdate=`echo $CDATE | cut -c1-8`
hha=`echo $CDATE | cut -c9-10`

prodgdas=/com/gfs/prod/gdas.$sdate
comgdas=$prodgdas
PREFIX=gdas1.t${hha}z

prodenkf=/com/gfs/prod/enkf.$sdate/$hha
comenkf=$prodenkf

prodgefs=/com/gens/prod/gefs.${sdate}/${hha}
comgefs=$prodgefs


# If GDAS files are not online, retrieve from tape.
if [[ ! -s $prodgdas/$PREFIX.sanl || ! -s $prodgdas/$PREFIX.sfcanl ]]; then
   YYYY=`echo $CDATE | cut -c1-4`
   MM=`echo $CDATE | cut -c5-6`
   DD=`echo $CDATE | cut -c7-8`
   HH=`echo $CDATE | cut -c9-10`

   comgdas=$TMPDIR$prodgdas
   mkdir -p $comgdas
   cd $comgdas

   hpssfile=/NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_gdas.${YYYY}${MM}${DD}${HH}.tar
   $HTAR -xvf $hpssfile ./$PREFIX.sanl
   $HTAR -xvf $hpssfile ./$PREFIX.sfcanl
   rc=$?
   if [[ "$rc" != "0" ]]; then
      echo "***WARNING*** can not retrieve GDAS initial conditions for $CDATE"
      exit
   fi
fi

# Copy GDAS initial condtions to OUTDIR
comdir=$comgdas
$NCP $comdir/$PREFIX.sanl   $OUTDIR/siganl.gdas.$CDATE
$NCP $comdir/$PREFIX.sfcanl $OUTDIR/sfcanl.gdas.$CDATE


# If CDATE >= 2012052100, look for EnKF files.  If not online, retrieve from tape.
if [[ $CDATE -ge 2012052100 ]]; then
  if [[ ! -s $prodenkf/siganl_${CDATE}_mem001 || ! -s $prodenkf/sfcanl_${CDATE}_mem001 ]]; then
    YYYY=`echo $CDATE | cut -c1-4`
    MM=`echo $CDATE | cut -c5-6`
    DD=`echo $CDATE | cut -c7-8`
    HH=`echo $CDATE | cut -c9-10`

    comenkf=$TMPDIR$prodenkf
    mkdir -p $comenkf
    cd $comenkf

    hpssfile=/NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gfs_prod_enkf.${YYYY}${MM}${DD}_${HH}.anl.tar
    $HTAR -xvf $hpssfile 
    rc=$?
    if [[ "$rc" != "0" ]]; then
      echo "***WARNING*** can not retrieve GEFS initial conditions for $CDATE"
      exit
    fi
  fi

# Copy EnKF initial conditions to OUTDIR 
  comdir=$comenkf
  imem=1
  while [[ $imem -le $NMEM_ENKF ]]; do
    member="mem"`printf %03i $imem`
    $NCP $comdir/siganl_${CDATE}_$member $OUTDIR/
    $NCP $comdir/sfcanl_${CDATE}_$member $OUTDIR/
    (( imem = $imem + 1 ))
  done
fi


# If CDATE before operational EnKF, use GEFS.
if [[ $CDATE -lt 2012052100 ]]; then

# If GEFS files are not online, retrieve from tape
  if [[ ! -s $prodgefs/init/gec00.t${hha}z.sanl || ! -s $prodgefs/init/gec00.t${hha}z.sfcanl ]]; then
    YYYY=`echo $CDATE | cut -c1-4` 
    MM=`echo $CDATE | cut -c5-6`
    DD=`echo $CDATE | cut -c7-8`
    HH=`echo $CDATE | cut -c9-10`

    comgefs=$TMPDIR$prodgefs
    mkdir -p $comgefs
    cd $comgefs

    hpssfile=/NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYY}${MM}/${YYYY}${MM}${DD}/com_gens_prod_gefs.${YYYY}${MM}${DD}_${HH}.init.tar
    $HTAR -xvf $hpssfile
    rc=$?
    if [[ "$rc" != "0" ]]; then
      echo "***WARNING*** can not retrieve GEFS initial conditions for $CDATE"
      exit
    fi
  fi 

# Copy GEFS control files to TMPDIR
  comdir=$comgefs/init
  $NCP $comdir/gec00.t${hha}z.sanl   $TMPDIR/siganl_${CDATE}_control.gefs
  $NCP $comdir/gec00.t${hha}z.sfcanl $TMPDIR/sfcanl_${CDATE}_control.gefs

# Chgres GEFS control to EnKF resolution
  export SIGI=$TMPDIR/siganl_${CDATE}_control.gefs
  export SFCI=$TMPDIR/sfcanl_${CDATE}_control.gefs
  export SIGO=$TMPDIR/siganl_${CDATE}_control
  export SFCO=$TMPDIR/sfcanl_${CDATE}_control
  export SIGLEVEL=/nwprod/fix/global_hyblev.l64.txt
  export VERBOSE=YES
  export DATA=$TMPDIR/chgres_control

  export JCAP_ens=$JCAP_ENKF
  export LEVS_ens=$LEVS_ENKF
  export LONB_ens=$LONB_ENKF
  export LATB_ens=$LATB_ENKF
  export NTRAC=3
  export IDVC=2
  export IDSL=1
  export LSOIL=4
  export CHGRESVARS="IDVT=21,IDVM=0"
  export OUTTYP=2

  if [ $machine = IBMP6 ]; then
    export CHGRESTHREAD=32
  elif [ $machine = ZEUS ]; then
    export CHGRESTHREAD=1   # service queue can only use 1 core
  fi
  export OMP_NUM_THREADS=$CHGRESTHREAD
  export NTHREADS=$OMP_NUM_THREADS

  $CHGRESSH $SIGI $SFCI $SIGO $SFCO $JCAP_ens $LEVS_ens $LONB_ens $LATB_ens
  rc=$?
  if [[ "$rc" != "0" ]]; then
    echo "***WARNING*** chgres of GEFS initial conditions failed with return code $rc"
    exit
  fi

  export OMP_NUM_THREADS=1
  export NTHREADS=$OMP_NUM_THREADS

# Loop to replicate control surface analysis as ensemble member
# surface analyses.  This means initial surface analyses are
# identical.   Only the atmospheric files are perturbed.

  imem=1
  while [[ $imem -le $NMEM_ENKF ]]; do
    member="mem"`printf %03i $imem`
    $NCP $TMPDIR/sfcanl_${CDATE}_control $OUTDIR/sfcanl_${CDATE}_${member}
    (( imem = $imem + 1 ))
  done


# Loop to rename GEFS atmospheric initial conditions
  imem=1
  cd $comdir
  for file in `ls gep*sanl*`; do
    member="mem"`printf %03i $imem`
    $NCP $comdir/$file $TMPDIR/sanl_${CDATE}_${member}.gefs
    (( imem = $imem + 1 ))
  done

# Loop to chgres GEFS atmospheric initial conditions to EnKF resolution
  imem=1
  while [[ $imem -le $NMEM_ENKF ]]; do
    member="mem"`printf %03i $imem`
    export SIGI=$TMPDIR/sanl_${CDATE}_${member}.gefs
    export SFCI=/dev/null
    export SIGO=$TMPDIR/sanl_${CDATE}_${member}
    export SFCO=/dev/null
    export SIGLEVEL=/nwprod/fix/global_hyblev.l64.txt
    export VERBOSE=YES
    export DATA=$TMPDIR/chgres_${member}

    export JCAP_ens=$JCAP_ENKF
    export LEVS_ens=$LEVS_ENKF
    export LONB_ens=$LONB_ENKF
    export LATB_ens=$LATB_ENKF
    export NTRAC=3
    export IDVC=2
    export IDSL=1
    export LSOIL=4
    export CHGRESVARS="IDVT=21,IDVM=0"
    export OUTTYP=2

    if [ $machine = IBMP6 ]; then
      export CHGRESTHREAD=32
    elif [ $machine = ZEUS ]; then
      export CHGRESTHREAD=1   # service queue can only use 1 core
    fi
    export OMP_NUM_THREADS=$CHGRESTHREAD
    export NTHREADS=$OMP_NUM_THREADS

    $CHGRESSH $SIGI $SFCI $SIGO $SFCO $JCAP_ens $LEVS_ens $LONB_ens $LATB_ens
    rc=$?
    if [[ "$rc" != "0" ]]; then
      echo "***WARNING*** chgres of GEFS ensemble members failed with return code $rc"
      exit
    fi

    export OMP_NUM_THREADS=1
    export NTHREADS=$OMP_NUM_THREADS

#   Copy chgres'd GEFS to OUTDIR
    $NCP $SIGO  $OUTDIR/siganl_${CDATE}_${member}

    (( imem = $imem + 1 ))
  done
fi

exit

