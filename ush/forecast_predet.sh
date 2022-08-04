#! /usr/bin/env bash

#####
## "forecast_def.sh"
## This script sets value of all variables
##
## This is the child script of ex-global forecast,
## This script is a definition of functions.
#####

# For all non-evironment variables
# Cycling and forecast hour specific parameters
common_predet(){
  echo "SUB ${FUNCNAME[0]}: Defining variables for shared through models"
  pwd=$(pwd)
  machine=${machine:-"WCOSS2"}
  machine=$(echo $machine | tr '[a-z]' '[A-Z]')
  CASE=${CASE:-C768}
  CDATE=${CDATE:-2017032500}
  DATA=${DATA:-$pwd/fv3tmp$$}    # temporary running directory
  ROTDIR=${ROTDIR:-$pwd}         # rotating archive directory
  ICSDIR=${ICSDIR:-$pwd}         # cold start initial conditions
}

DATM_predet(){
  SYEAR=$(echo  $CDATE | cut -c1-4)
  SMONTH=$(echo $CDATE | cut -c5-6)
  SDAY=$(echo   $CDATE | cut -c7-8)
  SHOUR=$(echo  $CDATE | cut -c9-10)
  # directory set up
  if [ ! -d $DATA ]; then mkdir -p $DATA; fi
  if [ ! -d $DATA/DATM_INPUT ]; then mkdir -p $DATA/DATM_INPUT; fi
  FHMAX=${FHMAX:-9}
  # Go to Run Directory (DATA)
  cd $DATA
}

FV3_GFS_predet(){
  echo "SUB ${FUNCNAME[0]}: Defining variables for FV3GFS"
  CDUMP=${CDUMP:-gdas}
  CDUMPwave="${CDUMP}wave"
  FHMIN=${FHMIN:-0}
  FHMAX=${FHMAX:-9}
  FHOUT=${FHOUT:-3}
  FHZER=${FHZER:-6}
  FHCYC=${FHCYC:-24}
  FHMAX_HF=${FHMAX_HF:-0}
  FHOUT_HF=${FHOUT_HF:-1}
  NSOUT=${NSOUT:-"-1"}
  FDIAG=$FHOUT
  if [ $FHMAX_HF -gt 0 -a $FHOUT_HF -gt 0 ]; then FDIAG=$FHOUT_HF; fi
  WRITE_DOPOST=${WRITE_DOPOST:-".false."}
  restart_interval=${restart_interval:-0}
  rst_invt1=$(echo $restart_interval |cut -d " " -f 1)

  # Convert output settings into an explicit list
  OUTPUT_FH=""
  FHMIN_LF=$FHMIN
  if (( FHOUT_HF > 0 && FHMAX_HF > 0 )); then
    for (( fh = FHMIN; fh < FHMAX_HF; fh = fh + FHOUT_HF )); do
      OUTPUT_FH="$OUTPUT_FH $fh"
    done
    FHMIN_LF=$FHMAX_HF
  fi
  for (( fh = FHMIN_LF; fh <= FHMAX; fh = fh + FHOUT )); do
    OUTPUT_FH="$OUTPUT_FH $fh"
  done

  PDY=$(echo $CDATE | cut -c1-8)
  cyc=$(echo $CDATE | cut -c9-10)

  # Directories.
  pwd=$(pwd)
  NWPROD=${NWPROD:-${NWROOT:-$pwd}}
  HOMEgfs=${HOMEgfs:-$NWPROD}
  FIX_DIR=${FIX_DIR:-$HOMEgfs/fix}
  FIX_AM=${FIX_AM:-$FIX_DIR/fix_am}
  FIX_AER=${FIX_AER:-$FIX_DIR/fix_aer}
  FIX_LUT=${FIX_LUT:-$FIX_DIR/fix_lut}
  FIXfv3=${FIXfv3:-$FIX_DIR/fix_fv3_gmted2010}
  DATA=${DATA:-$pwd/fv3tmp$$}    # temporary running directory
  ROTDIR=${ROTDIR:-$pwd}         # rotating archive directory
  ICSDIR=${ICSDIR:-$pwd}         # cold start initial conditions
  DMPDIR=${DMPDIR:-$pwd}         # global dumps for seaice, snow and sst analysis

  # Model resolution specific parameters
  DELTIM=${DELTIM:-225}
  layout_x=${layout_x:-8}
  layout_y=${layout_y:-16}
  LEVS=${LEVS:-65}

  # Utilities
  NCP=${NCP:-"/bin/cp -p"}
  NLN=${NLN:-"/bin/ln -sf"}
  NMV=${NMV:-"/bin/mv"}
  SEND=${SEND:-"YES"}   #move final result to rotating directory
  ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
  KEEPDATA=${KEEPDATA:-"NO"}

  # Other options
  MEMBER=${MEMBER:-"-1"} # -1: control, 0: ensemble mean, >0: ensemble member $MEMBER
  ENS_NUM=${ENS_NUM:-1}  # Single executable runs multiple members (e.g. GEFS)
  PREFIX_ATMINC=${PREFIX_ATMINC:-""} # allow ensemble to use recentered increment

  # IAU options
  DOIAU=${DOIAU:-"NO"}
  IAUFHRS=${IAUFHRS:-0}
  IAU_DELTHRS=${IAU_DELTHRS:-0}
  IAU_OFFSET=${IAU_OFFSET:-0}

  # Model specific stuff
  FCSTEXECDIR=${FCSTEXECDIR:-$HOMEgfs/exec}
  FCSTEXEC=${FCSTEXEC:-ufs_model.x}
  PARM_FV3DIAG=${PARM_FV3DIAG:-$HOMEgfs/parm/parm_fv3diag}
  PARM_POST=${PARM_POST:-$HOMEgfs/parm/post}

  # Model config options
  APRUN_FV3=${APRUN_FV3:-${APRUN_FCST:-${APRUN:-""}}}
  #the following NTHREAD_FV3 line is commented out because NTHREAD_FCST is not defined
  #and because NTHREADS_FV3 gets overwritten by what is in the env/${macine}.env
  #file and the value of npe_node_fcst is not correctly defined when using more than
  #one thread and sets NTHREADS_FV3=1 even when the number of threads is appropraitely >1
  #NTHREADS_FV3=${NTHREADS_FV3:-${NTHREADS_FCST:-${nth_fv3:-1}}}
  cores_per_node=${cores_per_node:-${npe_node_fcst:-24}}
  ntiles=${ntiles:-6}
  if [ $MEMBER -lt 0 ]; then
    NTASKS_TOT=${NTASKS_TOT:-${npe_fcst_gfs:-0}}
  else
    NTASKS_TOT=${NTASKS_TOT:-${npe_efcs:-0}}
  fi

  TYPE=${TYPE:-"nh"}                  # choices:  nh, hydro
  MONO=${MONO:-"non-mono"}            # choices:  mono, non-mono

  QUILTING=${QUILTING:-".true."}
  OUTPUT_GRID=${OUTPUT_GRID:-"gaussian_grid"}
  OUTPUT_FILE=${OUTPUT_FILE:-"nemsio"}
  WRITE_NEMSIOFLIP=${WRITE_NEMSIOFLIP:-".true."}
  WRITE_FSYNCFLAG=${WRITE_FSYNCFLAG:-".true."}
  affix="nemsio"
  [[ "$OUTPUT_FILE" = "netcdf" ]] && affix="nc"

  rCDUMP=${rCDUMP:-$CDUMP}

  #-------------------------------------------------------
  if [ ! -d $ROTDIR ]; then mkdir -p $ROTDIR; fi
  mkdata=NO
  if [ ! -d $DATA ]; then
    mkdata=YES
    mkdir -p $DATA ;
  fi
  cd $DATA || exit 8
  mkdir -p $DATA/INPUT

  #------------------------------------------------------------------
  # changeable parameters
  # dycore definitions
  res=$(echo $CASE |cut -c2-5)
  resp=$((res+1))
  npx=$resp
  npy=$resp
  npz=$((LEVS-1))
  io_layout="1,1"
  #ncols=$(( (${npx}-1)*(${npy}-1)*3/2 ))

  # spectral truncation and regular grid resolution based on FV3 resolution
  JCAP_CASE=$((2*res-2))
  LONB_CASE=$((4*res))
  LATB_CASE=$((2*res))

  JCAP=${JCAP:-$JCAP_CASE}
  LONB=${LONB:-$LONB_CASE}
  LATB=${LATB:-$LATB_CASE}

  LONB_IMO=${LONB_IMO:-$LONB_CASE}
  LATB_JMO=${LATB_JMO:-$LATB_CASE}

  # NSST Options
  # nstf_name contains the NSST related parameters
  # nstf_name(1) : NST_MODEL (NSST Model) : 0 = OFF, 1 = ON but uncoupled, 2 = ON and coupled
  # nstf_name(2) : NST_SPINUP : 0 = OFF, 1 = ON,
  # nstf_name(3) : NST_RESV (Reserved, NSST Analysis) : 0 = OFF, 1 = ON
  # nstf_name(4) : ZSEA1 (in mm) : 0
  # nstf_name(5) : ZSEA2 (in mm) : 0
  # nst_anl      : .true. or .false., NSST analysis over lake
  NST_MODEL=${NST_MODEL:-0}
  NST_SPINUP=${NST_SPINUP:-0}
  NST_RESV=${NST_RESV-0}
  ZSEA1=${ZSEA1:-0}
  ZSEA2=${ZSEA2:-0}
  nstf_name=${nstf_name:-"$NST_MODEL,$NST_SPINUP,$NST_RESV,$ZSEA1,$ZSEA2"}
  nst_anl=${nst_anl:-".false."}


  # blocking factor used for threading and general physics performance
  #nyblocks=$(expr \( $npy - 1 \) \/ $layout_y )
  #nxblocks=$(expr \( $npx - 1 \) \/ $layout_x \/ 32)
  #if [ $nxblocks -le 0 ]; then nxblocks=1 ; fi
  blocksize=${blocksize:-32}

  # variables for controlling initialization of NCEP/NGGPS ICs
  filtered_terrain=${filtered_terrain:-".true."}
  gfs_dwinds=${gfs_dwinds:-".true."}

  # various debug options
  no_dycore=${no_dycore:-".false."}
  dycore_only=${adiabatic:-".false."}
  chksum_debug=${chksum_debug:-".false."}
  print_freq=${print_freq:-6}

  #-------------------------------------------------------
  if [ $CDUMP = "gfs" -a $rst_invt1 -gt 0 ]; then
    RSTDIR_ATM=${RSTDIR:-$ROTDIR}/${CDUMP}.${PDY}/${cyc}/atmos/RERUN_RESTART
    if [ ! -d $RSTDIR_ATM ]; then mkdir -p $RSTDIR_ATM ; fi
    $NLN $RSTDIR_ATM RESTART
    # The final restart written at the end doesn't include the valid date
    # Create links that keep the same name pattern for these files
    VDATE=$($NDATE +$FHMAX_GFS $CDATE)
    vPDY=$(echo $VDATE | cut -c1-8)
    vcyc=$(echo $VDATE | cut -c9-10)
    files="coupler.res fv_core.res.nc"
    for tile in {1..6}; do
      for base in ca_data fv_core.res fv_srf_wnd.res fv_tracer.res phy_data sfc_data; do
        files="${files} ${base}.tile${tile}.nc"
      done
    done
    for file in $files; do
      $NLN $RSTDIR_ATM/$file $RSTDIR_ATM/${vPDY}.${vcyc}0000.$file
    done
  else
    mkdir -p $DATA/RESTART
  fi

  #-------------------------------------------------------
  # member directory
  if [ $MEMBER -lt 0 ]; then
    prefix=$CDUMP
    rprefix=$rCDUMP
    memchar=""
  else
    prefix=enkf$CDUMP
    rprefix=enkf$rCDUMP
    memchar=mem$(printf %03i $MEMBER)
  fi
  memdir=$ROTDIR/${prefix}.$PDY/$cyc/atmos/$memchar
  if [ ! -d $memdir ]; then mkdir -p $memdir; fi

  GDATE=$($NDATE -$assim_freq $CDATE)
  gPDY=$(echo $GDATE | cut -c1-8)
  gcyc=$(echo $GDATE | cut -c9-10)
  gmemdir=$ROTDIR/${rprefix}.$gPDY/$gcyc/atmos/$memchar
  sCDATE=$($NDATE -3 $CDATE)

  if [[ "$DOIAU" = "YES" ]]; then
    sCDATE=$($NDATE -3 $CDATE)
    sPDY=$(echo $sCDATE | cut -c1-8)
    scyc=$(echo $sCDATE | cut -c9-10)
    tPDY=$gPDY
    tcyc=$gcyc
  else
    sCDATE=$CDATE
    sPDY=$PDY
    scyc=$cyc
    tPDY=$sPDY
    tcyc=$cyc
  fi

  echo "SUB ${FUNCNAME[0]}: pre-determination variables set"
}

WW3_predet(){
  echo "SUB ${FUNCNAME[0]}: Defining variables for WW3"
  if [ $CDUMP = "gdas" ]; then
    export RSTDIR_WAVE=$ROTDIR/${CDUMP}.${PDY}/${cyc}/wave/restart
  else
    export RSTDIR_WAVE=${RSTDIR_WAVE:-$ROTDIR/${CDUMP}.${PDY}/${cyc}/wave/restart}
  fi
  if [ ! -d $RSTDIR_WAVE ]; then mkdir -p $RSTDIR_WAVE ; fi
  $NLN $RSTDIR_WAVE restart_wave
}

CICE_predet(){
  echo "SUB ${FUNCNAME[0]}: CICE before run type determination"
  if [ ! -d $ROTDIR ]; then mkdir -p $ROTDIR; fi
  if [ ! -d $DATA ]; then mkdir -p $DATA; fi
  if [ ! -d $DATA/RESTART ]; then mkdir -p $DATA/RESTART; fi
  if [ ! -d $DATA/INPUT ]; then mkdir -p $DATA/INPUT; fi
  if [ ! -d $DATA/restart ]; then mkdir -p $DATA/restart; fi
  if [ ! -d $DATA/history ]; then mkdir -p $DATA/history; fi
  if [ ! -d $DATA/OUTPUT ]; then mkdir -p $DATA/OUTPUT; fi
}

MOM6_predet(){
  echo "SUB ${FUNCNAME[0]}: MOM6 before run type determination"
  if [ ! -d $ROTDIR ]; then mkdir -p $ROTDIR; fi
  if [ ! -d $DATA ]; then mkdir -p $DATA; fi
  if [ ! -d $DATA/RESTART ]; then mkdir -p $DATA/RESTART; fi
  if [ ! -d $DATA/INPUT ]; then mkdir -p $DATA/INPUT; fi
  if [ ! -d $DATA/restart ]; then mkdir -p $DATA/restart; fi
  if [ ! -d $DATA/history ]; then mkdir -p $DATA/history; fi
  if [ ! -d $DATA/OUTPUT ]; then mkdir -p $DATA/OUTPUT; fi
  if [ ! -d $DATA/MOM6_OUTPUT ]; then mkdir -p $DATA/MOM6_OUTPUT; fi
  if [ ! -d $DATA/MOM6_RESTART ]; then mkdir -p $DATA/MOM6_RESTART; fi
  cd $DATA || exit 8
}
