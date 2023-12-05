#! /usr/bin/env bash

#####
## "forecast_predet.sh"
## This script sets value of all variables
##
## This is the child script of ex-global forecast,
## This script is a definition of functions.
#####

# For all non-evironment variables
# Cycling and forecast hour specific parameters

to_seconds() {
  # Function to convert HHMMSS to seconds since 00Z
  local hhmmss=${1:?}
  local hh=${hhmmss:0:2}
  local mm=${hhmmss:2:2}
  local ss=${hhmmss:4:2}
  local seconds=$((10#${hh}*3600+10#${mm}*60+10#${ss}))
  local padded_seconds=$(printf "%05d" "${seconds}")
  echo "${padded_seconds}"
}

middle_date(){
  # Function to calculate mid-point date in YYYYMMDDHH between two dates also in YYYYMMDDHH
  local date1=${1:?}
  local date2=${2:?}
  local date1s=$(date --utc -d "${date1:0:8} ${date1:8:2}:00:00" +%s)
  local date2s=$(date --utc -d "${date2:0:8} ${date2:8:2}:00:00" +%s)
  local dtsecsby2=$(( $((date2s - date1s)) / 2 ))
  local mid_date=$(date --utc -d "${date1:0:8} ${date1:8:2} + ${dtsecsby2} seconds" +%Y%m%d%H%M%S)
  echo "${mid_date:0:10}"
}

nhour(){
  # Function to calculate hours between two dates (This replicates prod-util NHOUR)
  local date1=${1:?}
  local date2=${2:?}
  # Convert dates to UNIX timestamps
  seconds1=$(date --utc -d "${date1:0:8} ${date1:8:2}:00:00" +%s)
  seconds2=$(date --utc -d "${date2:0:8} ${date2:8:2}:00:00" +%s)
  hours=$(( $((seconds1 - seconds2)) / 3600 ))  # Calculate the difference in seconds and convert to hours
  echo "${hours}"
}

common_predet(){
  echo "SUB ${FUNCNAME[0]}: Defining variables for shared through model components"
  # Ignore "not used" warning
  # shellcheck disable=SC2034
  pwd=$(pwd)
  CDUMP=${CDUMP:-gdas}
  CASE=${CASE:-C768}
  CDATE=${CDATE:-2017032500}
  ENSMEM=${ENSMEM:-000}

  FCSTEXECDIR=${FCSTEXECDIR:-${HOMEgfs}/exec}
  FCSTEXEC=${FCSTEXEC:-ufs_model.x}

  # Directories.
  FIX_DIR=${FIX_DIR:-${HOMEgfs}/fix}

  # Model specific stuff
  PARM_POST=${PARM_POST:-${HOMEgfs}/parm/post}

  # Define significant cycles
  current_cycle=${CDATE}
  previous_cycle=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} - ${assim_freq} hours" +%Y%m%d%H)
  # ignore errors that variable isn't used
  # shellcheck disable=SC2034
  next_cycle=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${assim_freq} hours" +%Y%m%d%H)
  forecast_end_cycle=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${FHMAX} hours" +%Y%m%d%H)

  # IAU options
  IAU_OFFSET=${IAU_OFFSET:-0}
  DOIAU=${DOIAU:-"NO"}
  if [[ "${DOIAU}" = "YES" ]]; then
    sCDATE=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} - 3 hours" +%Y%m%d%H)
    sPDY="${sCDATE:0:8}"
    scyc="${sCDATE:8:2}"
    tPDY=${previous_cycle:0:8}
    tcyc=${previous_cycle:8:2}
  else
    sCDATE=${current_cycle}
    sPDY=${current_cycle:0:8}
    scyc=${current_cycle:8:2}
    tPDY=${sPDY}
    tcyc=${scyc}
  fi

  mkdir -p "${COM_CONF}"
  cd "${DATA}" || ( echo "FATAL ERROR: Unable to 'cd ${DATA}', ABORT!"; exit 8 )
}

FV3_predet(){
  echo "SUB ${FUNCNAME[0]}: Defining variables for FV3"
  FHMIN=${FHMIN:-0}
  FHMAX=${FHMAX:-9}
  FHOUT=${FHOUT:-3}
  FHZER=${FHZER:-6}
  FHCYC=${FHCYC:-24}
  FHMAX_HF=${FHMAX_HF:-0}
  FHOUT_HF=${FHOUT_HF:-1}
  NSOUT=${NSOUT:-"-1"}
  FDIAG=${FHOUT}
  if (( FHMAX_HF > 0 && FHOUT_HF > 0 )); then FDIAG=${FHOUT_HF}; fi
  WRITE_DOPOST=${WRITE_DOPOST:-".false."}
  restart_interval=${restart_interval:-${FHMAX}}
  # restart_interval = 0 implies write restart at the END of the forecast i.e. at FHMAX
  if [[ ${restart_interval} -eq 0 ]]; then
    restart_interval=${FHMAX}
  fi

  # Convert output settings into an explicit list for FV3
  # NOTE:  FV3_OUTPUT_FH is also currently used in other components
  # TODO: Have a seperate control for other components to address issue #1629
  FV3_OUTPUT_FH=""
  local fhr=${FHMIN}
  if (( FHOUT_HF > 0 && FHMAX_HF > 0 )); then
    for (( fh = FHMIN; fh < FHMAX_HF; fh = fh + FHOUT_HF )); do
      FV3_OUTPUT_FH="${FV3_OUTPUT_FH} ${fh}"
    done
    fhr=${FHMAX_HF}
  fi
  for (( fh = fhr; fh <= FHMAX; fh = fh + FHOUT )); do
    FV3_OUTPUT_FH="${FV3_OUTPUT_FH} ${fh}"
  done


  # Model resolution specific parameters
  DELTIM=${DELTIM:-225}
  layout_x=${layout_x:-8}
  layout_y=${layout_y:-16}
  LEVS=${LEVS:-65}

  # Other options
  MEMBER=${MEMBER:-"-1"} # -1: control, 0: ensemble mean, >0: ensemble member $MEMBER
  ENS_NUM=${ENS_NUM:-1}  # Single executable runs multiple members (e.g. GEFS)
  PREFIX_ATMINC=${PREFIX_ATMINC:-""} # allow ensemble to use recentered increment

  # IAU options
  IAUFHRS=${IAUFHRS:-0}
  IAU_DELTHRS=${IAU_DELTHRS:-0}

  # Model config options
  ntiles=6

  TYPE=${TYPE:-"nh"}                  # choices:  nh, hydro
  MONO=${MONO:-"non-mono"}            # choices:  mono, non-mono

  QUILTING=${QUILTING:-".true."}
  OUTPUT_GRID=${OUTPUT_GRID:-"gaussian_grid"}
  WRITE_NEMSIOFLIP=${WRITE_NEMSIOFLIP:-".true."}
  WRITE_FSYNCFLAG=${WRITE_FSYNCFLAG:-".true."}

  rCDUMP=${rCDUMP:-${CDUMP}}

  mkdir -p "${DATA}/INPUT"

  #------------------------------------------------------------------
  # changeable parameters
  # dycore definitions
  res="${CASE:1}"
  resp=$((res+1))
  npx=${resp}
  npy=${resp}
  npz=$((LEVS-1))
  io_layout="1,1"
  #ncols=$(( (${npx}-1)*(${npy}-1)*3/2 ))

  # spectral truncation and regular grid resolution based on FV3 resolution
  JCAP_CASE=$((2*res-2))
  LONB_CASE=$((4*res))
  LATB_CASE=$((2*res))

  JCAP=${JCAP:-${JCAP_CASE}}
  LONB=${LONB:-${LONB_CASE}}
  LATB=${LATB:-${LATB_CASE}}

  LONB_IMO=${LONB_IMO:-${LONB_CASE}}
  LATB_JMO=${LATB_JMO:-${LATB_CASE}}

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
  nstf_name=${nstf_name:-"${NST_MODEL},${NST_SPINUP},${NST_RESV},${ZSEA1},${ZSEA2}"}
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
  if [[ ${RUN} =~ "gfs" || ${RUN} = "gefs" ]]; then
    if [[ ! -d ${COM_ATMOS_RESTART} ]]; then mkdir -p "${COM_ATMOS_RESTART}" ; fi
    ${NLN} "${COM_ATMOS_RESTART}" RESTART
    # The final restart written at the end doesn't include the valid date
    # Create links that keep the same name pattern for these files
    files="coupler.res fv_core.res.nc"
    for n in $(seq 1 "${ntiles}"); do
      for base in ca_data fv_core.res fv_srf_wnd.res fv_tracer.res phy_data sfc_data; do
        files="${files} ${base}.tile${n}.nc"
      done
    done
    for file in ${files}; do
      ${NLN} "${file}" "${COM_ATMOS_RESTART}/${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.${file}"
    done
  else
    mkdir -p "${DATA}/RESTART"
  fi

  echo "SUB ${FUNCNAME[0]}: pre-determination variables set"
}

WW3_predet(){
  echo "SUB ${FUNCNAME[0]}: WW3 before run type determination"
  if [[ ! -d "${COM_WAVE_RESTART}" ]]; then mkdir -p "${COM_WAVE_RESTART}" ; fi
  ${NLN} "${COM_WAVE_RESTART}" "restart_wave"
}

CICE_predet(){
  echo "SUB ${FUNCNAME[0]}: CICE before run type determination"
  if [[ ! -d "${DATA}/CICE_OUTPUT" ]]; then  mkdir -p "${DATA}/CICE_OUTPUT"; fi
  if [[ ! -d "${DATA}/CICE_RESTART" ]]; then mkdir -p "${DATA}/CICE_RESTART"; fi
}

MOM6_predet(){
  echo "SUB ${FUNCNAME[0]}: MOM6 before run type determination"
  if [[ ! -d "${DATA}/MOM6_OUTPUT" ]]; then mkdir -p "${DATA}/MOM6_OUTPUT"; fi
  if [[ ! -d "${DATA}/MOM6_RESTART" ]]; then mkdir -p "${DATA}/MOM6_RESTART"; fi
}
