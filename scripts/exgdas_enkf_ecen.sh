#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_enkf_ecen.sh
# Script description:  recenter ensemble around hi-res deterministic analysis
#
# Author:        Rahul Mahajan      Org: NCEP/EMC     Date: 2017-03-02
#
# Abstract: This script recenters ensemble around hi-res deterministic analysis
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

source "${USHgfs}/preamble.sh"

# Directories.
pwd=$(pwd)

# Base variables
CDATE=${CDATE:-"2010010100"}
DONST=${DONST:-"NO"}
export CASE=${CASE:-384}
ntiles=${ntiles:-6}

# Utilities
NCLEN=${NCLEN:-${USHgfs}/getncdimlen}

# Scripts

# Executables.
GETATMENSMEANEXEC=${GETATMENSMEANEXEC:-${EXECgfs}/getsigensmeanp_smooth.x}
GETSFCENSMEANEXEC=${GETSFCENSMEANEXEC:-${EXECgfs}/getsfcensmeanp.x}
RECENATMEXEC=${RECENATMEXEC:-${EXECgfs}/recentersigp.x}
CALCINCNEMSEXEC=${CALCINCNEMSEXEC:-${EXECgfs}/calc_increment_ens.x}
CALCINCNCEXEC=${CALCINCEXEC:-${EXECgfs}/calc_increment_ens_ncio.x}

# Files.
OPREFIX=${OPREFIX:-""}
OSUFFIX=${OSUFFIX:-""}
APREFIX=${APREFIX:-""}
APREFIX_ENS=${APREFIX_ENS:-$APREFIX}
GPREFIX=${GPREFIX:-""}
GPREFIX_ENS=${GPREFIX_ENS:-$GPREFIX}

# Variables
imp_physics=${imp_physics:-99}
INCREMENTS_TO_ZERO=${INCREMENTS_TO_ZERO:-"'NONE'"}
DOIAU=${DOIAU_ENKF:-"NO"}
FHMIN=${FHMIN_ECEN:-3}
FHMAX=${FHMAX_ECEN:-9}
FHOUT=${FHOUT_ECEN:-3}
FHSFC=${FHSFC_ECEN:-$FHMIN}
NMEM_ENS_MAX=${NMEM_ENS:-80}
if [ "${RUN}" = "enkfgfs" ]; then
   DO_CALC_INCREMENT=${DO_CALC_INCREMENT_ENKF_GFS:-"NO"}
   NMEM_ENS=${NMEM_ENS_GFS:-30}
   ec_offset=${NMEM_ENS_GFS_OFFSET:-20}
   mem_offset=$((ec_offset * cyc/6))
else
   DO_CALC_INCREMENT=${DO_CALC_INCREMENT:-"NO"}
   NMEM_ENS=${NMEM_ENS:-80}
   mem_offset=0
fi

# global_chgres stuff
CHGRESNEMS=${CHGRESNEMS:-${EXECgfs}/enkf_chgres_recenter.x}
CHGRESNC=${CHGRESNC:-${EXECgfs}/enkf_chgres_recenter_nc.x}
NTHREADS_CHGRES=${NTHREADS_CHGRES:-24}
APRUN_CHGRES=${APRUN_CHGRES:-""}

# global_cycle stuff
CYCLESH=${CYCLESH:-${USHgfs}/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-${EXECgfs}/global_cycle}
APRUN_CYCLE=${APRUN_CYCLE:-${APRUN:-""}}
NTHREADS_CYCLE=${NTHREADS_CYCLE:-${NTHREADS:-1}}
export CYCLVARS=${CYCLVARS:-"FSNOL=-2.,FSNOS=99999.,"}
export FHOUR=${FHOUR:-0}
export DELTSFC=${DELTSFC:-6}


RECENTER_ENKF=${RECENTER_ENKF:-"YES"}
SMOOTH_ENKF=${SMOOTH_ENKF:-"YES"}

APRUN_ECEN=${APRUN_ECEN:-${APRUN:-""}}
NTHREADS_ECEN=${NTHREADS_ECEN:-${NTHREADS:-1}}
APRUN_CALCINC=${APRUN_CALCINC:-${APRUN:-""}}
NTHREADS_CALCINC=${NTHREADS_CALCINC:-${NTHREADS:-1}}

################################################################################
# Preprocessing
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi
cd $DATA || exit 99

ENKF_SUFFIX="s"
[[ $SMOOTH_ENKF = "NO" ]] && ENKF_SUFFIX=""

################################################################################
# Link ensemble member guess, analysis and increment files
for FHR in $(seq $FHMIN $FHOUT $FHMAX); do

for imem in $(seq 1 $NMEM_ENS); do
   smem=$((imem + mem_offset))
   if (( smem > NMEM_ENS_MAX )); then
      smem=$((smem - NMEM_ENS_MAX))
   fi
   gmemchar="mem"$(printf %03i $smem)
   memchar="mem"$(printf %03i $imem)

   MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl -x \
      COMOUT_ATMOS_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

   MEMDIR=${gmemchar} RUN=${GDUMP_ENS} YMD=${gPDY} HH=${gcyc} declare_from_tmpl -x \
      COMIN_ATMOS_HISTORY_MEM_PREV:COM_ATMOS_HISTORY_TMPL

   ${NLN} "${COMIN_ATMOS_HISTORY_MEM_PREV}/${GPREFIX_ENS}atmf00${FHR}${ENKF_SUFFIX}.nc" "./atmges_${memchar}"
   if [ $DO_CALC_INCREMENT = "YES" ]; then
      if [ $FHR -eq 6 ]; then
         ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX_ENS}atmanl.nc" "./atmanl_${memchar}"
      else
         ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX_ENS}atma00${FHR}.nc" "./atmanl_${memchar}"
      fi
   fi
   mkdir -p "${COMOUT_ATMOS_ANALYSIS_MEM}"
   if [ $FHR -eq 6 ]; then
      ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX_ENS}atminc.nc" "./atminc_${memchar}"
   else
      ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX_ENS}atmi00${FHR}.nc" "./atminc_${memchar}"
   fi
   if [[ $RECENTER_ENKF = "YES" ]]; then
      if [ $DO_CALC_INCREMENT = "YES" ]; then
         if [ $FHR -eq 6 ]; then
            ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX_ENS}ratmanl.nc" "./ratmanl_${memchar}"
         else
            ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX_ENS}ratma00${FHR}.nc" "./ratmanl_${memchar}"
         fi
     else
         if [ $FHR -eq 6 ]; then
            ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX_ENS}ratminc.nc" "./ratminc_${memchar}"
         else
            ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX_ENS}ratmi00${FHR}.nc" "./ratminc_${memchar}"
         fi
     fi
   fi
done

if [ $DO_CALC_INCREMENT = "YES" ]; then
   # Link ensemble mean analysis
   if [ $FHR -eq 6 ]; then
      ${NLN} "${COMOUT_ATMOS_ANALYSIS_STAT}/${APREFIX_ENS}atmanl.ensmean.nc" "./atmanl_ensmean"
   else
      ${NLN} "${COMOUT_ATMOS_ANALYSIS_STAT}/${APREFIX_ENS}atma00${FHR}.ensmean.nc" "./atmanl_ensmean"
   fi

   # Compute ensemble mean analysis
   DATAPATH="./"
   ATMANLNAME="atmanl"
   ATMANLMEANNAME="atmanl_ensmean"

   export OMP_NUM_THREADS=$NTHREADS_ECEN
   export pgm=$GETATMENSMEANEXEC
   . prep_step

   $NCP $GETATMENSMEANEXEC $DATA
   $APRUN_ECEN ${DATA}/$(basename $GETATMENSMEANEXEC) $DATAPATH $ATMANLMEANNAME $ATMANLNAME $NMEM_ENS
   export err=$?; err_chk
else
   # Link ensemble mean increment
   if [ $FHR -eq 6 ]; then
      ${NLN} "${COMOUT_ATMOS_ANALYSIS_STAT}/${APREFIX_ENS}atminc.ensmean.nc" "./atminc_ensmean"
   else
      ${NLN} "${COMOUT_ATMOS_ANALYSIS_STAT}/${APREFIX_ENS}atmi00${FHR}.ensmean.nc" "./atminc_ensmean"
   fi

   # Compute ensemble mean increment
   DATAPATH="./"
   ATMINCNAME="atminc"
   ATMINCMEANNAME="atminc_ensmean"

   export OMP_NUM_THREADS=$NTHREADS_ECEN
   export pgm=$GETATMENSMEANEXEC
   . prep_step

   $NCP $GETATMENSMEANEXEC $DATA
   $APRUN_ECEN ${DATA}/$(basename $GETATMENSMEANEXEC) $DATAPATH $ATMINCMEANNAME $ATMINCNAME $NMEM_ENS
   export err=$?; err_chk

   # If available, link to ensemble mean guess.  Otherwise, compute ensemble mean guess
   if [[ -s "${COMIN_ATMOS_HISTORY_STAT_PREV}/${GPREFIX_ENS}atmf00${FHR}.ensmean.nc" ]]; then
       ${NLN} "${COMIN_ATMOS_HISTORY_STAT_PREV}/${GPREFIX_ENS}atmf00${FHR}.ensmean.nc" "./atmges_ensmean"
   else
       DATAPATH="./"
       ATMGESNAME="atmges"
       ATMGESMEANNAME="atmges_ensmean"

       export OMP_NUM_THREADS=$NTHREADS_ECEN
       export pgm=$GETATMENSMEANEXEC
       . prep_step

       $NCP $GETATMENSMEANEXEC $DATA
       $APRUN_ECEN ${DATA}/$(basename $GETATMENSMEANEXEC) $DATAPATH $ATMGESMEANNAME $ATMGESNAME $NMEM_ENS
       export err=$?; err_chk
   fi
fi

if [ $DO_CALC_INCREMENT = "YES" ]; then
   LONB_ENKF=${LONB_ENKF:-$($NCLEN atmanl_ensmean grid_xt)} # get LONB
   LATB_ENKF=${LATB_ENKF:-$($NCLEN atmanl_ensmean grid_yt)} # get LATB
   LEVS_ENKF=${LEVS_ENKF:-$($NCLEN atmanl_ensmean pfull)} # get LEVS
else
   LONB_ENKF=${LONB_ENKF:-$($NCLEN atminc_ensmean lon)} # get LONB
   LATB_ENKF=${LATB_ENKF:-$($NCLEN atminc_ensmean lat)} # get LATB
   LEVS_ENKF=${LEVS_ENKF:-$($NCLEN atminc_ensmean lev)} # get LEVS
fi
JCAP_ENKF=${JCAP_ENKF:--9999} # there is no jcap in these files
[ $JCAP_ENKF -eq -9999 -a $LATB_ENKF -ne -9999 ] && JCAP_ENKF=$((LATB_ENKF-2))
[ $LONB_ENKF -eq -9999 -o $LATB_ENKF -eq -9999 -o $LEVS_ENKF -eq -9999 -o $JCAP_ENKF -eq -9999 ] && exit -9999

################################################################################
# This is to give the user the option to recenter, default is YES
if [ $RECENTER_ENKF = "YES" ]; then

   # GSI EnVar analysis
   if [ $FHR -eq 6 ]; then
     ATMANL_GSI="${COMIN_ATMOS_ANALYSIS_DET}/${APREFIX}atmanl.nc"
     ATMANL_GSI_ENSRES="${COMIN_ATMOS_ANALYSIS_DET}/${APREFIX}atmanl.ensres.nc"
   else
     ATMANL_GSI="${COMIN_ATMOS_ANALYSIS_DET}/${APREFIX}atma00${FHR}.nc"
     ATMANL_GSI_ENSRES="${COMIN_ATMOS_ANALYSIS_DET}/${APREFIX}atma00${FHR}.ensres.nc"
   fi

   # if we already have a ensemble resolution GSI analysis then just link to it
   if [ -f $ATMANL_GSI_ENSRES ]; then

      $NLN $ATMANL_GSI_ENSRES        atmanl_gsi_ensres

   else

      $NLN $ATMANL_GSI        atmanl_gsi
      $NLN $ATMANL_GSI_ENSRES atmanl_gsi_ensres
      SIGLEVEL=${SIGLEVEL:-${FIXgfs}/am/global_hyblev.l${LEVS}.txt}
      $NLN $CHGRESNC chgres.x
      chgresnml=chgres_nc_gauss.nml
      nmltitle=chgres

      export OMP_NUM_THREADS=$NTHREADS_CHGRES

      [[ -f $chgresnml ]] && rm -f $chgresnml
      cat > $chgresnml << EOF
&${nmltitle}_setup
  i_output=$LONB_ENKF
  j_output=$LATB_ENKF
  input_file="atmanl_gsi"
  output_file="atmanl_gsi_ensres"
  terrain_file="atmanl_ensmean"
  vcoord_file="$SIGLEVEL"
/
EOF
      cat $chgresnml
      $APRUN_CHGRES ./chgres.x
      export err=$?; err_chk
   fi

   if [ $DO_CALC_INCREMENT = "YES" ]; then
      ################################################################################
      # Recenter ensemble member atmospheric analyses about hires analysis

      FILENAMEIN="atmanl"
      FILENAME_MEANIN="atmanl_ensmean"     # EnKF ensemble mean analysis
      FILENAME_MEANOUT="atmanl_gsi_ensres" # recenter around GSI analysis at ensemble resolution
      FILENAMEOUT="ratmanl"

      export OMP_NUM_THREADS=$NTHREADS_ECEN
      export pgm=$RECENATMEXEC
      . prep_step

      $NCP $RECENATMEXEC $DATA
      $APRUN_ECEN ${DATA}/$(basename $RECENATMEXEC) $FILENAMEIN $FILENAME_MEANIN $FILENAME_MEANOUT $FILENAMEOUT $NMEM_ENS
      export err=$?; err_chk
   else
      ################################################################################
      # Recenter ensemble member atmospheric increments about hires analysis

      FILENAMEIN="atminc"
      FILENAME_INCMEANIN="atminc_ensmean"     # EnKF ensemble mean increment
      FILENAME_GESMEANIN="atmges_ensmean"     # EnKF ensemble mean guess
      FILENAME_GSIDET="atmanl_gsi_ensres" # recenter around GSI analysis at ensemble resolution
      FILENAMEOUT="ratminc"

      export OMP_NUM_THREADS=$NTHREADS_ECEN

      # make the small namelist file for incvars_to_zero

      [[ -f recenter.nml ]] && rm recenter.nml
      cat > recenter.nml << EOF
&recenter
  incvars_to_zero = $INCREMENTS_TO_ZERO
/
EOF
cat recenter.nml

      export pgm=$RECENATMEXEC
      . prep_step

      $NCP $RECENATMEXEC $DATA
      $APRUN_ECEN ${DATA}/$(basename $RECENATMEXEC) $FILENAMEIN $FILENAME_INCMEANIN $FILENAME_GSIDET $FILENAMEOUT $NMEM_ENS $FILENAME_GESMEANIN
      export err=$?; err_chk
   fi
fi

################################################################################
# Calculate ensemble analysis increment
if [ $DO_CALC_INCREMENT = "YES" ]; then
   if [ $RECENTER_ENKF = "YES" ]; then
       ATMANLNAME='ratmanl'
   else
       ATMANLNAME='atmanl'
   fi

   export OMP_NUM_THREADS=$NTHREADS_CALCINC
   CALCINCEXEC=$CALCINCNCEXEC

   export pgm=$CALCINCEXEC
   . prep_step

   $NCP $CALCINCEXEC $DATA
   [[ -f calc_increment.nml ]] && rm calc_increment.nml
   cat > calc_increment.nml << EOF
&setup
  datapath = './'
  analysis_filename = '$ATMANLNAME'
  firstguess_filename = 'atmges'
  increment_filename = 'atminc'
  debug = .false.
  nens = $NMEM_ENS
  imp_physics = $imp_physics
/
&zeroinc
  incvars_to_zero = $INCREMENTS_TO_ZERO
/
EOF
cat calc_increment.nml

   $APRUN_CALCINC ${DATA}/$(basename $CALCINCEXEC)
   export err=$?; err_chk
fi
done # loop over analysis times in window

################################################################################

################################################################################
# Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA


exit ${err}
