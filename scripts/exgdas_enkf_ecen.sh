#!/bin/bash
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
#   Machine: WCOSS-Cray/Theia
#
################################################################################

# Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

# Directories.
pwd=$(pwd)

# Base variables
CDATE=${CDATE:-"2010010100"}
DONST=${DONST:-"NO"}
export CASE=${CASE:-384}
ntiles=${ntiles:-6}

# Utilities
NCP=${NCP:-"/bin/cp -p"}
NLN=${NLN:-"/bin/ln -sf"}
NEMSIOGET=${NEMSIOGET:-${NWPROD}/exec/nemsio_get}
NCLEN=${NCLEN:-$HOMEgfs/ush/getncdimlen}

# Scripts

# Executables.
GETATMENSMEANEXEC=${GETATMENSMEANEXEC:-$HOMEgfs/exec/getsigensmeanp_smooth.x}
GETSFCENSMEANEXEC=${GETSFCENSMEANEXEC:-$HOMEgfs/exec/getsfcensmeanp.x}
RECENATMEXEC=${RECENATMEXEC:-$HOMEgfs/exec/recentersigp.x}
CALCINCNEMSEXEC=${CALCINCNEMSEXEC:-$HOMEgfs/exec/calc_increment_ens.x}
CALCINCNCEXEC=${CALCINCEXEC:-$HOMEgfs/exec/calc_increment_ens_ncio.x}

# Files.
OPREFIX=${OPREFIX:-""}
OSUFFIX=${OSUFFIX:-""}
APREFIX=${APREFIX:-""}
APREFIX_ENKF=${APREFIX_ENKF:-$APREFIX}
ASUFFIX=${ASUFFIX:-$SUFFIX}
GPREFIX=${GPREFIX:-""}
GSUFFIX=${GSUFFIX:-$SUFFIX}

# Variables
NMEM_ENKF=${NMEM_ENKF:-80}
imp_physics=${imp_physics:-99}
INCREMENTS_TO_ZERO=${INCREMENTS_TO_ZERO:-"'NONE'"}
DOIAU=${DOIAU_ENKF:-"NO"}
FHMIN=${FHMIN_ECEN:-3}
FHMAX=${FHMAX_ECEN:-9}
FHOUT=${FHOUT_ECEN:-3}
FHSFC=${FHSFC_ECEN:-$FHMIN}
DO_CALC_INCREMENT=${DO_CALC_INCREMENT:-"NO"}


# global_chgres stuff
CHGRESNEMS=${CHGRESNEMS:-$HOMEgfs/exec/enkf_chgres_recenter.x}
CHGRESNC=${CHGRESNC:-$HOMEgfs/exec/enkf_chgres_recenter_nc.x}
NTHREADS_CHGRES=${NTHREADS_CHGRES:-24}
APRUN_CHGRES=${APRUN_CHGRES:-""}

# global_cycle stuff
CYCLESH=${CYCLESH:-$HOMEgfs/ush/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-$HOMEgfs/exec/global_cycle}
APRUN_CYCLE=${APRUN_CYCLE:-${APRUN:-""}}
NTHREADS_CYCLE=${NTHREADS_CYCLE:-${NTHREADS:-1}}
export FIXfv3=${FIXfv3:-$HOMEgfs/fix/fix_fv3_gmted2010}
export FIXgsm=${FIXgsm:-$HOMEgfs/fix/fix_am}
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

for imem in $(seq 1 $NMEM_ENKF); do
   memchar="mem"$(printf %03i $imem)
   $NLN $COMIN_GES_ENS/$memchar/${GPREFIX}atmf00${FHR}${ENKF_SUFFIX}$GSUFFIX ./atmges_$memchar
   if [ $DO_CALC_INCREMENT = "YES" ]; then
      if [ $FHR -eq 6 ]; then
         $NLN $COMIN_ENS/$memchar/${APREFIX_ENKF}atmanl$ASUFFIX ./atmanl_$memchar
      else
         $NLN $COMIN_ENS/$memchar/${APREFIX_ENKF}atma00${FHR}$ASUFFIX ./atmanl_$memchar
      fi
   fi
   mkdir -p $COMOUT_ENS/$memchar
   if [ $FHR -eq 6 ]; then
      $NLN $COMOUT_ENS/$memchar/${APREFIX}atminc.nc ./atminc_$memchar
   else
      $NLN $COMOUT_ENS/$memchar/${APREFIX}atmi00${FHR}.nc ./atminc_$memchar
   fi
   if [[ $RECENTER_ENKF = "YES" ]]; then
      if [ $DO_CALC_INCREMENT = "YES" ]; then
         if [ $FHR -eq 6 ]; then
            $NLN $COMOUT_ENS/$memchar/${APREFIX}ratmanl$ASUFFIX ./ratmanl_$memchar
         else
            $NLN $COMOUT_ENS/$memchar/${APREFIX}ratma00${FHR}$ASUFFIX ./ratmanl_$memchar
         fi
     else
         if [ $FHR -eq 6 ]; then
            $NLN $COMOUT_ENS/$memchar/${APREFIX}ratminc$ASUFFIX ./ratminc_$memchar
         else
            $NLN $COMOUT_ENS/$memchar/${APREFIX}ratmi00${FHR}$ASUFFIX ./ratminc_$memchar
         fi
     fi
   fi
done

if [ $DO_CALC_INCREMENT = "YES" ]; then
   # Link ensemble mean analysis
   if [ $FHR -eq 6 ]; then
      $NLN $COMIN_ENS/${APREFIX_ENKF}atmanl.ensmean$ASUFFIX ./atmanl_ensmean
   else
      $NLN $COMIN_ENS/${APREFIX_ENKF}atma00${FHR}.ensmean$ASUFFIX ./atmanl_ensmean
   fi

   # Compute ensemble mean analysis
   DATAPATH="./"
   ATMANLNAME="atmanl"
   ATMANLMEANNAME="atmanl_ensmean"

   export OMP_NUM_THREADS=$NTHREADS_ECEN
   export pgm=$GETATMENSMEANEXEC
   . prep_step

   $NCP $GETATMENSMEANEXEC $DATA
   $APRUN_ECEN ${DATA}/$(basename $GETATMENSMEANEXEC) $DATAPATH $ATMANLMEANNAME $ATMANLNAME $NMEM_ENKF
   export err=$?; err_chk
else
   # Link ensemble mean increment
   if [ $FHR -eq 6 ]; then
      $NLN $COMIN_ENS/${APREFIX_ENKF}atminc.ensmean$ASUFFIX ./atminc_ensmean
   else
      $NLN $COMIN_ENS/${APREFIX_ENKF}atmi00${FHR}.ensmean$ASUFFIX ./atminc_ensmean
   fi

   # Compute ensemble mean increment
   DATAPATH="./"
   ATMINCNAME="atminc"
   ATMINCMEANNAME="atminc_ensmean"

   export OMP_NUM_THREADS=$NTHREADS_ECEN
   export pgm=$GETATMENSMEANEXEC
   . prep_step

   $NCP $GETATMENSMEANEXEC $DATA
   $APRUN_ECEN ${DATA}/$(basename $GETATMENSMEANEXEC) $DATAPATH $ATMINCMEANNAME $ATMINCNAME $NMEM_ENKF
   export err=$?; err_chk

   # If available, link to ensemble mean guess.  Otherwise, compute ensemble mean guess
   if [ -s $COMIN_GES_ENS/${GPREFIX}atmf00${FHR}.ensmean$GSUFFIX ]; then
       $NLN $COMIN_GES_ENS/${GPREFIX}atmf00${FHR}.ensmean$GSUFFIX ./atmges_ensmean
   else
       DATAPATH="./"
       ATMGESNAME="atmges"
       ATMGESMEANNAME="atmges_ensmean"

       export OMP_NUM_THREADS=$NTHREADS_ECEN
       export pgm=$GETATMENSMEANEXEC
       . prep_step

       $NCP $GETATMENSMEANEXEC $DATA
       $APRUN_ECEN ${DATA}/$(basename $GETATMENSMEANEXEC) $DATAPATH $ATMGESMEANNAME $ATMGESNAME $NMEM_ENKF
       export err=$?; err_chk
   fi
fi

if [ ${SUFFIX} = ".nc" ]; then
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
else
   LONB_ENKF=${LONB_ENKF:-$($NEMSIOGET atmanl_ensmean dimx | awk '{print $2}')}
   LATB_ENKF=${LATB_ENKF:-$($NEMSIOGET atmanl_ensmean dimy | awk '{print $2}')}
   LEVS_ENKF=${LEVS_ENKF:-$($NEMSIOGET atmanl_ensmean dimz | awk '{print $2}')}
   JCAP_ENKF=${JCAP_ENKF:-$($NEMSIOGET atmanl_ensmean jcap | awk '{print $2}')}
fi
[ $JCAP_ENKF -eq -9999 -a $LATB_ENKF -ne -9999 ] && JCAP_ENKF=$((LATB_ENKF-2))
[ $LONB_ENKF -eq -9999 -o $LATB_ENKF -eq -9999 -o $LEVS_ENKF -eq -9999 -o $JCAP_ENKF -eq -9999 ] && exit -9999

################################################################################
# This is to give the user the option to recenter, default is YES
if [ $RECENTER_ENKF = "YES" ]; then

   # GSI EnVar analysis
   if [ $FHR -eq 6 ]; then
     ATMANL_GSI=$COMIN/${APREFIX}atmanl$ASUFFIX
     ATMANL_GSI_ENSRES=$COMIN/${APREFIX}atmanl.ensres$ASUFFIX
   else
     ATMANL_GSI=$COMIN/${APREFIX}atma00${FHR}$ASUFFIX
     ATMANL_GSI_ENSRES=$COMIN/${APREFIX}atma00${FHR}.ensres$ASUFFIX
   fi

   # if we already have a ensemble resolution GSI analysis then just link to it
   if [ -f $ATMANL_GSI_ENSRES ]; then

      $NLN $ATMANL_GSI_ENSRES        atmanl_gsi_ensres

   else

      $NLN $ATMANL_GSI        atmanl_gsi
      $NLN $ATMANL_GSI_ENSRES atmanl_gsi_ensres
      SIGLEVEL=${SIGLEVEL:-${FIXgsm}/global_hyblev.l${LEVS}.txt}
      if [ ${SUFFIX} = ".nc" ]; then
         $NLN $CHGRESNC chgres.x
         chgresnml=chgres_nc_gauss.nml
         nmltitle=chgres
      else
         $NLN $CHGRESNEMS chgres.x
         chgresnml=fort.43
         nmltitle=nam
      fi

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
      $APRUN_ECEN ${DATA}/$(basename $RECENATMEXEC) $FILENAMEIN $FILENAME_MEANIN $FILENAME_MEANOUT $FILENAMEOUT $NMEM_ENKF
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
      $APRUN_ECEN ${DATA}/$(basename $RECENATMEXEC) $FILENAMEIN $FILENAME_INCMEANIN $FILENAME_GSIDET $FILENAMEOUT $NMEM_ENKF $FILENAME_GESMEANIN
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
   if [ ${SUFFIX} = ".nc" ]; then
      CALCINCEXEC=$CALCINCNCEXEC
   else
      CALCINCEXEC=$CALCINCNEMSEXEC
   fi

   export pgm=$CALCINCEXEC
   . prep_step

   $NCP $CALCINCEXEC $DATA

   rm calc_increment.nml
   cat > calc_increment.nml << EOF
&setup
  datapath = './'
  analysis_filename = '$ATMANLNAME'
  firstguess_filename = 'atmges'
  increment_filename = 'atminc'
  debug = .false.
  nens = $NMEM_ENKF
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
set +x
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
