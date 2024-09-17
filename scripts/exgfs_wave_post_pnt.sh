#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         exgfs_wave_post_pnt.sh
# Script description:  Creates point output products from binary WW3 data
#
# Author:   Jose-Henrique Alves Org: NCEP/EMC      Date: 2019-12-06
# Abstract: This script is the point postprocessor for the wave component in GFS.
#           It executes several scripts forpreparing and creating output data
#           as follows:
#
#  wave_outp_spec.sh         : generates spectral data for output locations
#  wave_outp_bull.sh         : generates bulletins for output locations
#  wave_outp_cat.sh          : cats the by hour into the single output file
#  wave_tar.sh               : tars the spectral and bulletin multiple files
#
# Script history log:
# 2019-12-06  J-Henrique Alves: First Version adapted from HTolman post.sh 2007
# 2020-06-10  J-Henrique Alves: Porting to R&D machine Hera
# 2020-07-30  Jessica Meixner: Points only - no gridded data
# 2020-09-29  Jessica Meixner: optimized by changing loop structures
#
# COM inputs:
#  - ${COMIN_WAVE_PREP}/${RUN}wave.mod_def.${grdID}
#  - ${COMIN_WAVE_HISTORY}/${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${PDY}.${HMS}
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (Bash) Shell
#
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"

# 0.a Basic modes of operation

  cd $DATA

  # Set wave model ID tag to include member number
  # if ensemble; waveMEMB var empty in deterministic
  export WAV_MOD_TAG=${RUN}wave${waveMEMB}

  echo "HAS BEGUN on $(hostname)"
  echo "Starting WAVE PNT POSTPROCESSOR SCRIPT for $WAV_MOD_TAG"

  set +x
  echo ' '
  echo '                     *************************************'
  echo '                     *** WAVE PNT POSTPROCESSOR SCRIPT ***'
  echo '                     *************************************'
  echo ' '
  echo "Starting at : $(date)"
  echo '-------------'
  echo ' '
  set_trace

# Script will run only if pre-defined NTASKS
#     The actual work is distributed over these tasks.
  if [ -z ${NTASKS} ]
  then
    echo "FATAL ERROR: requires NTASKS to be set "
    err=1; export err;${errchk}
    exit $err
  fi

# 0.c Defining model grids

  waveuoutpGRD=${waveuoutpGRD:?buoyNotSet}

# 0.c.1 Define a temporary directory for storing ascii point output files
#       and flush it

  export STA_DIR=$DATA/station_ascii_files
  if [ -d $STA_DIR ]
  then
    rm -rf ${STA_DIR}
  fi
  mkdir -p ${STA_DIR}
  mkdir -p ${STA_DIR}/spec
  mkdir -p ${STA_DIR}/bull
  mkdir -p ${STA_DIR}/cbull
  mkdir -p ${STA_DIR}/specfhr
  mkdir -p ${STA_DIR}/bullfhr
  mkdir -p ${STA_DIR}/cbullfhr

  set +x
  echo ' '
  echo 'Grid information  :'
  echo '-------------------'
  echo "   Output points : $waveuoutpGRD"
  echo ' '
  set_trace

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

  exit_code=0

  set +x
  echo ' '
  echo 'Preparing input files :'
  echo '-----------------------'
  set_trace

# 1.a Model definition files and output files (set up using poe)

# 1.a.1 Set up the parallel command tasks

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

  set_trace

# Copy model definition files
  iloop=0
  for grdID in ${waveuoutpGRD}; do
    if [[ -f "${COMIN_WAVE_PREP}/${RUN}wave.mod_def.${grdID}" ]]; then
      set +x
      echo " Mod def file for ${grdID} found in ${COMIN_WAVE_PREP}. copying ...."
      set_trace

      cp -f "${COMIN_WAVE_PREP}/${RUN}wave.mod_def.${grdID}" "mod_def.${grdID}"
      iloop=$((iloop + 1))
    fi
  done

  for grdID in $waveuoutpGRD
  do
    if [ ! -f mod_def.$grdID ]
    then
      set +x
      echo ' '
      echo '*************************************************** '
      echo " FATAL ERROR : NO MOD_DEF FILE mod_def.$grdID "
      echo '*************************************************** '
      echo ' '
      set_trace
      err=2; export err;${errchk}
      exit $err
    else
      set +x
      echo "File mod_def.$grdID found. Syncing to all nodes ..."
      set_trace
    fi
  done

# 1.c Output locations file

  rm -f buoy.loc

  if [ -f ${PARMgfs}/wave/wave_${NET}.buoys ]
  then
    cp -f ${PARMgfs}/wave/wave_${NET}.buoys buoy.loc.temp
    if [ "$DOBNDPNT_WAV" = YES ]; then
      #only do boundary points
      sed -n '/^\$.*/!p' buoy.loc.temp | grep IBP > buoy.loc || {
          echo "WARNING: No boundary points found in buoy file ${PARMgfs}/wave/wave_${NET}.buoys"
          echo "         Ending job without doing anything."
          exit 0
        }
    else
      #exclude boundary points
      sed -n '/^\$.*/!p' buoy.loc.temp | grep -v IBP > buoy.loc
    fi
  fi

  if [ -s buoy.loc ]
  then
    set +x
    echo "   buoy.loc and buoy.ibp copied and processed (${PARMgfs}/wave/wave_${NET}.buoys)."
    set_trace
  else
    set +x
    echo ' '
    echo '************************************* '
    echo ' FATAL ERROR : NO BUOY LOCATION FILE  '
    echo '************************************* '
    echo ' '
    set_trace
    err=3; export err;${errchk}
    exit $err
    DOSPC_WAV='NO'
    DOBLL_WAV='NO'
  fi

# 1.d Input template files

  if [ -f ${PARMgfs}/wave/ww3_outp_spec.inp.tmpl ]
  then
    cp -f ${PARMgfs}/wave/ww3_outp_spec.inp.tmpl ww3_outp_spec.inp.tmpl
  fi

  if [ -f ww3_outp_spec.inp.tmpl ]
  then
    set +x
    echo "   ww3_outp_spec.inp.tmpl copied. Syncing to all grids ..."
    set_trace
  else
    set +x
    echo ' '
    echo '*********************************************** '
    echo '*** ERROR : NO TEMPLATE FOR SPEC INPUT FILE *** '
    echo '*********************************************** '
    echo ' '
    set_trace
    exit_code=3
    DOSPC_WAV='NO'
    DOBLL_WAV='NO'
  fi

  if [ -f ${PARMgfs}/wave/ww3_outp_bull.inp.tmpl ]
  then
    cp -f ${PARMgfs}/wave/ww3_outp_bull.inp.tmpl ww3_outp_bull.inp.tmpl
  fi

  if [ -f ww3_outp_bull.inp.tmpl ]
  then
    set +x
    echo "   ww3_outp_bull.inp.tmpl copied. Syncing to all nodes ..."
    set_trace
  else
    set +x
    echo ' '
    echo '*************************************************** '
    echo '*** ERROR : NO TEMPLATE FOR BULLETIN INPUT FILE *** '
    echo '*************************************************** '
    echo ' '
    set_trace
    exit_code=4
    DOBLL_WAV='NO'
  fi

# 1.e Getting buoy information for points

  if [ "$DOSPC_WAV" = 'YES' ] || [ "$DOBLL_WAV" = 'YES' ]
  then
    ymdh=$(${NDATE} -"${WAVHINDH}" "${PDY}${cyc}")
    tstart="${ymdh:0:8} ${ymdh:8:2}0000"
    dtspec=3600.            # default time step (not used here)
    sed -e "s/TIME/${tstart}/g" \
        -e "s/DT/${dtspec}/g" \
        -e "s/POINT/1/g" \
        -e "s/ITYPE/0/g" \
        -e "s/FORMAT/F/g" \
                               ww3_outp_spec.inp.tmpl > ww3_outp.inp

    ${NLN} mod_def.$waveuoutpGRD mod_def.ww3
    #export OFFSET_START_HOUR=$( printf "%02d" ${half_assim} )
    hh=$( printf "%02d" $(( cyc + OFFSET_START_HOUR )) )
    HMS="${hh}0000"
    if [[ -f "${COMIN_WAVE_HISTORY}/${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${PDY}.${HMS}" ]]; then
      ${NLN} "${COMIN_WAVE_HISTORY}/${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${PDY}.${HMS}" \
        "./out_pnt.${waveuoutpGRD}"
    else
      echo '*************************************************** '
      echo " FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.${waveuoutpGRD}.${PDY}.${HMS} "
      echo '*************************************************** '
      echo ' '
      set_trace
      echo "${WAV_MOD_TAG} post ${waveuoutpGRD} ${PDY}${cyc} ${cycle} : field output missing."
      err=4; export err;${errchk}
    fi

    rm -f buoy_tmp.loc buoy_log.ww3 ww3_oup.inp
    ${NLN} ./out_pnt.${waveuoutpGRD} ./out_pnt.ww3
    ${NLN} ./mod_def.${waveuoutpGRD} ./mod_def.ww3
    export pgm=ww3_outp;. prep_step
    ${EXECgfs}/ww3_outp > buoy_lst.loc 2>&1
    export err=$?;err_chk


    if [ "$err" != '0' ] && [ ! -f buoy_log.ww3 ]
    then
      pgm=wave_post
      set +x
      echo ' '
      echo '******************************************** '
      echo '*** FATAL ERROR : ERROR IN ww3_outp *** '
      echo '******************************************** '
      echo ' '
      cat buoy_tmp.loc
      echo "$WAV_MOD_TAG post $date $cycle : buoy log file failed to be created."
      set_trace
      err=5;export err;${errchk}
      DOSPC_WAV='NO'
      DOBLL_WAV='NO'
      exit $err
    fi

# Create new buoy_log.ww3
    awk '{print $3}' buoy.loc | sed 's/'\''//g' > ibp_tags
    grep -F -f ibp_tags buoy_log.ww3 > buoy_log.tmp
    rm -f buoy_log.dat
    mv buoy_log.tmp buoy_log.dat

    grep -F -f ibp_tags buoy_lst.loc > buoy_tmp1.loc
    #sed    '$d' buoy_tmp1.loc > buoy_tmp2.loc
    awk '{ print $1 }' buoy_tmp1.loc > buoy_lst.txt
    Nb=$(wc buoy_tmp1.loc | awk '{ print $1 }')
    rm -f buoy_tmp1.loc

    if [ -s buoy_log.dat ]
    then
      set +x
      echo 'Buoy log file created. Syncing to all nodes ...'
      set_trace
    else
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO BUOY LOG FILE CREATED *** '
      echo '**************************************** '
      echo ' '
      set_trace
      err=6;export err;${errchk}
      DOSPC_WAV='NO'
      DOBLL_WAV='NO'
    fi

 fi

# 1.f Data summary

  set +x
  echo ' '
  echo "   Input files read and processed at : $(date)"
  echo ' '
  echo '   Data summary : '
  echo '   ---------------------------------------------'
  echo "      Sufficient data for spectral files        : $DOSPC_WAV ($Nb points)"
  echo "      Sufficient data for bulletins             : $DOBLL_WAV ($Nb points)"
  echo "      Boundary points                           : $DOBNDPNT_WAV"
  echo ' '
  set_trace

# --------------------------------------------------------------------------- #
# 2. Make files for processing boundary points
#
# 2.a Command file set-up

  set +x
  echo '   Making command file for wave post points '
  set_trace

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

# 1.a.2 Loop over forecast time to generate post files
  fhr=$FHMIN_WAV
  # Generated sed-searchable paths
  escaped_USHgfs="${USHgfs//\//\\\/}"
  while [ $fhr -le $FHMAX_WAV_PNT ]; do

    echo "   Creating the wave point scripts at : $(date)"
    ymdh=$($NDATE "${fhr}" "${PDY}${cyc}")
    YMD=${ymdh:0:8}
    HMS="${ymdh:8:2}0000"
    YMDHMS=${YMD}${HMS}
    FH3=$(printf %03i ${fhr})

    rm -f tmpcmdfile.${FH3}
    touch tmpcmdfile.${FH3}
    mkdir output_$YMDHMS
    cd output_$YMDHMS

# Create instances of directories for spec and gridded output
    export SPECDATA=${DATA}/output_$YMDHMS
    escaped_SPECDATA="${SPECDATA//\//\\\/}"
    export BULLDATA=${DATA}/output_$YMDHMS
    cp $DATA/mod_def.${waveuoutpGRD} mod_def.${waveuoutpGRD}

    pfile="${COMIN_WAVE_HISTORY}/${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${YMD}.${HMS}"
    if [ -f  ${pfile} ]
    then
      ${NLN} ${pfile} ./out_pnt.${waveuoutpGRD}
    else
      echo " FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.$waveuoutpGRD.${YMD}.${HMS} "
      echo ' '
      set_trace
      err=7; export err;${errchk}
      exit $err
    fi

    cd $DATA

    if [ "$DOSPC_WAV" = 'YES' ]
    then
      export dtspec=3600.
      # Construct the wave_outp_spec (spec) command to run on each buoy in buoy_lst.txt
      sed "s/^\(.*\)$/${escaped_USHgfs}\/wave_outp_spec.sh \1 ${ymdh} spec ${escaped_SPECDATA} > ${escaped_SPECDATA}\/spec_\1.out 2>\&1/" buoy_lst.txt >> "tmpcmdfile.${FH3}"
    fi

    if [ "$DOBLL_WAV" = 'YES' ]
    then
      export dtspec=3600.
      # Construct the wave_outp_spec (bull) command to run on each buoy in buoy_lst.txt
      sed "s/^\(.*\)$/${escaped_USHgfs}\/wave_outp_spec.sh \1 ${ymdh} bull ${escaped_SPECDATA} > ${escaped_SPECDATA}\/bull_\1.out 2>\&1/" buoy_lst.txt >> "tmpcmdfile.${FH3}"
    fi

    split -n l/1/10  tmpcmdfile.$FH3 > cmdfile.${FH3}.01
    split -n l/2/10  tmpcmdfile.$FH3 > cmdfile.${FH3}.02
    split -n l/3/10  tmpcmdfile.$FH3 > cmdfile.${FH3}.03
    split -n l/4/10  tmpcmdfile.$FH3 > cmdfile.${FH3}.04
    split -n l/5/10  tmpcmdfile.$FH3 > cmdfile.${FH3}.05
    split -n l/6/10  tmpcmdfile.$FH3 > cmdfile.${FH3}.06
    split -n l/7/10  tmpcmdfile.$FH3 > cmdfile.${FH3}.07
    split -n l/8/10  tmpcmdfile.$FH3 > cmdfile.${FH3}.08
    split -n l/9/10  tmpcmdfile.$FH3 > cmdfile.${FH3}.09
    split -n l/10/10 tmpcmdfile.$FH3 > cmdfile.${FH3}.10

    rm tmpcmdfile.$FH3
    chmod 744 cmdfile.${FH3}.01 cmdfile.${FH3}.02 cmdfile.${FH3}.03 cmdfile.${FH3}.04
    chmod 744 cmdfile.${FH3}.05 cmdfile.${FH3}.06 cmdfile.${FH3}.07 cmdfile.${FH3}.08
    chmod 744 cmdfile.${FH3}.09 cmdfile.${FH3}.10
    echo "$DATA/cmdfile.${FH3}.01" >> cmdfile
    echo "$DATA/cmdfile.${FH3}.02" >> cmdfile
    echo "$DATA/cmdfile.${FH3}.03" >> cmdfile
    echo "$DATA/cmdfile.${FH3}.04" >> cmdfile
    echo "$DATA/cmdfile.${FH3}.05" >> cmdfile
    echo "$DATA/cmdfile.${FH3}.06" >> cmdfile
    echo "$DATA/cmdfile.${FH3}.07" >> cmdfile
    echo "$DATA/cmdfile.${FH3}.08" >> cmdfile
    echo "$DATA/cmdfile.${FH3}.09" >> cmdfile
    echo "$DATA/cmdfile.${FH3}.10" >> cmdfile


    FHINCP=$(( DTPNT_WAV / 3600 ))
    fhrp=$((fhr+FHINCP))
    fhr=$fhrp # no gridded output, loop with out_pnt stride

  done


  if [ ${CFP_MP:-"NO"} = "YES" ]; then
    nfile=0
    ifile=0
    iline=1
    ifirst='yes'
    nlines=$( wc -l cmdfile | awk '{print $1}' )
    while [ $iline -le $nlines ]; do
      line=$( sed -n ''$iline'p' cmdfile )
      if [ -z "$line" ]; then
        break
      else
        if [ "$ifirst" = 'yes' ]; then
          echo "#!/bin/sh" > cmdmfile.$nfile
          echo "$nfile cmdmfile.$nfile" >> cmdmprog
          chmod 744 cmdmfile.$nfile
        fi
        echo $line >> cmdmfile.$nfile
        nfile=$(( nfile + 1 ))
        if [ $nfile -eq $NTASKS ]; then
          nfile=0
          ifirst='no'
        fi
        iline=$(( iline + 1 ))
      fi
    done
  fi

  wavenproc=$(wc -l cmdfile | awk '{print $1}')
  wavenproc=$(echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS})))

  set +x
  echo ' '
  echo "   Executing the wave point scripts at : $(date)"
  echo '   ------------------------------------'
  echo ' '
  set_trace

  if [ "$wavenproc" -gt '1' ]
  then
    if [ ${CFP_MP:-"NO"} = "YES" ]; then
      ${wavempexec} -n ${wavenproc} ${wave_mpmd} cmdmprog
    else
      ${wavempexec} ${wavenproc} ${wave_mpmd} cmdfile
    fi
    exit=$?
  else
    chmod 744 cmdfile
    ./cmdfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '*************************************'
    echo '*** FATAL ERROR: CMDFILE FAILED   ***'
    echo '*************************************'
    echo '     See Details Below '
    echo ' '
    set_trace
    err=8; export err;${errchk}
    exit $err
  fi

# 2.b Loop over each buoy to cat the final buoy file for all fhr

  cd $DATA

  echo "Before create cmdfile for cat buoy : $(date)"
  rm -f cmdfile.buoy
  touch cmdfile.buoy
  chmod 744 cmdfile.buoy
  CATOUTDIR=${DATA}/pnt_cat_out
  escaped_CATOUTDIR="${CATOUTDIR//\//\\\/}"
  mkdir -p ${CATOUTDIR}

  if [ "$DOSPC_WAV" = 'YES' ]
  then
    # Construct wave_outp_cat (spec) call for each buoy in buoy_lst.txt
    sed "s/^\(.*\)$/${escaped_USHgfs}\/wave_outp_cat.sh \1 ${FHMAX_WAV_PNT} spec > ${escaped_CATOUTDIR}\/spec_cat_\1.out 2>\&1/" buoy_lst.txt >> cmdfile.buoy
  fi

  if [ "$DOBLL_WAV" = 'YES' ]
  then
    # Construct wave_outp_cat (bull) call for each buoy in buoy_lst.txt
    sed "s/^\(.*\)$/${escaped_USHgfs}\/wave_outp_cat.sh \1 ${FHMAX_WAV_PNT} bull > ${escaped_CATOUTDIR}\/bull_cat_\1.out 2>\&1/" buoy_lst.txt >> cmdfile.buoy
  fi

  if [ ${CFP_MP:-"NO"} = "YES" ]; then
    nfile=0
    ifile=0
    iline=1
    ifirst='yes'
    nlines=$( wc -l < cmdfile.buoy)
    while [ $iline -le $nlines ]; do
      line=$( sed -n ''$iline'p' cmdfile.buoy )
      if [ -z "$line" ]; then
        break
      else
        if [ "$ifirst" = 'yes' ]; then
          echo "#!/bin/sh" > cmdfile.buoy.$nfile
          echo "$nfile cmdfile.buoy.$nfile" >> cmdmprogbuoy
          chmod 744 cmdfile.buoy.$nfile
        fi
        echo $line >> cmdfile.buoy.$nfile
        nfile=$(( nfile + 1 ))
        if [ $nfile -eq $NTASKS ]; then
          nfile=0
          ifirst='no'
        fi
        iline=$(( iline + 1 ))
      fi
    done
  fi

  wavenproc=$(wc -l < cmdfile.buoy)
  wavenproc=$(echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS})))

  set +x
  echo ' '
  echo "   Executing the boundary point cat script at : $(date)"
  echo '   ------------------------------------'
  echo ' '
  set_trace

  if [ "$wavenproc" -gt '1' ]
  then
    if [ ${CFP_MP:-"NO"} = "YES" ]; then
      # shellcheck disable=SC2086
      ${wavempexec} -n "${wavenproc}" ${wave_mpmd} cmdmprogbuoy
    else
      # shellcheck disable=SC2086
      ${wavempexec} "${wavenproc}" ${wave_mpmd} cmdfile.buoy
    fi
    exit=$?
  else
    chmod 744 ${fcmdnow}
    ./${fcmdnow}
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '*************************************'
    echo '*** FATAL ERROR: CMDFILE FAILED   ***'
    echo '*************************************'
    echo '     See Details Below '
    echo ' '
    set_trace
    err=9; export err;${errchk}
    exit $err
  fi


# --------------------------------------------------------------------------- #
# 3. Compress point output data into tar files

# 3.a Set up cmdfile

  rm -f cmdtarfile
  touch cmdtarfile
  chmod 744 cmdtarfile

  set +x
  echo ' '
  echo '   Making command file for taring all point output files.'

  set_trace

# 6.b Spectral data files

  if [ ${CFP_MP:-"NO"} = "YES" ]; then nm=0; fi

  if [ ${CFP_MP:-"NO"} = "YES" ] && [ "$DOBLL_WAV" = "YES" ]; then
    if [ "$DOBNDPNT_WAV" = YES ]; then
      if [ "$DOSPC_WAV" = YES ]; then
        echo "$nm ${USHgfs}/wave_tar.sh $WAV_MOD_TAG ibp $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
      fi
      if [ "$DOBLL_WAV" = YES ]; then
        echo "$nm ${USHgfs}/wave_tar.sh $WAV_MOD_TAG ibpbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
        echo "$nm ${USHgfs}/wave_tar.sh $WAV_MOD_TAG ibpcbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
      fi
    else
      if [ "$DOSPC_WAV" = YES ]; then
        echo "$nm ${USHgfs}/wave_tar.sh $WAV_MOD_TAG spec $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
      fi
      if [ "$DOBLL_WAV" = YES ]; then
        echo "$nm ${USHgfs}/wave_tar.sh $WAV_MOD_TAG bull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
        echo "$nm ${USHgfs}/wave_tar.sh $WAV_MOD_TAG cbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
      fi
    fi
  else
    if [ "$DOBNDPNT_WAV" = YES ]; then
      if [ "$DOSPC_WAV" = YES ]; then
        echo "${USHgfs}/wave_tar.sh $WAV_MOD_TAG ibp $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
      fi
      if [ "$DOBLL_WAV" = YES ]; then
        echo "${USHgfs}/wave_tar.sh $WAV_MOD_TAG ibpbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        echo "${USHgfs}/wave_tar.sh $WAV_MOD_TAG ibpcbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
      fi
    else
      if [ "$DOSPC_WAV" = YES ]; then
        echo "${USHgfs}/wave_tar.sh $WAV_MOD_TAG spec $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
      fi
      if [ "$DOBLL_WAV" = YES ]; then
        echo "${USHgfs}/wave_tar.sh $WAV_MOD_TAG bull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        echo "${USHgfs}/wave_tar.sh $WAV_MOD_TAG cbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
      fi
    fi
  fi

  wavenproc=$(wc -l cmdtarfile | awk '{print $1}')
  wavenproc=$(echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS})))

  set +x
  echo ' '
  echo "   Executing the wave_tar scripts at : $(date)"
  echo '   ------------------------------------'
  echo ' '
  set_trace

  if [ "$wavenproc" -gt '1' ]
  then
    if [ ${CFP_MP:-"NO"} = "YES" ]; then
      ${wavempexec} -n ${wavenproc} ${wave_mpmd} cmdtarfile
    else
      ${wavempexec} ${wavenproc} ${wave_mpmd} cmdtarfile
    fi
    exit=$?
  else
    chmod 744 cmdtarfile
    ./cmdtarfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '*************************************'
    echo '*** FATAL ERROR: CMDFILE FAILED   ***'
    echo '*************************************'
    echo '     See Details Below '
    echo ' '
    set_trace
    err=10; export err;${errchk}
  exit $err
  fi

# --------------------------------------------------------------------------- #
# 4.  Ending output


exit "${exit_code}"

# End of MWW3 point prostprocessor script ---------------------------------------- #
