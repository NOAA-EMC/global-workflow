#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         exwave_prep.sh
# Script description:  Creates output products from binary WW3 data
#
# Author:   Hendrik Tolman      Org: NCEP/EMC      Date: 2007-03-01
# Abstract: This is the preprocessor for the wave component in GFS.
#           It executes several scripts for preparing and creating input data
#           as follows:
#                                                                             
#  wave_prnc_ice.sh     : preprocess ice fields.                              #
#  wave_prnc_cur.sh     : preprocess current fields.                          #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors output is witten to the wave.log file.               #
#                                                                             #
# COM inputs:                                                                 #
#  - ${COMIN_WAVE_PREP}/${RUN}wave.mod_def.${grdID}                           #
#  - ${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_f#HHH_prog.nc        #
#                                                                             #
# COM outputs:                                                                #
#  - ${COMOUT_WAVE_PREP}/${RUN}wave.${WAVECUR_FID}.$cycle.cur                 #
#                                                                             #
#  Update record :                                                            #
#                                                                             #
# - Origination:                                               01-Mar-2007    #
#                                                                             #
# Update log                                                                  #
# Mar2007 HTolman - Added NCO note on resources on mist/dew                   #
# Apr2007 HTolman - Renaming mod_def files in ${FIXgfs}/wave.                 #
# Mar2011 AChawla - Migrating to a vertical structure                         #
# Nov2012 JHAlves - Transitioning to WCOSS                                    #
# Apr2019 JHAlves - Transitioning to GEFS workflow                            #
# Nov2019 JHAlves - Merging wave scripts to global workflow                   #
# Jun2020 JHAlves - Porting to R&D machine Hera                               #
# Oct2020 JMeixner - Updating RTOFS dates for processing minimal amount       #
# May2022 JMeixner - Clean up and moving input to other routine               #
#                                                                             #
#   WAV_MOD_ID and WAV_MOD_TAG replace modID. WAV_MOD_TAG                     # 
#   is used for ensemble-specific I/O. For deterministic                      #
#   WAV_MOD_ID=WAV_MOD_TAG                                                    # 
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"

# 0.a Basic modes of operation

  # Set wave model ID tag to include member number
  # if ensemble; waveMEMB var empty in deterministic
  export WAV_MOD_TAG=${RUN}wave${waveMEMB}

  cd $DATA
  mkdir outtmp

  echo "HAS BEGUN on $(hostname)"
  echo "Starting MWW3 PREPROCESSOR SCRIPT for $WAV_MOD_TAG"

  set +x
  echo ' '
  echo '                      ********************************'
  echo '                      *** WW3 PREPROCESSOR SCRIPT  ***'
  echo '                      ********************************'
  echo '                          PREP for wave component of NCEP coupled system'
  echo "                          Wave component identifier : $WAV_MOD_TAG "
  echo ' '
  echo "Starting at : $(date)"
  echo ' '
  set_trace

  # 0.b Date and time stuff

  # Beginning time for outpupt may differ from SDATE if DOIAU=YES
  export date=$PDY
  export YMDH=${PDY}${cyc}
  # Roll back $IAU_FHROT hours of DOIAU=YES
  IAU_FHROT=3
  if [ "$DOIAU" = "YES" ]
  then
    WAVHINDH=$(( WAVHINDH + IAU_FHROT ))
  fi
  # Set time stamps for model start and output
  # For special case when IAU is on but this is an initial half cycle 
  if [ $IAU_OFFSET = 0 ]; then
    ymdh_beg=$YMDH
  else
    ymdh_beg=$($NDATE -$WAVHINDH $YMDH)
  fi
  time_beg="$(echo $ymdh_beg | cut -c1-8) $(echo $ymdh_beg | cut -c9-10)0000"
  ymdh_end=$($NDATE $FHMAX_WAV $YMDH)
  time_end="$(echo $ymdh_end | cut -c1-8) $(echo $ymdh_end | cut -c9-10)0000"
  ymdh_beg_out=$YMDH
  time_beg_out="$(echo $ymdh_beg_out | cut -c1-8) $(echo $ymdh_beg_out | cut -c9-10)0000"

  # Restart file times (already has IAU_FHROT in WAVHINDH) 
  RSTOFFSET=$(( ${WAVHCYC} - ${WAVHINDH} ))
  # Update restart time is added offset relative to model start
  RSTOFFSET=$(( ${RSTOFFSET} + ${RSTIOFF_WAV} ))
  ymdh_rst_ini=$($NDATE ${RSTOFFSET} $YMDH)
  RST2OFFSET=$(( DT_2_RST_WAV / 3600 ))
  ymdh_rst2_ini=$($NDATE ${RST2OFFSET} $YMDH) # DT2 relative to first-first-cycle restart file
  # First restart file for cycling
  time_rst_ini="$(echo $ymdh_rst_ini | cut -c1-8) $(echo $ymdh_rst_ini | cut -c9-10)0000"
  if [ ${DT_1_RST_WAV} = 1 ]; then
    time_rst1_end=${time_rst_ini}
  else
    RST1OFFSET=$(( DT_1_RST_WAV / 3600 ))
    ymdh_rst1_end=$($NDATE $RST1OFFSET $ymdh_rst_ini)
    time_rst1_end="$(echo $ymdh_rst1_end | cut -c1-8) $(echo $ymdh_rst1_end | cut -c9-10)0000"
  fi
  # Second restart file for checkpointing
  if [ "${RSTTYPE_WAV}" = "T" ]; then
    time_rst2_ini="$(echo $ymdh_rst2_ini | cut -c1-8) $(echo $ymdh_rst2_ini | cut -c9-10)0000"
    time_rst2_end=$time_end
  # Condition for gdas run or any other run when checkpoint stamp is > ymdh_end
    if [ $ymdh_rst2_ini -ge $ymdh_end ]; then
      ymdh_rst2_ini=$($NDATE 3 $ymdh_end)
      time_rst2_ini="$(echo $ymdh_rst2_ini | cut -c1-8) $(echo $ymdh_rst2_ini | cut -c9-10)0000"
      time_rst2_end=$time_rst2_ini
    fi
  else
    time_rst2_ini="$"
    time_rst2_end=
    DT_2_RST_WAV=
  fi
  set +x
  echo ' '
  echo 'Times in wave model format :'
  echo '----------------------------'
  echo "   date / cycle  : $date $cycle"
  echo "   starting time : $time_beg"
  echo "   ending time   : $time_end"
  echo ' '
  set_trace

  # Script will run only if pre-defined NTASKS
  #     The actual work is distributed over these tasks.
  if [ -z ${NTASKS} ]        
  then
    echo "FATAL ERROR: Requires NTASKS to be set "
    err=1; export err;${errchk}
  fi

  # --------------------------------------------------------------------------- #
  # 1.  Get files that are used by most child scripts

  set +x
  echo 'Preparing input files :'
  echo '-----------------------'
  echo ' '
  set_trace

  # 1.a Model definition files

  rm -f cmdfile
  touch cmdfile

  grdINP=''
  if [ "${WW3ATMINP}" = 'YES' ]; then grdINP="${grdINP} $WAVEWND_FID" ; fi
  if [ "${WW3ICEINP}" = 'YES' ]; then grdINP="${grdINP} $WAVEICE_FID" ; fi
  if [ "${WW3CURINP}" = 'YES' ]; then grdINP="${grdINP} $WAVECUR_FID" ; fi

  ifile=1

  for grdID in $grdINP $waveGRD
  do
    if [ -f "${COMIN_WAVE_PREP}/${RUN}wave.mod_def.${grdID}" ]
    then
      set +x
      echo " Mod def file for $grdID found in ${COMIN_WAVE_PREP}. copying ...."
      set_trace
      cp ${COMIN_WAVE_PREP}/${RUN}wave.mod_def.${grdID} mod_def.$grdID

    else
      set +x
      echo ' '
      echo '*********************************************************** '
      echo '*** FATAL ERROR : NOT FOUND WAVE  MODEL DEFINITION FILE *** '
      echo '*********************************************************** '
      echo "                                grdID = $grdID"
      echo ' '
      echo "FATAL ERROR: NO MODEL DEFINITION FILE"
      set_trace
      err=2;export err;${errchk}
    fi
  done

  # 1.b Netcdf Preprocessor template files
   if [[ "${WW3ATMINP}" == 'YES' ]]; then itype="${itype:-} wind" ; fi 
   if [[ "${WW3ICEINP}" == 'YES' ]]; then itype="${itype:-} ice" ; fi 
   if [[ "${WW3CURINP}" == 'YES' ]]; then itype="${itype:-} cur" ; fi 

   for type in $itype
   do

     case $type in
       wind )
         grdID=$WAVEWND_FID
       ;;
       ice )
         grdID=$WAVEICE_FID 
       ;;
       cur )
         grdID=$WAVECUR_FID 
       ;;
       * )
              echo 'Input type not yet implemented' 	    
              err=3; export err;${errchk}
              ;;
     esac 

     if [ -f ${PARMgfs}/wave/ww3_prnc.${type}.$grdID.inp.tmpl ]
     then
       cp ${PARMgfs}/wave/ww3_prnc.${type}.$grdID.inp.tmpl .
     fi

     if [ -f ww3_prnc.${type}.$grdID.inp.tmpl ]
     then
       set +x
       echo ' '
       echo "   ww3_prnc.${type}.$grdID.inp.tmpl copied (${PARMgfs}/wave)."
       echo ' '
       set_trace
     else
       set +x
       echo ' '
       echo '************************************** '
       echo '*** FATAL ERROR : NO TEMPLATE FILE *** '
       echo '************************************** '
       echo "             ww3_prnc.${type}.$grdID.inp.tmpl"
       echo ' '
       echo "ABNORMAL EXIT: NO FILE $file"
       echo ' '
       set_trace
       err=4;export err;${errchk}
     fi
   done

# --------------------------------------------------------------------------- #
# ICEC processing

  if [ "${WW3ICEINP}" = 'YES' ]; then

# --------------------------------------------------------------------------- #
# 2. Ice pre - processing 

# 2.a Check if ice input is perturbed (number of inputs equal to number of wave
#     ensemble members
    if [ "${RUNMEM}" = "-1" ] || [ "${WW3ICEIENS}" = "T" ] || [ "$waveMEMB" = "00" ]
    then

      ${USHgfs}/wave_prnc_ice.sh > wave_prnc_ice.out
      ERR=$?
    
      if [ -d ice ]
      then
        set +x
        echo ' '
        echo '      FATAL ERROR: ice field not generated '
        echo ' '
        sed "s/^/wave_prnc_ice.out : /g" wave_prnc_ice.out
        echo ' '
        set_trace
        err=5;export err;${errchk}
      else
        mv -f wave_prnc_ice.out $DATA/outtmp
        set +x
        echo ' '
        echo '      Ice field unpacking successful.'
        echo ' '
        set_trace
      fi
    else
      echo ' '
      echo "WARNING: Ice input is not perturbed, single ice file generated, skipping ${WAV_MOD_TAG}"
      echo ' '
    fi 
  else
      echo ' '
      echo 'WARNING: No input ice file generated, this run did not request pre-processed ice data '
      echo ' '
  fi

# --------------------------------------------------------------------------- #
# WIND processing 
  if [ "${WW3ATMINP}" = 'YES' ]; then

    echo ' '
    echo '*************************************************** '
    echo '*** FATAL ERROR : Not set-up to preprocess wind *** '
    echo '*************************************************** '
    echo ' '
    set_trace
    err=6;export err;${errchk} 

  fi

#-------------------------------------------------------------------
# 3.  Process current fields

  if [ "${WW3CURINP}" = 'YES' ]; then

# Get into single file 
    if [ "${RUNMEM}" = "-1" ] || [ "${WW3CURIENS}" = "T" ] || [ "$waveMEMB" = "00" ]
    then

      set +x
      echo ' '
      echo '   Concatenate binary current fields ...'
      echo ' '
      set_trace

# Prepare files for cfp process
      rm -f cmdfile
      touch cmdfile
      chmod 744 cmdfile

      ymdh_rtofs=${RPDY}00 # RTOFS runs once daily use ${PDY}00
      if [ "$ymdh_beg" -lt "$ymdh_rtofs" ];then 
         #If the start time is before the first hour of RTOFS, use the previous cycle
         export RPDY=$($NDATE -24 ${RPDY}00 | cut -c1-8)
      fi 
      #Set the first time for RTOFS files to be the beginning time of simulation
      ymdh_rtofs=$ymdh_beg

      if [  "$FHMAX_WAV_CUR" -le 72 ]; then 
        rtofsfile1="${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_f024_prog.nc"
        rtofsfile2="${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_f048_prog.nc"
        rtofsfile3="${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_f072_prog.nc"
        if [ ! -f $rtofsfile1 ] || [ ! -f $rtofsfile2 ] || [ ! -f $rtofsfile3 ]; then 
           #Needed current files are not available, so use RTOFS from previous day 
           export RPDY=$($NDATE -24 ${RPDY}00 | cut -c1-8)
        fi 
      else
        rtofsfile1="${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_f096_prog.nc"
        rtofsfile2="${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_f120_prog.nc"
        rtofsfile3="${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_f144_prog.nc"
        rtofsfile4="${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_f168_prog.nc"
        rtofsfile5="${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_f192_prog.nc"
        if [ ! -f $rtofsfile1 ] || [ ! -f $rtofsfile2 ] || [ ! -f $rtofsfile3 ] ||
            [ ! -f $rtofsfile4 ] || [ ! -f $rtofsfile5 ]; then
            #Needed current files are not available, so use RTOFS from previous day 
            export RPDY=$($NDATE -24 ${RPDY}00 | cut -c1-8)
        fi
      fi

      ymdh_end_rtofs=$($NDATE ${FHMAX_WAV_CUR} ${RPDY}00)
      if [ "$ymdh_end" -lt "$ymdh_end_rtofs" ]; then 
         ymdh_end_rtofs=$ymdh_end
      fi

      NDATE_DT=${WAV_CUR_HF_DT}
      FLGHF='T'
      FLGFIRST='T'
      fext='f'
  
      if [ ${CFP_MP:-"NO"} = "YES" ]; then nm=0 ; fi # Counter for MP CFP
      while [ "$ymdh_rtofs" -le "$ymdh_end_rtofs" ]
      do
        # Timing has to be made relative to the single 00z RTOFS cycle for RTOFS PDY (RPDY)
        # Start at first fhr for 
        fhr_rtofs=$(${NHOUR} ${ymdh_rtofs} ${RPDY}00)
        fh3_rtofs=$(printf "%03d" "${fhr_rtofs#0}")

        curfile1h=${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_${fext}${fh3_rtofs}_prog.nc
        curfile3h=${COMIN_RTOFS}/${WAVECUR_DID}.${RPDY}/rtofs_glo_2ds_${fext}${fh3_rtofs}_prog.nc

        if [ -s ${curfile1h} ]  && [ "${FLGHF}" = "T" ] ; then
          curfile=${curfile1h}
        elif [ -s ${curfile3h} ]; then
          curfile=${curfile3h}
          FLGHF='F'
        else
          echo ' '
          if [ "${FLGHF}" = "T" ] ; then
             curfile=${curfile1h}
          else 
             curfile=${curfile3h}
          fi
          set -x
          echo ' '
          echo '************************************** '
          echo "*** FATAL ERROR: NO CUR FILE $curfile ***  "
          echo '************************************** '
          echo ' '
          set_trace
          echo "FATAL ERROR - NO CURRENT FILE (RTOFS)"
          err=11;export err;${errchk}
          exit $err
          echo ' '
        fi

        if [ ${CFP_MP:-"NO"} = "YES" ]; then
          echo "$nm ${USHgfs}/wave_prnc_cur.sh $ymdh_rtofs $curfile $fhr_rtofs $FLGFIRST > cur_$ymdh_rtofs.out 2>&1" >> cmdfile
          nm=$(expr $nm + 1)
        else
          echo "${USHgfs}/wave_prnc_cur.sh $ymdh_rtofs $curfile $fhr_rtofs $FLGFIRST > cur_$ymdh_rtofs.out 2>&1" >> cmdfile
        fi

        if [ "${FLGFIRST}" = "T" ] ; then
            FLGFIRST='F'
        fi 

        if [ $fhr_rtofs -ge ${WAV_CUR_HF_FH} ] ; then
          NDATE_DT=${WAV_CUR_DT}
        fi
        ymdh_rtofs=$($NDATE $NDATE_DT $ymdh_rtofs)
      done

# Set number of processes for mpmd
      wavenproc=$(wc -l cmdfile | awk '{print $1}')
      wavenproc=$(echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS})))

      set +x
      echo ' '
      echo "   Executing the curr prnc cmdfile at : $(date)"
      echo '   ------------------------------------'
      echo ' '
      set_trace

      if [ $wavenproc -gt '1' ]
      then
        if [ ${CFP_MP:-"NO"} = "YES" ]; then
          ${wavempexec} -n ${wavenproc} ${wave_mpmd} cmdfile
        else
          ${wavempexec} ${wavenproc} ${wave_mpmd} cmdfile
        fi
        exit=$?
      else
        chmod 744 ./cmdfile
        ./cmdfile
        exit=$?
      fi

      if [ "$exit" != '0' ]
      then
        set +x
        echo ' '
        echo '********************************************'
        echo '*** CMDFILE FAILED IN CUR GENERATION   ***'
        echo '********************************************'
        echo '     See Details Below '
        echo ' '
        set_trace
      fi

      files=$(ls ${WAVECUR_DID}.* 2> /dev/null)

      if [ -z "$files" ]
      then
        set +x
        echo ' '
        echo '******************************************** '
        echo '*** FATAL ERROR : CANNOT FIND CURR FILES *** '
        echo '******************************************** '
        echo ' '
        echo "ABNORMAL EXIT: NO ${WAVECUR_FID}.* FILES FOUND"
        set_trace
        err=11;export err;${errchk}
      fi

      rm -f cur.${WAVECUR_FID}

      for file in $files
      do
        echo $file
        cat $file >> cur.${WAVECUR_FID}
      done

      cp -f cur.${WAVECUR_FID} ${COMOUT_WAVE_PREP}/${RUN}wave.${WAVECUR_FID}.$cycle.cur 

    else
      echo ' '
      echo " Current input is not perturbed, single cur file generated, skipping ${WAV_MOD_TAG}"
      echo ' '
    fi

  else
  
      echo ' '
      echo ' Current inputs not generated, this run did not request pre-processed currents '
      echo ' '

  fi

# --------------------------------------------------------------------------- #
# 4.  Ending output


# End of MWW3 preprocessor script ------------------------------------------- #
