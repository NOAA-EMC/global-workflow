#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         exwave_prep.sh
# Script description:  Creates output products from binary WW3 data
#
# Abstract: This is the preprocessor for the wave component in GFS.
#           It executes several scripts for preparing and creating input data
#           as follows:
#                                                                             #
#  wave_prnc_ice.sh     : preprocess ice fields.                              #
#  wave_prnc_cur.sh     : preprocess current fields.                          #
#                                                                             #
# COM inputs:                                                                 #
#  - ${COMIN_WAVE_PREP}/${RUN}.wave.mod_def.${grdID}                          #
#  - ${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_f#HHH_prog.nc                 #
#                                                                             #
# COM outputs:                                                                #
#  - ${COMOUT_WAVE_PREP}/${RUN}.wave.${WAVECUR_FID}.${cycle}.cur              #
#                                                                             #
#  Update record :                                                            #
#                                                                             #
# - Origination:                                               01-Mar-2007    #
#                                                                             #
#   WAV_MOD_ID and WAV_MOD_TAG replace modID. WAV_MOD_TAG                     #
#   is used for ensemble-specific I/O. For deterministic                      #
#   WAV_MOD_ID=WAV_MOD_TAG                                                    #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations

# 0.a Basic modes of operation

  # Set wave model ID tag to include member number
  # if ensemble; waveMEMB var empty in deterministic
  export WAV_MOD_TAG="${RUN}wave${waveMEMB}"

  mkdir outtmp

cat << EOF
HAS BEGUN on $(hostname)
Starting MWW3 PREPROCESSOR SCRIPT for ${WAV_MOD_TAG}

                      ********************************
                      *** WW3 PREPROCESSOR SCRIPT  ***
                      ********************************
                          PREP for wave component of NCEP coupled system
                          Wave component identifier : ${WAV_MOD_TAG}


EOF

  # 0.b Date and time stuff

  # Beginning time for outpupt may differ from SDATE if DOIAU=YES
  # Roll back ${IAU_FHROT} hours of DOIAU=YES
  IAU_FHROT=3
  if [[ "${DOIAU}" == "YES" ]]
  then
    WAVHINDH=$(( WAVHINDH + IAU_FHROT ))
  fi
  # Set time stamps for model start and output
  # For special case when IAU is on but this is an initial half cycle
  if [[ ${IAU_OFFSET} -eq 0 ]]; then
    ymdh_beg=${PDY}${cyc}
  else
    ymdh_beg=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - ${WAVHINDH} hours")
  fi
  time_beg=$(date --utc -d "${ymdh_beg:0:8} ${ymdh_beg:8:2}" +"%Y%m%d %H0000")
  ymdh_end=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} + ${FHMAX_WAV} hours")
  time_end=$(date --utc -d "${ymdh_end:0:8} ${ymdh_end:8:2}" +"%Y%m%d %H0000")
  ymdh_beg_out=${PDY}${cyc}
  time_beg_out="$(echo ${ymdh_beg_out} | cut -c1-8) $(echo ${ymdh_beg_out} | cut -c9-10)0000"

  # Restart file times (already has IAU_FHROT in WAVHINDH)
  RSTOFFSET=$(( ${WAVHCYC} - ${WAVHINDH} ))
  # Update restart time is added offset relative to model start
  RSTOFFSET=$(( ${RSTOFFSET} + ${RSTIOFF_WAV} ))
  ymdh_rst_ini=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} + ${RSTOFFSET} hours")
  RST2OFFSET=$(( DT_2_RST_WAV / 3600 ))
  # DT2 relative to first-first-cycle restart file
  ymdh_rst2_ini=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} + ${RST2OFFSET} hours")
  # First restart file for cycling
  time_rst_ini="$(echo ${ymdh_rst_ini} | cut -c1-8) $(echo ${ymdh_rst_ini} | cut -c9-10)0000"
  if [[ ${DT_1_RST_WAV} -eq 1 ]]; then
    time_rst1_end=${time_rst_ini}
  else
    RST1OFFSET=$(( DT_1_RST_WAV / 3600 ))
    ymdh_rst1_end=$(date --utc +%Y%m%d%H -d "${ymdh_rst_ini} + ${RST1OFFSET} hours")
    time_rst1_end="$(echo ${ymdh_rst1_end} | cut -c1-8) $(echo ${ymdh_rst1_end} | cut -c9-10)0000"
  fi
  # Second restart file for checkpointing
  if [[ "${RSTTYPE_WAV}" == "T" ]]; then
    time_rst2_ini="$(echo ${ymdh_rst2_ini} | cut -c1-8) $(echo ${ymdh_rst2_ini} | cut -c9-10)0000"
    time_rst2_end=${time_end}
  # Condition for gdas run or any other run when checkpoint stamp is > ymdh_end
    if [[ ${ymdh_rst2_ini} -ge ${ymdh_end} ]]; then
      ymdh_rst2_ini=$(date --utc +%Y%m%d%H -d "${ymdh_end} + 3 hours")
      time_rst2_ini="$(echo ${ymdh_rst2_ini} | cut -c1-8) $(echo ${ymdh_rst2_ini} | cut -c9-10)0000"
      time_rst2_end=${time_rst2_ini}
    fi
  else
    time_rst2_ini="$"
    time_rst2_end=
    DT_2_RST_WAV=
  fi
  cat << EOF

Times in wave model format :
----------------------------
   date / cycle  : ${PDY} ${cycle}
   starting time : ${time_beg}
   ending time   : ${time_end}

EOF

  # Script will run only if pre-defined NTASKS
  #     The actual work is distributed over these tasks.
  if [[ -z ${NTASKS} ]]
  then
    export err=1
    err_exit "Requires NTASKS to be set"
  fi

  # --------------------------------------------------------------------------- #
  # 1.  Get files that are used by most child scripts

  printf "Preparing input files :\n-----------------------\n"

  # 1.a Model definition files

  rm -f cmdfile
  touch cmdfile

  grdINP=''
  if [[ "${WW3ATMINP}" == 'YES' ]]; then
     grdINP="${grdINP} ${WAVEWND_FID}"
  fi
  if [[ "${WW3ICEINP}" == 'YES' ]]; then
     grdINP="${grdINP} ${WAVEICE_FID}"
  fi
  if [[ "${WW3CURINP}" == 'YES' ]]; then
     grdINP="${grdINP} ${WAVECUR_FID}"
  fi

  ifile=1

  for grdID in ${grdINP} ${waveGRD}
  do
    if [[ -f "${COMIN_WAVE_PREP}/${RUN}.wave.mod_def.${grdID}" ]]
    then
      echo " Mod def file for ${grdID} found in ${COMIN_WAVE_PREP}. copying ...."
      cpreq ${COMIN_WAVE_PREP}/${RUN}.wave.mod_def.${grdID} mod_def.${grdID}

    else
      export err=2
      err_exit "NO MODEL DEFINITION FILE"
    fi
  done

  # 1.b Netcdf Preprocessor template files
   if [[ "${WW3ATMINP}" == 'YES' ]]; then
      itype="${itype:-} wind"
   fi
   if [[ "${WW3ICEINP}" == 'YES' ]]; then
      itype="${itype:-} ice"
   fi
   if [[ "${WW3CURINP}" == 'YES' ]]; then
      itype="${itype:-} cur"
   fi

   for type in ${itype}
   do

     case ${type} in
       wind )
         grdID=${WAVEWND_FID}
       ;;
       ice )
         grdID=${WAVEICE_FID}
       ;;
       cur )
         grdID=${WAVECUR_FID}
       ;;
       * )
         export err=3
         err_exit 'Input type not yet implemented'
       ;;
     esac

     if [[ -f ${PARMgfs}/wave/ww3_prnc.${type}.${grdID}.inp.tmpl ]]
     then
       cpreq ${PARMgfs}/wave/ww3_prnc.${type}.${grdID}.inp.tmpl .
     fi

     if [[ -f ww3_prnc.${type}.${grdID}.inp.tmpl ]]
     then
       printf"\n   ww3_prnc.${type}.${grdID}.inp.tmpl copied (${PARMgfs}/wave).\n"
     else
       export err=4
       err_exit "NO TEMPLATE FILE ww3_prnc.${type}.${grdID}.inp.tmpl"
     fi
   done

# --------------------------------------------------------------------------- #
# ICEC processing

  if [[ "${WW3ICEINP}" == 'YES' ]]; then

# --------------------------------------------------------------------------- #
# 2. Ice pre - processing

# 2.a Check if ice input is perturbed (number of inputs equal to number of wave
#     ensemble members
    if [[ "${RUNMEM}" == "-1" || "${WW3ICEIENS}" == "T" || "${waveMEMB}" == "00" ]]
    then

      ${USHgfs}/wave_prnc_ice.sh > wave_prnc_ice.out && true
      ERR=$?

      if [[ -d ice || ${ERR} -ne 0 ]]
      then
        sed "s/^/wave_prnc_ice.out : /g" wave_prnc_ice.out
        echo ' '
        if [[ ${ERR} -ne 0 ]]; then
           export err=${ERR}
        else
           export err=5
        fi
        err_exit "ice field not generated"
      else
        mv -f wave_prnc_ice.out ${DATA}/outtmp
        printf "\n      Ice field unpacking successful.\n"
      fi

    else
      echo ' '
      echo "WARNING: Ice input is not perturbed, single ice file generated, skipping ${WAV_MOD_TAG}"
      echo ' '
    fi
  else
      echo ' '
      echo "WARNING: No input ice file generated, this run did not request pre-processed ice data"
      echo ' '
  fi

# --------------------------------------------------------------------------- #
# WIND processing
  if [[ "${WW3ATMINP}" == 'YES' ]]; then

    export err=6
    err_exit "Not set-up to preprocess wind"
  fi

#-------------------------------------------------------------------
# 3.  Process current fields

  if [[ "${WW3CURINP}" == 'YES' ]]; then

# Get into single file
    if [[ "${RUNMEM}" == "-1" || "${WW3CURIENS}" == "T" || "${waveMEMB}" == "00" ]]
    then

      printf "\n   Concatenate binary current fields ...\n"

# Prepare files for cfp process
      rm -f cmdfile
      touch cmdfile
      chmod 744 cmdfile

      BDATE=$(date --utc +%Y%m%d%H -d "${RPDY}00 - 24 hours")
      bPDY=${BDATE:0:8}

      ymdh_rtofs=${RPDY}00 # RTOFS runs once daily use ${PDY}00
      if [[ ${ymdh_beg} -lt ${ymdh_rtofs} ]]; then
         #If the start time is before the first hour of RTOFS, use the previous cycle
         export RPDY=${bPDY}
      fi
      #Set the first time for RTOFS files to be the beginning time of simulation
      ymdh_rtofs=${ymdh_beg}

      if [[  "${FHMAX_WAV_CUR}" -le 72 ]]; then
        rtofsfile1="${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_f024_prog.nc"
        rtofsfile2="${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_f048_prog.nc"
        rtofsfile3="${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_f072_prog.nc"
        if [[ ! -f ${rtofsfile1} ]] || [[ ! -f ${rtofsfile2} ]] || [[ ! -f ${rtofsfile3} ]]; then
           #Needed current files are not available, so use RTOFS from previous day 
           export RPDY=${bPDY}
        fi
      else
        rtofsfile1="${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_f096_prog.nc"
        rtofsfile2="${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_f120_prog.nc"
        rtofsfile3="${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_f144_prog.nc"
        rtofsfile4="${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_f168_prog.nc"
        rtofsfile5="${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_f192_prog.nc"
        if [[ ! -f ${rtofsfile1} ]] || [[ ! -f ${rtofsfile2} ]] || [[ ! -f ${rtofsfile3} ]] ||
            [[ ! -f ${rtofsfile4} ]] || [[ ! -f ${rtofsfile5} ]]; then
            #Needed current files are not available, so use RTOFS from previous day 
            export RPDY=${bPDY}
        fi
      fi

      ymdh_end_rtofs=$(date --utc +%Y%m%d%H -d "${RPDY}00 + ${FHMAX_WAV_CUR} hours")
      if [[ ${ymdh_end} -lt ${ymdh_end_rtofs} ]]; then
         ymdh_end_rtofs=${ymdh_end}
      fi

      DATE_DT=${WAV_CUR_HF_DT}
      FLGHF='T'
      FLGFIRST='T'
      fext='f'

      if [[ ${CFP_MP:-"NO"} == "YES" ]]; then
         # Counter for MP CFP
         nm=0
      fi
      while [[ ${ymdh_rtofs} -le ${ymdh_end_rtofs} ]]
      do
        # Timing has to be made relative to the single 00z RTOFS cycle for RTOFS PDY (RPDY)
        # Start at first fhr for
        fhr_rtofs=$(${NHOUR} ${ymdh_rtofs} ${RPDY}00)
        fh3_rtofs=$(printf "%03d" "${fhr_rtofs#0}")

        curfile1h=${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_${fext}${fh3_rtofs}_prog.nc
        curfile3h=${COMINrtofs}/rtofs.${RPDY}/rtofs_glo_2ds_${fext}${fh3_rtofs}_prog.nc

        if [[ -s ${curfile1h} && "${FLGHF}" == "T" ]] ; then
          curfile=${curfile1h}
        elif [[ -s ${curfile3h} ]]; then
          curfile=${curfile3h}
          FLGHF='F'
        else
          echo ' '
          if [[ "${FLGHF}" == "T" ]] ; then
             curfile=${curfile1h}
          else
             curfile=${curfile3h}
          fi
          export err=11
          err_exit "NO CURRENT FILE (RTOFS): ${curfile}"
        fi

        if [[ ${CFP_MP:-"NO"} == "YES" ]]; then
          echo "${nm} ${USHgfs}/wave_prnc_cur.sh ${ymdh_rtofs} ${curfile} ${fhr_rtofs} ${FLGFIRST} > cur_${ymdh_rtofs}.out 2>&1" >> cmdfile
          nm=$(expr ${nm} + 1)
        else
          echo "${USHgfs}/wave_prnc_cur.sh ${ymdh_rtofs} ${curfile} ${fhr_rtofs} ${FLGFIRST} > cur_${ymdh_rtofs}.out 2>&1" >> cmdfile
        fi

        if [[ "${FLGFIRST}" == "T" ]] ; then
            FLGFIRST='F'
        fi

        if [[ ${fhr_rtofs} -ge ${WAV_CUR_HF_FH} ]] ; then
          DATE_DT=${WAV_CUR_DT}
        fi
        ymdh_rtofs=$(date --utc +%Y%m%d%H -d "${ymdh_rtofs} + ${DATE_DT} hours")
      done

# Set number of processes for mpmd
      wavenproc=$(wc -l < cmdfile)
      wavenproc=$(echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS})))

      printf "\n   Executing the curr prnc cmdfile at : %s\n   ------------------------------------\n" "$(date)"

      if [[ ${wavenproc} -gt 1 ]]
      then
        if [[ ${CFP_MP:-"NO"} == "YES" ]]; then
          ${wavempexec} -n "${wavenproc}" "${wave_mpmd}" cmdfile && true
        else
          ${wavempexec} "${wavenproc}" "${wave_mpmd}" cmdfile && true
        fi
        export stat=$?
      else
        chmod 744 ./cmdfile
        ./cmdfile
        export stat=$?
      fi

      if [[ ${stat} -ne 0 ]]
      then
        set +x
        echo ' '
        echo '********************************************'
        echo '*** CMDFILE FAILED IN CUR GENERATION   ***'
        echo '********************************************'
        echo '     See Details Below '
        # TODO: What details should be expected?
        echo ' '
        # TODO: Should this raise a fatal error whether or not rtofs files are found?
        set_trace
      fi

      files=$(ls rtofs.* 2> /dev/null)

      if [[ -z "${files}" ]]
      then
        export err=11
        err_exit "NO ${WAVECUR_FID}.* FILES FOUND"
      fi

      rm -f cur.${WAVECUR_FID}

      for file in ${files}
      do
        echo ${file}
        cat ${file} >> cur.${WAVECUR_FID}
      done

      cpfs cur.${WAVECUR_FID} ${COMOUT_WAVE_PREP}/${RUN}.wave.${WAVECUR_FID}.${cycle}.cur

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
