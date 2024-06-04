#! /usr/bin/env bash

# Disable variable not used warnings
# shellcheck disable=SC2034
FV3_postdet() {
  echo "SUB ${FUNCNAME[0]}: Entering for RUN = ${RUN}"

  echo "warm_start = ${warm_start}"
  echo "RERUN = ${RERUN}"

  # cold start case
  if [[ "${warm_start}" == ".false." ]]; then

    # Create an array of chgres-ed FV3 files
    local fv3_input_files tile_files fv3_input_file
    fv3_input_files=(gfs_ctrl.nc)
    tile_files=(gfs_data sfc_data)
    local nn tt
    for (( nn = 1; nn <= ntiles; nn++ )); do
      for tt in "${tile_files[@]}"; do
        fv3_input_files+=("${tt}.tile${nn}.nc")
      done
    done

    echo "Copying FV3 cold start files for 'RUN=${RUN}' at '${current_cycle}' from '${COMIN_ATMOS_INPUT}'"
    for fv3_input_file in "${fv3_input_files[@]}"; do
      ${NCP} "${COMIN_ATMOS_INPUT}/${fv3_input_file}" "${DATA}/INPUT/${fv3_input_file}" \
      || ( echo "FATAL ERROR: Unable to copy FV3 IC, ABORT!"; exit 1 )
    done

  # warm start case
  elif [[ "${warm_start}" == ".true." ]]; then

    # Create an array of FV3 restart files
    local fv3_restart_files tile_files fv3_restart_file restart_file
    fv3_restart_files=(coupler.res fv_core.res.nc)
    tile_files=(fv_core.res fv_srf_wnd.res fv_tracer.res phy_data sfc_data ca_data)
    local nn tt
    for (( nn = 1; nn <= ntiles; nn++ )); do
      for tt in "${tile_files[@]}"; do
        fv3_restart_files+=("${tt}.tile${nn}.nc")
      done
    done

    # Determine restart date and directory containing restarts
    local restart_date restart_dir
    if [[ "${RERUN}" == "YES" ]]; then
      restart_date="${RERUN_DATE}"
      restart_dir="${DATArestart}/FV3_RESTART"
    else  # "${RERUN}" == "NO"
      restart_date="${model_start_date_current_cycle}"
      restart_dir="${COMIN_ATMOS_RESTART_PREV}"
    fi

    echo "Copying FV3 restarts for 'RUN=${RUN}' at '${restart_date}' from '${restart_dir}'"
    for fv3_restart_file in "${fv3_restart_files[@]}"; do
      restart_file="${restart_date:0:8}.${restart_date:8:2}0000.${fv3_restart_file}"
      ${NCP} "${restart_dir}/${restart_file}" "${DATA}/INPUT/${fv3_restart_file}" \
      || ( echo "FATAL ERROR: Unable to copy FV3 IC, ABORT!"; exit 1 )
    done

    if [[ "${RERUN}" == "YES" ]]; then

      local restart_fhr
      restart_fhr=$(nhour "${RERUN_DATE}" "${current_cycle}")
      IAU_FHROT=$((IAU_OFFSET + restart_fhr))
      if [[ "${DOIAU}" == "YES" ]]; then
        IAUFHRS=-1
        IAU_DELTHRS=0
        IAU_INC_FILES="''"
      fi

    else  # "${RERUN}" == "NO"

      # Replace sfc_data with sfcanl_data restart files from current cycle (if found)
      local nn
      for (( nn = 1; nn <= ntiles; nn++ )); do
        if [[ -f "${COMOUT_ATMOS_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.sfcanl_data.tile${nn}.nc" ]]; then
          rm -f "${DATA}/INPUT/sfc_data.tile${nn}.nc"
          ${NCP} "${COMOUT_ATMOS_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.sfcanl_data.tile${nn}.nc" \
                 "${DATA}/INPUT/sfc_data.tile${nn}.nc"
        else
          echo "'sfcanl_data.tile1.nc' not found in '${COMOUT_ATMOS_RESTART}', using 'sfc_data.tile1.nc'"
          break
        fi
      done

      # Need a coupler.res that is consistent with the model start time
      if [[ "${DOIAU}" == "YES" ]]; then
        local model_start_time="${previous_cycle}"
      else
        local model_start_time="${current_cycle}"
      fi
      local model_current_time="${model_start_date_current_cycle}"
      rm -f "${DATA}/INPUT/coupler.res"
      cat >> "${DATA}/INPUT/coupler.res" << EOF
      3        (Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)
      ${model_start_time:0:4}  ${model_start_time:4:2}  ${model_start_time:6:2}  ${model_start_time:8:2}  0  0        Model start time: year, month, day, hour, minute, second
      ${model_current_time:0:4}  ${model_current_time:4:2}  ${model_current_time:6:2}  ${model_current_time:8:2}  0  0        Current model time: year, month, day, hour, minute, second
EOF

      # Create a array of increment files
      local inc_files inc_file iaufhrs iaufhr
      if [[ "${DOIAU}" == "YES" ]]; then
        # create an array of inc_files for each IAU hour
        IFS=',' read -ra iaufhrs <<< "${IAUFHRS}"
        inc_files=()
        delimiter=""
        IAU_INC_FILES=""
        for iaufhr in "${iaufhrs[@]}"; do
          if (( iaufhr == 6 )); then
            inc_file="atminc.nc"
          else
            inc_file="atmi$(printf %03i "${iaufhr}").nc"
          fi
          inc_files+=("${inc_file}")
          IAU_INC_FILES="${IAU_INC_FILES}${delimiter}'${inc_file}'"
          delimiter=","
        done
      else  # "${DOIAU}" == "NO"
        inc_files=("atminc.nc")
        read_increment=".true."
        res_latlon_dynamics="atminc.nc"
      fi

      local increment_file
      for inc_file in "${inc_files[@]}"; do
        increment_file="${COMIN_ATMOS_ANALYSIS}/${RUN}.t${cyc}z.${PREFIX_ATMINC}${inc_file}"
        if [[ -f "${increment_file}" ]]; then
          ${NCP} "${increment_file}" "${DATA}/INPUT/${inc_file}"
        else
          echo "FATAL ERROR: missing increment file '${increment_file}', ABORT!"
          exit 1
        fi
      done

    fi  # if [[ "${RERUN}" == "YES" ]]; then

  fi  # if [[ "${warm_start}" == ".true." ]]; then

  # If doing IAU, change forecast hours
  if [[ "${DOIAU:-}" == "YES" ]]; then
    FHMAX=$((FHMAX + 6))
    if (( FHMAX_HF > 0 )); then
      FHMAX_HF=$((FHMAX_HF + 6))
    fi
  fi

  # If warm starting from restart files, set the following flags
  if [[ "${warm_start}" == ".true." ]]; then

    # start from restart file
    nggps_ic=".false."
    ncep_ic=".false."
    external_ic=".false."
    mountain=".true."

    # restarts contain non-hydrostatic state
    [[ "${TYPE}" == "nh" ]] && make_nh=".false."

    # do not pre-condition the solution
    na_init=0

  fi  # warm_start == .true.

  cd "${DATA}" || exit 1
  if [[ "${QUILTING}" = ".true." ]] && [[ "${OUTPUT_GRID}" = "gaussian_grid" ]]; then
    local FH2 FH3
    for fhr in ${FV3_OUTPUT_FH}; do
      FH3=$(printf %03i "${fhr}")
      FH2=$(printf %02i "${fhr}")
      ${NLN} "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.atmf${FH3}.nc" "atmf${FH3}.nc"
      ${NLN} "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.sfcf${FH3}.nc" "sfcf${FH3}.nc"
      ${NLN} "${COMOUT_ATMOS_HISTORY}/${RUN}.t${cyc}z.atm.logf${FH3}.txt" "log.atm.f${FH3}"
      if [[ "${WRITE_DOPOST}" == ".true." ]]; then
        ${NLN} "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.grb2f${FH3}" "GFSPRS.GrbF${FH2}"
        ${NLN} "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.sfluxgrbf${FH3}.grib2" "GFSFLX.GrbF${FH2}"
        if [[ "${DO_NEST:-NO}" == "YES" ]] ; then
          ${NLN} "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.nest.grb2f${FH3}" "GFSPRS.GrbF${FH2}.nest02"
          ${NLN} "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.nest.sfluxgrbf${FH3}.grib2" "GFSFLX.GrbF${FH2}.nest02"
        fi
      fi
    done
  else  # TODO: Is this even valid anymore?
    local nn
    for (( nn = 1; nn <= ntiles; nn++ )); do
      ${NLN} "nggps2d.tile${nn}.nc"       "${COMOUT_ATMOS_HISTORY}/nggps2d.tile${nn}.nc"
      ${NLN} "nggps3d.tile${nn}.nc"       "${COMOUT_ATMOS_HISTORY}/nggps3d.tile${nn}.nc"
      ${NLN} "grid_spec.tile${nn}.nc"     "${COMOUT_ATMOS_HISTORY}/grid_spec.tile${nn}.nc"
      ${NLN} "atmos_static.tile${nn}.nc"  "${COMOUT_ATMOS_HISTORY}/atmos_static.tile${nn}.nc"
      ${NLN} "atmos_4xdaily.tile${nn}.nc" "${COMOUT_ATMOS_HISTORY}/atmos_4xdaily.tile${nn}.nc"
    done
  fi
}

FV3_nml() {
  # namelist output for a certain component
  echo "SUB ${FUNCNAME[0]}: Creating name lists and model configure file for FV3"

  source "${USHgfs}/parsing_namelists_FV3.sh"
  source "${USHgfs}/parsing_model_configure_FV3.sh"

  # Call the appropriate namelist functions
  if [[ "${DO_NEST:-NO}" == "YES" ]] ; then
    source "${USHgfs}/parsing_namelists_FV3_nest.sh"
    FV3_namelists_nest global
    FV3_namelists_nest nest
  else
    FV3_namelists
  fi
  FV3_model_configure

  echo "SUB ${FUNCNAME[0]}: FV3 name lists and model configure file created"
}

FV3_out() {
  echo "SUB ${FUNCNAME[0]}: copying output data for FV3"

  # Copy configuration files
  if [[ "${RUN}" == "gfs" || "${RUN}" == "gefs" ]]; then
    ${NCP} "${DATA}/input.nml" "${COMOUT_CONF}/ufs.input.nml"
    ${NCP} "${DATA}/model_configure" "${COMOUT_CONF}/ufs.model_configure"
    ${NCP} "${DATA}/ufs.configure" "${COMOUT_CONF}/ufs.ufs.configure"
    ${NCP} "${DATA}/diag_table" "${COMOUT_CONF}/ufs.diag_table"
  fi

  # Create an array of fv3 restart files
  local fv3_restart_files tile_files fv3_restart_file restart_file
  fv3_restart_files=(coupler.res fv_core.res.nc)
  tile_files=(fv_core.res fv_srf_wnd.res fv_tracer.res phy_data sfc_data ca_data)
  local nn tt
  for (( nn = 1; nn <= ntiles; nn++ )); do
    for tt in "${tile_files[@]}"; do
      fv3_restart_files+=("${tt}.tile${nn}.nc")
    done
  done

  # Copy restarts in the assimilation window for RUN=gdas|enkfgdas|enkfgfs
  if [[ "${RUN}" =~ "gdas" || "${RUN}" == "enkfgfs" ]]; then
    local restart_date
    restart_date=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${restart_interval} hours" +%Y%m%d%H)
    while (( restart_date <= forecast_end_cycle )); do
      echo "Copying FV3 restarts for 'RUN=${RUN}' at ${restart_date}"
      for fv3_restart_file in "${fv3_restart_files[@]}"; do
        restart_file="${restart_date:0:8}.${restart_date:8:2}0000.${fv3_restart_file}"
        ${NCP} "${DATArestart}/FV3_RESTART/${restart_file}" \
               "${COMOUT_ATMOS_RESTART}/${restart_file}"
      done
      restart_date=$(date --utc -d "${restart_date:0:8} ${restart_date:8:2} + ${restart_interval} hours" +%Y%m%d%H)
    done
  fi

  # Copy the final restart files at the end of the forecast segment
  # The final restart written at the end of the forecast does not include the valid date
  # TODO: verify the above statement since RM found that it did!
  # TODO: For other components, this is only for gfs/gefs - check to see if this should also have this
  if [[ "${COPY_FINAL_RESTARTS}" == "YES" ]]; then
    echo "Copying FV3 restarts for 'RUN=${RUN}' at the end of the forecast segment: ${forecast_end_cycle}"
    for fv3_restart_file in "${fv3_restart_files[@]}"; do
      restart_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.${fv3_restart_file}"
      ${NCP} "${DATArestart}/FV3_RESTART/${restart_file}" \
             "${COMOUT_ATMOS_RESTART}/${restart_file}"
    done
  fi
  echo "SUB ${FUNCNAME[0]}: Output data for FV3 copied"
}

# Disable variable not used warnings
# shellcheck disable=SC2034
WW3_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking input data for WW3"

  local ww3_grid
  # Copy initial condition files:
  if [[ "${warm_start}" == ".true." ]]; then
    local restart_date restart_dir
    if [[ "${RERUN}" == "YES" ]]; then
      restart_date="${RERUN_DATE}"
      restart_dir="${DATArestart}/WW3_RESTART"
    else
      restart_date="${model_start_date_current_cycle}"
      restart_dir="${COMIN_WAVE_RESTART_PREV}"
    fi
    echo "Copying WW3 restarts for 'RUN=${RUN}' at '${restart_date}' from '${restart_dir}'"
    local ww3_restart_file
    for ww3_grid in ${waveGRD} ; do
      ww3_restart_file="${restart_dir}/${restart_date:0:8}.${restart_date:8:2}0000.restart.${ww3_grid}"
      if [[ ! -f "${ww3_restart_file}" ]]; then
        echo "WARNING: WW3 restart file '${ww3_restart_file}' not found for warm_start='${warm_start}', will start from rest!"
        if [[ "${RERUN}" == "YES" ]]; then
          # In the case of a RERUN, the WW3 restart file is required
          echo "FATAL ERROR: WW3 restart file '${ww3_restart_file}' not found for RERUN='${RERUN}', ABORT!"
          exit 1
        fi
      fi
      if [[ "${waveMULTIGRID}" == ".true." ]]; then
        ${NCP} "${ww3_restart_file}" "${DATA}/restart.${ww3_grid}" \
        || ( echo "FATAL ERROR: Unable to copy WW3 IC, ABORT!"; exit 1 )
      else
        ${NCP} "${ww3_restart_file}" "${DATA}/restart.ww3" \
        || ( echo "FATAL ERROR: Unable to copy WW3 IC, ABORT!"; exit 1 )
      fi
    done
  else  # cold start
    echo "WW3 will start from rest!"
  fi  # [[ "${warm_start}" == ".true." ]]

  # Link output files
  local wavprfx="${RUN}wave${WAV_MEMBER:-}"
  if [[ "${waveMULTIGRID}" == ".true." ]]; then
    ${NLN} "${COMOUT_WAVE_HISTORY}/${wavprfx}.log.mww3.${PDY}${cyc}" "log.mww3"
    for ww3_grid in ${waveGRD}; do
      ${NLN} "${COMOUT_WAVE_HISTORY}/${wavprfx}.log.${ww3_grid}.${PDY}${cyc}" "log.${ww3_grid}"
    done
  else
    ${NLN} "${COMOUT_WAVE_HISTORY}/${wavprfx}.log.${waveGRD}.${PDY}${cyc}" "log.ww3"
  fi

  # Loop for gridded output (uses FHINC)
  local fhr vdate FHINC ww3_grid
  fhr=${FHMIN_WAV}
  fhinc=${FHOUT_WAV}
  while (( fhr <= FHMAX_WAV )); do
    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d.%H0000)
    if [[ "${waveMULTIGRID}" == ".true." ]]; then
      for ww3_grid in ${waveGRD} ; do
        ${NLN} "${COMOUT_WAVE_HISTORY}/${wavprfx}.out_grd.${ww3_grid}.${vdate}" "${DATA}/${vdate}.out_grd.${ww3_grid}"
      done
    else
      ${NLN} "${COMOUT_WAVE_HISTORY}/${wavprfx}.out_grd.${waveGRD}.${vdate}" "${DATA}/${vdate}.out_grd.ww3"
    fi
    if (( FHMAX_HF_WAV > 0 && FHOUT_HF_WAV > 0 && fhr < FHMAX_HF_WAV )); then
      fhinc=${FHOUT_HF_WAV}
    fi
    fhr=$((fhr + fhinc))
  done

  # Loop for point output (uses DTPNT)
  fhr=${FHMIN_WAV}
  fhinc=${FHINCP_WAV}
  while (( fhr <= FHMAX_WAV )); do
    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d.%H0000)
    if [[ "${waveMULTIGRID}" == ".true." ]]; then
      ${NLN} "${COMOUT_WAVE_HISTORY}/${wavprfx}.out_pnt.${waveuoutpGRD}.${vdate}" "${DATA}/${vdate}.out_pnt.${waveuoutpGRD}"
    else
      ${NLN} "${COMOUT_WAVE_HISTORY}/${wavprfx}.out_pnt.${waveuoutpGRD}.${vdate}" "${DATA}/${vdate}.out_pnt.ww3"
    fi
    fhr=$((fhr + fhinc))
  done
}

WW3_nml() {
  echo "SUB ${FUNCNAME[0]}: Copying input files for WW3"
  source "${USHgfs}/parsing_namelists_WW3.sh"
  WW3_namelists
}

WW3_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for WW3"
  # TODO: Need to add logic to copy restarts from DATArestart/WW3_RESTART to COMOUT_WAVE_RESTART
}


CPL_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for general cpl fields"
  if [[ "${esmf_profile:-}" == ".true." ]]; then
    ${NCP} "${DATA}/ESMF_Profile.summary" "${COMOUT_ATMOS_HISTORY}/ESMF_Profile.summary"
  fi
}

MOM6_postdet() {
  echo "SUB ${FUNCNAME[0]}: MOM6 after run type determination"

  local restart_dir restart_date
  if [[ "${RERUN}" == "YES" ]]; then
    restart_dir="${DATArestart}/MOM6_RESTART"
    restart_date="${RERUN_DATE}"
  else  # "${RERUN}" == "NO"
    restart_dir="${COMIN_OCEAN_RESTART_PREV}"
    restart_date="${model_start_date_current_cycle}"
  fi

  # Copy MOM6 ICs
  ${NCP} "${restart_dir}/${restart_date:0:8}.${restart_date:8:2}0000.MOM.res.nc" "${DATA}/INPUT/MOM.res.nc" \
  || ( echo "FATAL ERROR: Unable to copy MOM6 IC, ABORT!"; exit 1 )
  case ${OCNRES} in
    "025")
      local nn
      for (( nn = 1; nn <= 4; nn++ )); do
        if [[ -f "${restart_dir}/${restart_date:0:8}.${restart_date:8:2}0000.MOM.res_${nn}.nc" ]]; then
          ${NCP} "${restart_dir}/${restart_date:0:8}.${restart_date:8:2}0000.MOM.res_${nn}.nc" "${DATA}/INPUT/MOM.res_${nn}.nc" \
          || ( echo "FATAL ERROR: Unable to copy MOM6 IC, ABORT!"; exit 1 )
        fi
      done
    ;;
    *) ;;
  esac

  # Copy increment (only when RERUN=NO)
  if [[ "${RERUN}" == "NO" ]]; then
    if [[ "${DO_JEDIOCNVAR:-NO}" == "YES" ]]; then
      ${NCP} "${COMIN_OCEAN_ANALYSIS}/${RUN}.t${cyc}z.ocninc.nc" "${DATA}/INPUT/mom6_increment.nc" \
      || ( echo "FATAL ERROR: Unable to copy MOM6 increment, ABORT!"; exit 1 )
    fi

    # GEFS perturbations
    # TODO if [[ $RUN} == "gefs" ]] block maybe be needed
    #     to ensure it does not interfere with the GFS when ensemble is updated in the GFS
    if (( MEMBER > 0 )) && [[ "${ODA_INCUPD:-False}" == "True" ]]; then
      ${NCP} "${COMIN_OCEAN_RESTART_PREV}/${restart_date:0:8}.${restart_date:0:8}0000.mom6_increment.nc" "${DATA}/INPUT/mom6_increment.nc" \
      || ( echo "FATAL ERROR: Unable to copy ensemble MOM6 increment, ABORT!"; exit 1 )
    fi
  fi  # if [[ "${RERUN}" == "NO" ]]; then

  # Link output files
  if [[ "${RUN}" =~ "gfs" || "${RUN}" == "gefs" ]]; then  # Link output files for RUN=gfs|enkfgfs|gefs

    # Looping over MOM6 output hours
    local fhr fhr3 last_fhr interval midpoint vdate vdate_mid source_file dest_file
    for fhr in ${MOM6_OUTPUT_FH}; do
      fhr3=$(printf %03i "${fhr}")

      if [[ -z ${last_fhr:-} ]]; then
        last_fhr=${fhr}
        continue
      fi

      (( interval = fhr - last_fhr ))
      (( midpoint = last_fhr + interval/2 ))

      vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
      vdate_mid=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${midpoint} hours" +%Y%m%d%H)

      # Native model output uses window midpoint in the filename, but we are mapping that to the end of the period for COM
      source_file="ocn_${vdate_mid:0:4}_${vdate_mid:4:2}_${vdate_mid:6:2}_${vdate_mid:8:2}.nc"
      dest_file="${RUN}.ocean.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
      ${NLN} "${COMOUT_OCEAN_HISTORY}/${dest_file}" "${DATA}/MOM6_OUTPUT/${source_file}"

      # Daily output
      if (( fhr > 0 & fhr % 24 == 0 )); then
        source_file="ocn_daily_${vdate:0:4}_${vdate:4:2}_${vdate:6:2}.nc"
        dest_file="${RUN}.ocean.t${cyc}z.daily.f${fhr3}.nc"
        ${NLN} "${COMOUT_OCEAN_HISTORY}/${dest_file}" "${DATA}/MOM6_OUTPUT/${source_file}"
      fi

      last_fhr=${fhr}

    done

  elif [[ "${RUN}" =~ "gdas" ]]; then  # Link output files for RUN=gdas|enkfgdas

    # Save (instantaneous) MOM6 backgrounds
    local fhr3 vdatestr
    for fhr in ${MOM6_OUTPUT_FH}; do
      fhr3=$(printf %03i "${fhr}")
      vdatestr=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y_%m_%d_%H)
      ${NLN} "${COMOUT_OCEAN_HISTORY}/${RUN}.ocean.t${cyc}z.inst.f${fhr3}.nc" "${DATA}/MOM6_OUTPUT/ocn_da_${vdatestr}.nc"
    done
  fi

  echo "SUB ${FUNCNAME[0]}: MOM6 input data linked/copied"

}

MOM6_nml() {
  echo "SUB ${FUNCNAME[0]}: Creating name list for MOM6"
  source "${USHgfs}/parsing_namelists_MOM6.sh"
  MOM6_namelists
}

MOM6_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for MOM6"

  # Copy MOM_input from DATA to COMOUT_CONF after the forecast is run (and successfull)
  ${NCP} "${DATA}/INPUT/MOM_input" "${COMOUT_CONF}/ufs.MOM_input"

  # Create a list of MOM6 restart files
  # Coarser than 1/2 degree has a single MOM restart
  local mom6_restart_files mom6_restart_file restart_file
  mom6_restart_files=(MOM.res.nc)
  # 1/4 degree resolution has 3 additional restarts
  case "${OCNRES}" in
    "025")
      local nn
      for (( nn = 1; nn <= 3; nn++ )); do
        mom6_restart_files+=("MOM.res_${nn}.nc")
      done
      ;;
    *) ;;
  esac

  # Copy MOM6 restarts at the end of the forecast segment to COM for RUN=gfs|gefs
  if [[ "${COPY_FINAL_RESTARTS}" == "YES" ]]; then
    local restart_file
    if [[ "${RUN}" == "gfs" || "${RUN}" == "gefs" ]]; then
      echo "Copying MOM6 restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
      for mom6_restart_file in "${mom6_restart_files[@]}"; do
        restart_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.${mom6_restart_file}"
        ${NCP} "${DATArestart}/MOM6_RESTART/${restart_file}" \
               "${COMOUT_OCEAN_RESTART}/${restart_file}"
      done
    fi
  fi

  # Copy restarts for the next cycle for RUN=gdas|enkfgdas|enkfgfs
  if [[ "${RUN}" =~ "gdas" || "${RUN}" == "enkfgfs" ]]; then
    local restart_date
    restart_date="${model_start_date_next_cycle}"
    echo "Copying MOM6 restarts for 'RUN=${RUN}' at ${restart_date}"
    for mom6_restart_file in "${mom6_restart_files[@]}"; do
      restart_file="${restart_date:0:8}.${restart_date:8:2}0000.${mom6_restart_file}"
      ${NCP} "${DATArestart}/MOM6_RESTART/${restart_file}" \
             "${COMOUT_OCEAN_RESTART}/${restart_file}"
    done
  fi
}

CICE_postdet() {
  echo "SUB ${FUNCNAME[0]}: CICE after run type determination"

  local restart_date cice_restart_file
  if [[ "${RERUN}" == "YES" ]]; then
    restart_date="${RERUN_DATE}"
    local seconds
    seconds=$(to_seconds "${restart_date:8:2}0000")  # convert HHMMSS to seconds
    cice_restart_file="${DATArestart}/CICE_RESTART/cice_model.res.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
  else  # "${RERUN}" == "NO"
    restart_date="${model_start_date_current_cycle}"
    cice_restart_file="${COMIN_ICE_RESTART_PREV}/${restart_date:0:8}.${restart_date:8:2}0000.cice_model.res.nc"
    if [[ "${DO_JEDIOCNVAR:-NO}" == "YES" ]]; then
      cice_restart_file="${COMIN_ICE_ANALYSIS}/${restart_date:0:8}.${restart_date:8:2}0000.cice_model_anl.res.nc"
    fi
  fi

  # Copy CICE ICs
  ${NCP} "${cice_restart_file}" "${DATA}/cice_model.res.nc" \
  || ( echo "FATAL ERROR: Unable to copy CICE IC, ABORT!"; exit 1 )

  # Link iceh_ic file to COM.  This is the initial condition file from CICE (f000)
  # TODO: Is this file needed in COM? Is this going to be used for generating any products?
  local vdate seconds vdatestr fhr fhr3 interval last_fhr
  seconds=$(to_seconds "${model_start_date_current_cycle:8:2}0000")  # convert HHMMSS to seconds
  vdatestr="${model_start_date_current_cycle:0:4}-${model_start_date_current_cycle:4:2}-${model_start_date_current_cycle:6:2}-${seconds}"
  ${NLN} "${COMOUT_ICE_HISTORY}/${RUN}.ice.t${cyc}z.ic.nc" "${DATA}/CICE_OUTPUT/iceh_ic.${vdatestr}.nc"

  # Link CICE forecast output files from DATA/CICE_OUTPUT to COM
  local source_file dest_file
  for fhr in ${CICE_OUTPUT_FH}; do
    fhr3=$(printf %03i "${fhr}")

    if [[ -z ${last_fhr:-} ]]; then
      last_fhr=${fhr}
      continue
    fi

    (( interval = fhr - last_fhr ))

    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
    seconds=$(to_seconds "${vdate:8:2}0000")  # convert HHMMSS to seconds
    vdatestr="${vdate:0:4}-${vdate:4:2}-${vdate:6:2}-${seconds}"

    if [[ "${RUN}" =~ "gfs" || "${RUN}" =~ "gefs" ]]; then
      source_file="iceh_$(printf "%0.2d" "${interval}")h.${vdatestr}.nc"
      dest_file="${RUN}.ice.t${cyc}z.${interval}hr_avg.f${fhr3}.nc"
    elif [[ "${RUN}" =~ "gdas" ]]; then
      source_file="iceh_inst.${vdatestr}.nc"
      dest_file="${RUN}.ice.t${cyc}z.inst.f${fhr3}.nc"
    fi
    ${NLN} "${COMOUT_ICE_HISTORY}/${dest_file}" "${DATA}/CICE_OUTPUT/${source_file}"

    last_fhr=${fhr}
  done

}

CICE_nml() {
  echo "SUB ${FUNCNAME[0]}: Creating name list for CICE"
  source "${USHgfs}/parsing_namelists_CICE.sh"
  CICE_namelists
}

CICE_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for CICE"

  # Copy ice_in namelist from DATA to COMOUT_CONF after the forecast is run (and successfull)
  ${NCP} "${DATA}/ice_in" "${COMOUT_CONF}/ufs.ice_in"

  # Copy CICE restarts at the end of the forecast segment to COM for RUN=gfs|gefs
  if [[ "${COPY_FINAL_RESTARTS}" == "YES" ]]; then
    local seconds source_file target_file
    if [[ "${RUN}" == "gfs" || "${RUN}" == "gefs" ]]; then
      echo "Copying CICE restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
      seconds=$(to_seconds "${forecast_end_cycle:8:2}0000")  # convert HHMMSS to seconds
      source_file="cice_model.res.${forecast_end_cycle:0:4}-${forecast_end_cycle:4:2}-${forecast_end_cycle:6:2}-${seconds}.nc"
      target_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.cice_model.res.nc"
      ${NCP} "${DATArestart}/CICE_RESTART/${source_file}" \
             "${COMOUT_ICE_RESTART}/${target_file}"
    fi
  fi

  # Copy restarts for next cycle for RUN=gdas|enkfgdas|enkfgfs
  if [[ "${RUN}" =~ "gdas" || "${RUN}" == "enkfgfs" ]]; then
    local restart_date
    restart_date="${model_start_date_next_cycle}"
    echo "Copying CICE restarts for 'RUN=${RUN}' at ${restart_date}"
    seconds=$(to_seconds "${restart_date:8:2}0000")  # convert HHMMSS to seconds
    source_file="cice_model.res.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
    target_file="${restart_date:0:8}.${restart_date:8:2}0000.cice_model.res.nc"
    ${NCP} "${DATArestart}/CICE_RESTART/${source_file}" \
           "${COMOUT_ICE_RESTART}/${target_file}"
  fi

}

GOCART_rc() {
  echo "SUB ${FUNCNAME[0]}: Linking input data and copying config files for GOCART"
  # set input directory containing GOCART input data and configuration files
  # this variable is platform-dependent and should be set via a YAML file

  # link directory containing GOCART input dataset, if provided
  if [[ -n "${AERO_INPUTS_DIR}" ]]; then
    ${NLN} "${AERO_INPUTS_DIR}" "${DATA}/ExtData"
    status=$?
    [[ ${status} -ne 0 ]] && exit "${status}"
  fi

  # copying GOCART configuration files
  if [[  -n "${AERO_CONFIG_DIR}" ]]; then
    ${NCP} "${AERO_CONFIG_DIR}"/*.rc "${DATA}"
    status=$?
    [[ ${status} -ne 0 ]] && exit "${status}"
    # attempt to generate ExtData configuration file if not provided
    if [[ ! -f "${DATA}/AERO_ExtData.rc" ]]; then
      { \
        echo "PrimaryExports%%" ; \
        cat "${AERO_CONFIG_DIR}/ExtData.other" ; \
        cat "${AERO_CONFIG_DIR}/ExtData.${AERO_EMIS_FIRE:-none}" ; \
        echo "%%" ; \
      } > "${DATA}/AERO_ExtData.rc"
      status=$?
      if (( status != 0 )); then exit "${status}"; fi
    fi
  fi
}

GOCART_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking output data for GOCART"

  local vdate
  for fhr in ${GOCART_OUTPUT_FH}; do
    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)

    # Temporarily delete existing files due to noclobber in GOCART
    if [[ -e "${COMOUT_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4" ]]; then
      rm -f "${COMOUT_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4"
    fi

    #TODO: Temporarily removing this as this will crash gocart, adding copy statement at the end
    #${NLN} "${COMOUT_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4" \
    #       "${DATA}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4"
  done
}

GOCART_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for GOCART"

  # Copy gocart.inst_aod after the forecast is run (and successfull)
  # TODO: this should be linked but there are issues where gocart crashing if it is linked
  local fhr
  local vdate
  for fhr in ${GOCART_OUTPUT_FH}; do
    if (( fhr == 0 )); then continue; fi
    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
    ${NCP} "${DATA}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4" \
      "${COMOUT_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4"
  done
}

CMEPS_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking output data for CMEPS mediator"

  if [[ "${warm_start}" == ".true." ]]; then

    # Determine the appropriate restart file
    local restart_date cmeps_restart_file
    if [[ "${RERUN}" == "YES" ]]; then
      restart_date="${RERUN_DATE}"
      local seconds
      seconds=$(to_seconds "${restart_date:8:2}0000")  # convert HHMMSS to seconds
      cmeps_restart_file="${DATArestart}/CMEPS_RESTART/ufs.cpld.cpl.r.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
    else  # "${RERUN}" == "NO"
      restart_date="${model_start_date_current_cycle}"
      cmeps_restart_file="${COMIN_MED_RESTART_PREV}/${restart_date:0:8}.${restart_date:8:2}0000.ufs.cpld.cpl.r.nc"
    fi

    # Copy CMEPS restarts
    if [[ -f "${cmeps_restart_file}" ]]; then
      ${NCP} "${cmeps_restart_file}" "${DATA}/ufs.cpld.cpl.r.nc" \
      || ( echo "FATAL ERROR: Unable to copy CMEPS restarts, ABORT!"; exit 1 )
      rm -f "${DATA}/rpointer.cpl"
      touch "${DATA}/rpointer.cpl"
      echo "ufs.cpld.cpl.r.nc" >> "${DATA}/rpointer.cpl"
    else
      # We have a choice to make here.
      # Either we can FATAL ERROR out, or we can let the coupling fields initialize from zero
      # cmeps_run_type is determined based on the availability of the CMEPS restart file
      echo "WARNING: CMEPS restart file '${cmeps_restart_file}' not found for warm_start='${warm_start}', will initialize!"
      if [[ "${RERUN}" == "YES" ]]; then
        # In the case of a RERUN, the CMEPS restart file is required
        echo "FATAL ERROR: CMEPS restart file '${cmeps_restart_file}' not found for RERUN='${RERUN}', ABORT!"
        exit 1
      fi
    fi

  fi  # [[ "${warm_start}" == ".true." ]];
}

CMEPS_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for CMEPS mediator"

  # Copy mediator restarts at the end of the forecast segment to COM for RUN=gfs|gefs
  if [[ "${COPY_FINAL_RESTARTS}" == "YES" ]]; then
    echo "Copying mediator restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
    local seconds source_file target_file
    seconds=$(to_seconds "${forecast_end_cycle:8:2}"0000)
    source_file="ufs.cpld.cpl.r.${forecast_end_cycle:0:4}-${forecast_end_cycle:4:2}-${forecast_end_cycle:6:2}-${seconds}.nc"
    target_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.ufs.cpld.cpl.r.nc"
    if [[ -f "${DATArestart}/CMEPS_RESTART/${source_file}" ]]; then
      ${NCP} "${DATArestart}/CMEPS_RESTART/${source_file}" \
             "${COMOUT_MED_RESTART}/${target_file}"
    else
      echo "Mediator restart '${DATArestart}/CMEPS_RESTART/${source_file}' not found."
    fi
  fi

  # Copy restarts for the next cycle to COM for RUN=gdas|enkfgdas|enkfgfs
  if [[ "${RUN}" =~ "gdas" || "${RUN}" == "enkfgfs" ]]; then
    local restart_date
    restart_date="${model_start_date_next_cycle}"
    echo "Copying mediator restarts for 'RUN=${RUN}' at ${restart_date}"
    seconds=$(to_seconds "${restart_date:8:2}"0000)
    source_file="ufs.cpld.cpl.r.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
    target_file="${restart_date:0:8}.${restart_date:8:2}0000.ufs.cpld.cpl.r.nc"
    if [[ -f "${DATArestart}/CMEPS_RESTART/${source_file}" ]]; then
      ${NCP} "${DATArestart}/CMEPS_RESTART/${source_file}" \
             "${COMOUT_MED_RESTART}/${target_file}"
    else
      echo "Mediator restart '${DATArestart}/CMEPS_RESTART/${source_file}' not found."
    fi
  fi

}
