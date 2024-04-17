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

    echo "Copying FV3 cold start files for 'RUN=${RUN}' at '${current_cycle}' from '${COM_ATMOS_INPUT}'"
    for fv3_input_file in "${fv3_input_files[@]}"; do
      ${NCP} "${COM_ATMOS_INPUT}/${fv3_input_file}" "${DATA}/INPUT/${fv3_input_file}" \
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

    local restart_date restart_dir
    if [[ "${RERUN}" == "YES" ]]; then
      restart_date="${RERUN_DATE}"
      restart_dir="${DATArestart}/FV3_RESTART"
    else  # "${RERUN}" == "NO"
      if [[ "${DOIAU}" == "YES" ]]; then
        restart_date="${current_cycle_begin}"
      else
        restart_date="${current_cycle}"
      fi
      restart_dir="${COM_ATMOS_RESTART_PREV}"
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
        if [[ -f "${COM_ATMOS_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.sfcanl_data.tile${nn}.nc" ]]; then
          rm -f "${DATA}/INPUT/sfc_data.tile${nn}.nc"
          ${NCP} "${COM_ATMOS_RESTART}/${restart_date:0:8}.${restart_date:8:2}0000.sfcanl_data.tile${nn}.nc" \
                 "${DATA}/INPUT/sfc_data.tile${nn}.nc"
        else
          echo "'sfcanl_data.tile1.nc' not found in '${COM_ATMOS_RESTART}', using 'sfc_data.tile1.nc'"
          break
        fi
      done

      # Need a coupler.res when doing IAU
      if [[ "${DOIAU}" == "YES" ]]; then
        rm -f "${DATA}/INPUT/coupler.res"
        cat >> "${DATA}/INPUT/coupler.res" << EOF
        3        (Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)
        ${previous_cycle:0:4}  ${previous_cycle:4:2}  ${previous_cycle:6:2}  ${previous_cycle:8:2}  0  0        Model start time: year, month, day, hour, minute, second
        ${current_cycle_begin:0:4}  ${current_cycle_begin:4:2}  ${current_cycle_begin:6:2}  ${current_cycle_begin:8:2}  0  0        Current model time: year, month, day, hour, minute, second
EOF
      fi

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
        increment_file="${COM_ATMOS_ANALYSIS}/${RUN}.t${cyc}z.${PREFIX_ATMINC}${inc_file}"
        if [[ -f "${increment_file}" ]]; then
          ${NCP} "${increment_file}" "${DATA}/INPUT/${inc_file}"
        else
          echo "FATAL ERROR: missing increment file '${increment_file}', ABORT!"
          exit 1
        fi
      done

    fi

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
    for fhr in ${FV3_OUTPUT_FH}; do
      local FH3=$(printf %03i "${fhr}")
      local FH2=$(printf %02i "${fhr}")
      ${NLN} "${COM_ATMOS_HISTORY}/${RUN}.t${cyc}z.atmf${FH3}.nc" "atmf${FH3}.nc"
      ${NLN} "${COM_ATMOS_HISTORY}/${RUN}.t${cyc}z.sfcf${FH3}.nc" "sfcf${FH3}.nc"
      ${NLN} "${COM_ATMOS_HISTORY}/${RUN}.t${cyc}z.atm.logf${FH3}.txt" "log.atm.f${FH3}"
      if [[ ${WRITE_DOPOST} = ".true." ]]; then
        ${NLN} "${COM_ATMOS_MASTER}/${RUN}.t${cyc}z.master.grb2f${FH3}" "GFSPRS.GrbF${FH2}"
        ${NLN} "${COM_ATMOS_MASTER}/${RUN}.t${cyc}z.sfluxgrbf${FH3}.grib2" "GFSFLX.GrbF${FH2}"
      fi
    done
  else  # TODO: Is this even valid anymore?
    local nn
    for (( nn = 1; nn <= ntiles; nn++ )); do
      ${NLN} "nggps2d.tile${nn}.nc"       "${COM_ATMOS_HISTORY}/nggps2d.tile${nn}.nc"
      ${NLN} "nggps3d.tile${nn}.nc"       "${COM_ATMOS_HISTORY}/nggps3d.tile${nn}.nc"
      ${NLN} "grid_spec.tile${nn}.nc"     "${COM_ATMOS_HISTORY}/grid_spec.tile${nn}.nc"
      ${NLN} "atmos_static.tile${nn}.nc"  "${COM_ATMOS_HISTORY}/atmos_static.tile${nn}.nc"
      ${NLN} "atmos_4xdaily.tile${nn}.nc" "${COM_ATMOS_HISTORY}/atmos_4xdaily.tile${nn}.nc"
    done
  fi
}

FV3_nml() {
  # namelist output for a certain component
  echo "SUB ${FUNCNAME[0]}: Creating name lists and model configure file for FV3"

  source "${USHgfs}/parsing_namelists_FV3.sh"
  source "${USHgfs}/parsing_model_configure_FV3.sh"

  FV3_namelists
  FV3_model_configure

  echo "SUB ${FUNCNAME[0]}: FV3 name lists and model configure file created"
}

FV3_out() {
  echo "SUB ${FUNCNAME[0]}: copying output data for FV3"

  # Copy configuration files
  if [[ "${RUN}" == "gfs" || "${RUN}" == "gefs" ]]; then
    ${NCP} "${DATA}/input.nml" "${COM_CONF}/ufs.input.nml"
    ${NCP} "${DATA}/model_configure" "${COM_CONF}/ufs.model_configure"
    ${NCP} "${DATA}/ufs.configure" "${COM_CONF}/ufs.ufs.configure"
    ${NCP} "${DATA}/diag_table" "${COM_CONF}/ufs.diag_table"
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
    while (( restart_date < forecast_end_cycle )); do
      echo "Copying FV3 restarts for 'RUN=${RUN}' at ${restart_date}"
      for fv3_restart_file in ${fv3_restart_files[@]}; do
        restart_file="${restart_date:0:8}.${restart_date:8:2}0000.${fv3_restart_file}"
        ${NCP} "${DATArestart}/FV3_RESTART/${restart_file}" \
               "${COM_ATMOS_RESTART}/${restart_file}"
      done
      restart_date=$(date --utc -d "${restart_date:0:8} ${restart_date:8:2} + ${restart_interval} hours" +%Y%m%d%H)
    done
  fi

  # Copy the final restart files at the end of the forecast segment
  # The final restart written at the end of the forecast does not include the valid date
  # TODO: verify the above statement since RM found that it did!
  echo "Copying FV3 restarts for 'RUN=${RUN}' at the end of the forecast segment: ${forecast_end_cycle}"
  for fv3_restart_file in ${fv3_restart_files[@]}; do
    restart_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.${fv3_restart_file}"
    ${NCP} "${DATArestart}/FV3_RESTART/${restart_file}" \
           "${COM_ATMOS_RESTART}/${restart_file}"
  done

  echo "SUB ${FUNCNAME[0]}: Output data for FV3 copied"
}

WW3_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking input data for WW3"
  COMPONENTwave=${COMPONENTwave:-${RUN}wave}

  #Link mod_def files for wave grids
  if [[ ${waveMULTIGRID} = ".true." ]]; then
    local array=(${WAVECUR_FID} ${WAVEICE_FID} ${WAVEWND_FID} ${waveuoutpGRD} ${waveGRD} ${waveesmfGRD})
    echo "Wave Grids: ${WAVECUR_FID} ${WAVEICE_FID} ${WAVEWND_FID} ${waveuoutpGRD} ${waveGRD} ${waveesmfGRD}"
    local grdALL=$(printf "%s\n" "${array[@]}" | sort -u | tr '\n' ' ')

    for wavGRD in ${grdALL}; do
      ${NCP} "${COM_WAVE_PREP}/${COMPONENTwave}.mod_def.${wavGRD}" "${DATA}/mod_def.${wavGRD}"
    done
  else
    #if shel, only 1 waveGRD which is linked to mod_def.ww3
    ${NCP} "${COM_WAVE_PREP}/${COMPONENTwave}.mod_def.${waveGRD}" "${DATA}/mod_def.ww3"
  fi


  #if wave mesh is not the same as the ocean mesh, link it in the file
  if [[ "${MESH_WAV}" == "${MESH_OCN:-mesh.mx${OCNRES}.nc}" ]]; then
    echo "Wave is on same mesh as ocean"
  else
    ${NLN} "${FIXgfs}/wave/${MESH_WAV}" "${DATA}/"
  fi

  export wavprfx=${RUNwave}${WAV_MEMBER:-}

  #Copy initial condition files:
  for wavGRD in ${waveGRD} ; do
    if [[ "${warm_start}" = ".true." ]] || [[ "${RERUN}" = "YES" ]]; then
      if [[ ${RERUN} = "NO" ]]; then
        local waverstfile="${COM_WAVE_RESTART_PREV}/${sPDY}.${scyc}0000.restart.${wavGRD}"
      else
        local waverstfile="${COM_WAVE_RESTART}/${PDYT}.${cyct}0000.restart.${wavGRD}"
      fi
    else
      local waverstfile="${COM_WAVE_RESTART}/${sPDY}.${scyc}0000.restart.${wavGRD}"
    fi
    if [[ ! -f ${waverstfile} ]]; then
      if [[ ${RERUN} = "NO" ]]; then
        echo "WARNING: NON-FATAL ERROR wave IC is missing, will start from rest"
      else
        echo "ERROR: Wave IC is missing in RERUN, exiting."
        exit 1
      fi
    else
      if [[ ${waveMULTIGRID} = ".true." ]]; then
        ${NLN} "${waverstfile}" "${DATA}/restart.${wavGRD}"
      else
        ${NLN} "${waverstfile}" "${DATA}/restart.ww3"
      fi
    fi
  done

  if [[ ${waveMULTIGRID} = ".true." ]]; then
    for wavGRD in ${waveGRD} ; do
      ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.log.${wavGRD}.${PDY}${cyc}" "log.${wavGRD}"
    done
  else
    ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.log.${waveGRD}.${PDY}${cyc}" "log.ww3"
  fi

  if [[ "${WW3ICEINP}" = "YES" ]]; then
    local wavicefile="${COM_WAVE_PREP}/${RUNwave}.${WAVEICE_FID}.${cycle}.ice"
    if [[ ! -f ${wavicefile} ]]; then
      echo "ERROR: WW3ICEINP = ${WW3ICEINP}, but missing ice file"
      echo "Abort!"
      exit 1
    fi
    ${NLN} "${wavicefile}" "${DATA}/ice.${WAVEICE_FID}"
  fi

  if [[ "${WW3CURINP}" = "YES" ]]; then
    local wavcurfile="${COM_WAVE_PREP}/${RUNwave}.${WAVECUR_FID}.${cycle}.cur"
    if [[ ! -f ${wavcurfile} ]]; then
      echo "ERROR: WW3CURINP = ${WW3CURINP}, but missing current file"
      echo "Abort!"
      exit 1
    fi
    ${NLN} "${wavcurfile}" "${DATA}/current.${WAVECUR_FID}"
  fi

  # Link output files
  cd "${DATA}"
  if [[ ${waveMULTIGRID} = ".true." ]]; then
    ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.log.mww3.${PDY}${cyc}" "log.mww3"
  fi

  # Loop for gridded output (uses FHINC)
  local fhr vdate FHINC wavGRD
  fhr=${FHMIN_WAV}
  while [[ ${fhr} -le ${FHMAX_WAV} ]]; do
    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
    if [[ ${waveMULTIGRID} = ".true." ]]; then
      for wavGRD in ${waveGRD} ; do
        ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.out_grd.${wavGRD}.${vdate:0:8}.${vdate:8:2}0000" "${DATA}/${vdate:0:8}.${vdate:8:2}0000.out_grd.${wavGRD}"
      done
    else
      ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.out_grd.${waveGRD}.${vdate:0:8}.${vdate:8:2}0000" "${DATA}/${vdate:0:8}.${vdate:8:2}0000.out_grd.ww3"
    fi
    FHINC=${FHOUT_WAV}
    if (( FHMAX_HF_WAV > 0 && FHOUT_HF_WAV > 0 && fhr < FHMAX_HF_WAV )); then
      FHINC=${FHOUT_HF_WAV}
    fi
    fhr=$((fhr+FHINC))
  done

  # Loop for point output (uses DTPNT)
  fhr=${FHMIN_WAV}
  while [[ ${fhr} -le ${FHMAX_WAV} ]]; do
    vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)
    if [[ ${waveMULTIGRID} = ".true." ]]; then
      ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.out_pnt.${waveuoutpGRD}.${vdate:0:8}.${vdate:8:2}0000" "${DATA}/${vdate:0:8}.${vdate:8:2}0000.out_pnt.${waveuoutpGRD}"
    else
      ${NLN} "${COM_WAVE_HISTORY}/${wavprfx}.out_pnt.${waveuoutpGRD}.${vdate:0:8}.${vdate:8:2}0000" "${DATA}/${vdate:0:8}.${vdate:8:2}0000.out_pnt.ww3"
    fi

    FHINC=${FHINCP_WAV}
    fhr=$((fhr+FHINC))
  done
}

WW3_nml() {
  echo "SUB ${FUNCNAME[0]}: Copying input files for WW3"
  WAV_MOD_TAG=${RUN}wave${waveMEMB}
  if [[ "${USE_WAV_RMP:-YES}" = "YES" ]]; then
    if (( $( ls -1 "${FIXgfs}/wave/rmp_src_to_dst_conserv_"* 2> /dev/null | wc -l) > 0 )); then
      for file in $(ls "${FIXgfs}/wave/rmp_src_to_dst_conserv_"*) ; do
        ${NLN} "${file}" "${DATA}/"
      done
    else
      echo 'FATAL ERROR : No rmp precomputed nc files found for wave model'
      exit 4
    fi
  fi
  source "${USHgfs}/parsing_namelists_WW3.sh"
  WW3_namelists
}

WW3_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for WW3"
}


CPL_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for general cpl fields"
  if [[ "${esmf_profile:-}" = ".true." ]]; then
    ${NCP} "${DATA}/ESMF_Profile.summary" "${COM_ATMOS_HISTORY}/ESMF_Profile.summary"
  fi
}

MOM6_postdet() {
  echo "SUB ${FUNCNAME[0]}: MOM6 after run type determination"

  local restart_dir restart_date
  if [[ "${RERUN}" == "YES" ]]; then
    restart_dir="${DATArestart}/MOM6_RESTART"
    restart_date="${RERUN_DATE}"
  else  # "${RERUN}" == "NO"
    restart_dir="${COM_OCEAN_RESTART_PREV}"
    if [[ "${DOIAU}" == "YES" ]]; then
      restart_date="${current_cycle_begin}"
    else
      restart_date="${current_cycle}"
    fi
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
  esac

  # Copy increment (only when RERUN=NO)
  if [[ "${RERUN}" == "NO" ]]; then
    if [[ "${DO_JEDIOCNVAR:-NO}" == "YES" ]]; then
      ${NCP} "${COM_OCEAN_ANALYSIS}/${RUN}.t${cyc}z.ocninc.nc" "${DATA}/INPUT/mom6_increment.nc" \
      || ( echo "FATAL ERROR: Unable to copy MOM6 increment, ABORT!"; exit 1 )
    fi

    # GEFS perturbations
    # TODO if [[ $RUN} == "gefs" ]] block maybe be needed
    #     to ensure it does not interfere with the GFS when ensemble is updated in the GFS
    if (( MEMBER > 0 )) && [[ "${ODA_INCUPD:-False}" == "True" ]]; then
      ${NCP} "${COM_OCEAN_RESTART_PREV}/${restart_date:0:8}.${restart_date:0:8}0000.mom6_increment.nc" "${DATA}/INPUT/mom6_increment.nc" \
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
      ${NLN} "${COM_OCEAN_HISTORY}/${dest_file}" "${DATA}/MOM6_OUTPUT/${source_file}"

      # Daily output
      if (( fhr > 0 & fhr % 24 == 0 )); then
        source_file="ocn_daily_${vdate:0:4}_${vdate:4:2}_${vdate:6:2}.nc"
        dest_file="${RUN}.ocean.t${cyc}z.daily.f${fhr3}.nc"
        ${NLN} "${COM_OCEAN_HISTORY}/${dest_file}" "${DATA}/MOM6_OUTPUT/${source_file}"
      fi

      last_fhr=${fhr}

    done

  elif [[ "${RUN}" =~ "gdas" ]]; then  # Link output files for RUN=gdas|enkfgdas

    # Save (instantaneous) MOM6 backgrounds
    for fhr in ${MOM6_OUTPUT_FH}; do
      local fhr3=$(printf %03i "${fhr}")
      local vdatestr=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y_%m_%d_%H)
      ${NLN} "${COM_OCEAN_HISTORY}/${RUN}.ocean.t${cyc}z.inst.f${fhr3}.nc" "${DATA}/MOM6_OUTPUT/ocn_da_${vdatestr}.nc"
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

  # Copy MOM_input from DATA to COM_OCEAN_INPUT after the forecast is run (and successfull)
  ${NCP} "${DATA}/INPUT/MOM_input" "${COM_CONF}/ufs.MOM_input"

  # Create a list of MOM6 restart files
  # Coarser than 1/2 degree has a single MOM restart
  local mom6_restart_files mom6_restart_file restart_file
  mom6_restart_files=(MOM.res.nc)
  # 1/4 degree resolution has 4 additional restarts
  case "${OCNRES}" in
    "025")
      local nn
      for (( nn = 1; nn <= 4; nn++ )); do
        mom6_restart_files+=("MOM.res_${nn}.nc")
      done
      ;;
  esac

  # Copy MOM6 restarts at the end of the forecast segment to COM for RUN=gfs|gefs
  local restart_file
  if [[ "${RUN}" == "gfs" || "${RUN}" == "gefs" ]]; then
    echo "Copying MOM6 restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
    for mom6_restart_file in ${mom6_restart_files[@]}; do
      restart_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.${mom6_restart_file}"
      ${NCP} "${DATArestart}/MOM6_RESTART/${restart_file}" \
             "${COM_OCEAN_RESTART}/${restart_file}"
    done
  fi

  # Copy restarts at the beginning/middle of the next assimilation cycle to COM for RUN=gdas|enkfgdas|enkfgfs
  if [[ "${RUN}" =~ "gdas" || "${RUN}" == "enkfgfs" ]]; then
    local restart_date
    if [[ "${DOIAU}" == "YES" ]]; then  # Copy restarts at the beginning of the next cycle from DATA to COM
      restart_date="${next_cycle_begin}"
    else  # Copy restarts at the middle of the next cycle from DATA to COM
      restart_date="${next_cycle}"
    fi
    echo "Copying MOM6 restarts for 'RUN=${RUN}' at ${restart_date}"
    for mom6_restart_file in ${mom6_restart_files[@]}; do
      restart_file="${restart_date:0:8}.${restart_date:8:2}0000.${mom6_restart_file}"
      ${NCP} "${DATArestart}/MOM6_RESTART/${restart_file}" \
             "${COM_OCEAN_RESTART}/${restart_file}"
    done
  fi

}

CICE_postdet() {
  echo "SUB ${FUNCNAME[0]}: CICE after run type determination"

  local restart_dir restart_date
  if [[ "${RERUN}" == "YES" ]]; then
    restart_dir="${DATArestart}/CICE_RESTART"
    restart_date="${RERUN_DATE}"
  else  # "${RERUN}" == "NO"
    restart_dir="${COM_ICE_RESTART_PREV}"
    if [[ "${DOIAU}" == "YES" ]]; then
      restart_date="${current_cycle_begin}"
    else
      restart_date="${current_cycle}"
    fi
  fi

  # Copy CICE ICs
  cice_restart_file="${restart_dir}/${restart_date:0:8}.${restart_date:8:2}0000.cice_model.res.nc"
  ${NCP} "${cice_restart_file}" "${DATA}/cice_model.res.nc" \
  || ( echo "FATAL ERROR: Unable to copy CICE IC, ABORT!"; exit 1 )

  # Link iceh_ic file to COM.  This is the initial condition file from CICE (f000)
  # TODO: Is this file needed in COM? Is this going to be used for generating any products?
  local vdate seconds vdatestr fhr fhr3 interval last_fhr
  seconds=$(to_seconds "${current_cycle:8:2}0000")  # convert HHMMSS to seconds
  vdatestr="${current_cycle:0:4}-${current_cycle:4:2}-${current_cycle:6:2}-${seconds}"
  ${NLN} "${COM_ICE_HISTORY}/${RUN}.ice.t${cyc}z.ic.nc" "${DATA}/CICE_OUTPUT/iceh_ic.${vdatestr}.nc"

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
    ${NLN} "${COM_ICE_HISTORY}/${dest_file}" "${DATA}/CICE_OUTPUT/${source_file}"

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

  # Copy ice_in namelist from DATA to COMOUTice after the forecast is run (and successfull)
  ${NCP} "${DATA}/ice_in" "${COM_CONF}/ufs.ice_in"

  # Copy CICE restarts at the end of the forecast segment to COM for RUN=gfs|gefs
  local seconds source_file target_file
  if [[ "${RUN}" == "gfs" || "${RUN}" == "gefs" ]]; then
    echo "Copying CICE restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
    seconds=$(to_seconds "${forecast_end_cycle:8:2}0000")  # convert HHMMSS to seconds
    source_file="cice_model.res.${forecast_end_cycle:0:4}-${forecast_end_cycle:4:2}-${forecast_end_cycle:6:2}-${seconds}.nc"
    target_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.cice_model.res.nc"
    ${NCP} "${DATArestart}/CICE_RESTART/${source_file}" \
           "${COM_ICE_RESTART}/${target_file}"
  fi

  # Copy restarts at the beginning/middle of the next assimilation cycle to COM for RUN=gdas|enkfgdas|enkfgfs
  if [[ "${RUN}" =~ "gdas" || "${RUN}" == "enkfgfs" ]]; then
    local restart_date
    if [[ "${DOIAU}" == "YES" ]]; then  # Copy restarts at the beginning of the next cycle from DATA to COM
      restart_date="${next_cycle_begin}"
    else  # Copy restarts at the middle of the next cycle from DATA to COM
      restart_date="${next_cycle}"
    fi
    echo "Copying CICE restarts for 'RUN=${RUN}' at ${restart_date}"
    seconds=$(to_seconds "${restart_date:8:2}0000")  # convert HHMMSS to seconds
    source_file="cice_model.res.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
    target_file="${restart_date:0:8}.${restart_date:8:2}0000.cice_model.res.nc"
    ${NCP} "${DATArestart}/CICE_RESTART/${source_file}" \
           "${COM_ICE_RESTART}/${target_file}"
  fi

}

GOCART_rc() {
  echo "SUB ${FUNCNAME[0]}: Linking input data and copying config files for GOCART"
  # set input directory containing GOCART input data and configuration files
  # this variable is platform-dependent and should be set via a YAML file

  # link directory containing GOCART input dataset, if provided
  if [[ ! -z "${AERO_INPUTS_DIR}" ]]; then
    ${NLN} "${AERO_INPUTS_DIR}" "${DATA}/ExtData"
    status=$?
    [[ ${status} -ne 0 ]] && exit "${status}"
  fi

  # copying GOCART configuration files
  if [[ ! -z "${AERO_CONFIG_DIR}" ]]; then
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

  for fhr in ${GOCART_OUTPUT_FH}; do
    local vdate=$(date --utc -d "${current_cycle:0:8} ${current_cycle:8:2} + ${fhr} hours" +%Y%m%d%H)

    # Temporarily delete existing files due to noclobber in GOCART
    if [[ -e "${COM_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4" ]]; then
      rm -f "${COM_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4"
    fi

    #TODO: Temporarily removing this as this will crash gocart, adding copy statement at the end
    #${NLN} "${COM_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4" \
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
      "${COM_CHEM_HISTORY}/gocart.inst_aod.${vdate:0:8}_${vdate:8:2}00z.nc4"
  done
}

CMEPS_postdet() {
  echo "SUB ${FUNCNAME[0]}: Linking output data for CMEPS mediator"

  # Copy mediator restart files to RUNDIR
  if [[ "${warm_start}" = ".true." ]]; then
    local mediator_file="${COM_MED_RESTART}/${PDY}.${cyc}0000.ufs.cpld.cpl.r.nc"
    if [[ -f "${mediator_file}" ]]; then
      ${NCP} "${mediator_file}" "${DATA}/ufs.cpld.cpl.r.nc"
      rm -f "${DATA}/rpointer.cpl"
      touch "${DATA}/rpointer.cpl"
      echo "ufs.cpld.cpl.r.nc" >> "${DATA}/rpointer.cpl"
    else
      # We have a choice to make here.
      # Either we can FATAL ERROR out, or we can let the coupling fields initialize from zero
      # cmeps_run_type is determined based on the availability of the mediator restart file
      echo "WARNING: ${mediator_file} does not exist for warm_start = .true., initializing!"
      #echo "FATAL ERROR: ${mediator_file} must exist for warm_start = .true. and does not, ABORT!"
      #exit 4
    fi
  fi

}

CMEPS_out() {
  echo "SUB ${FUNCNAME[0]}: Copying output data for CMEPS mediator"

  # Copy mediator restarts at the end of the forecast segment to COM for RUN=gfs|gefs
  echo "Copying mediator restarts for 'RUN=${RUN}' at ${forecast_end_cycle}"
  local seconds source_file target_file
  seconds=$(to_seconds "${forecast_end_cycle:8:2}"0000)
  source_file="ufs.cpld.cpl.r.${forecast_end_cycle:0:4}-${forecast_end_cycle:4:2}-${forecast_end_cycle:6:2}-${seconds}.nc"
  target_file="${forecast_end_cycle:0:8}.${forecast_end_cycle:8:2}0000.ufs.cpld.cpl.r.nc"
  if [[ -f "${DATArestart}/CMEPS_RESTART/${source_file}" ]]; then
    ${NCP} "${DATArestart}/CMEPS_RESTART/${source_file}" \
           "${COM_MED_RESTART}/${target_file}"
  else
    echo "Mediator restart '${DATArestart}/CMEPS_RESTART/${source_file}' not found."
  fi

  # Copy restarts at the beginning/middle of the next assimilation cycle to COM for RUN=gdas|enkfgdas|enkfgfs
  if [[ "${RUN}" =~ "gdas" || "${RUN}" == "enkfgfs" ]]; then
    local restart_date
    if [[ "${DOIAU}" == "YES" ]]; then  # Copy restarts at the beginning of the next cycle from DATA to COM
      restart_date="${next_cycle_begin}"
    else  # Copy restarts at the middle of the next cycle from DATA to COM
      restart_date="${next_cycle}"
    fi
    echo "Copying mediator restarts for 'RUN=${RUN}' at ${restart_date}"
    seconds=$(to_seconds "${restart_date:8:2}"0000)
    source_file="ufs.cpld.cpl.r.${restart_date:0:4}-${restart_date:4:2}-${restart_date:6:2}-${seconds}.nc"
    target_file="${restart_date:0:8}.${restart_date:8:2}0000.ufs.cpld.cpl.r.nc"
    if [[ -f "${DATArestart}/CMEPS_RESTART/${source_file}" ]]; then
      ${NCP} "${DATArestart}/CMEPS_RESTART/${source_file}" \
             "${COM_MED_RESTART}/${target_file}"
    else
      echo "Mediator restart '${DATArestart}/CMEPS_RESTART/${source_file}' not found."
    fi
  fi

}
