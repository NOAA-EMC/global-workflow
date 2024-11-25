#! /usr/bin/env bash

# Disable variable not used warnings
# shellcheck disable=SC2034
GOCART_namelists() {
  # copying GOCART configuration files
  if [[  -n "${AERO_CONFIG_DIR}" ]]; then

    local base_in
    local fhout_aero_padded
    fhout_aero_padded=$(printf "%02d" "${FHOUT_AERO}")
    # Only instantaneous AOD is output right now
    local inst_aod_freq="${fhout_aero_padded}0000"

    # Other gocart fields not currently used
    local inst_du_ss_freq="120000"
    local tavg_du_ss_freq="120000"
    local inst_ca_freq="120000"
    local inst_ni_freq="120000"
    local inst_su_freq="120000"
    local inst_du_bin_freq="010000"
    local tavg_du_bin_freq="030000"
    local inst_ss_bin_freq="060000"
    local inst_ca_bin_freq="120000"
    local inst_ni_bin_freq="120000"
    local inst_su_bin_freq="120000"
    local inst_2d_freq="030000"
    local inst_3d_freq="060000"
    local tavg_2d_rad_freq="120000"
    local tavg_3d_rad_freq="120000"

    for template_in in "${AERO_CONFIG_DIR}/"*.rc; do
      base_in="$(basename "${template_in}")"
      atparse < "${template_in}" >> "${DATA}/${base_in}"
      status=$?
      [[ ${status} -ne 0 ]] && exit "${status}"
    done

    # attempt to generate ExtData configuration file if not provided
    if [[ ! -f "${DATA}/AERO_ExtData.rc" ]]; then
      { \
        echo "PrimaryExports%%" ; \
        cat "${AERO_CONFIG_DIR}/ExtData.other" ; \
        cat "${AERO_CONFIG_DIR}/ExtData.${AERO_EMIS_FIRE:-none}" ; \
        echo "%%" ; \
      } > "${DATA}/AERO_ExtData.rc"
      # shellcheck disable=SC2320
      status=$?
      if (( status != 0 )); then exit "${status}"; fi
    fi
  fi
}
