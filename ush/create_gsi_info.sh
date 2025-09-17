#!/usr/bin/env bash
# Generic script to create convinfo, ozinfo, or satinfo for a given date.
# Usage:
#   ./create_gsi_info.sh <type> <date> [use2mobs]
#   <type>: conv, oz, or sat
#   <date>: date string to match
#   [use2mobs]: (optional, only for conv) YES or NO

set -eu
# Inherit set -e inside of the get_usedate function below
shopt -s inherit_errexit
type_in=${1:-}
date_in=${2:-}
use2mobs=${3:-NO}

if [[ -z "${type_in}" || -z "${date_in}" ]]; then
  echo "Usage: ${0} <type> <date> [use2mobs]"
  echo "  <type>: conv, oz, or sat"
  echo "  <date>: date string to match"
  echo "  [use2mobs]: (optional, only for conv, whether to use 2m observations) YES or NO"
  exit 1
fi

# Function to get the most recent data available for the target obs.
# If an empty string is returned, this represents an error.
get_usedate() {
  usedate=""
  # Loop over files matching date pattern.
  for datex in [1-2][0-9][0-9][0-9]*; do
    # Skip for loop if there are no matches.
    if [[ ! -e "${datex}" ]]; then
      continue
    fi

    if [[ ${date_in} -ge ${datex} ]]; then
      usedate=${datex}
    fi
  done

  echo "${usedate}"
}

# Get the starting directory
starting_dir="${DATA:-${PWD}}"

# Filename to write the info to
info_file="${starting_dir}/${type_in}info"
if [[ -f "${info_file}" ]]; then
  rm -f "${info_file}"
fi

case "${type_in}" in
  conv)
    cd "${BUILD_GSINFO_DIR}/convinfo" || exit 1

    usedate=$(get_usedate)
    if [[ ${usedate} != "" ]]; then
      if [[ ${use2mobs} == "YES" ]]; then
        # Turn on 2m t,q obs over land
        sed -e "s/t        181    0   -1/t        181    0    1/g" \
            -e "s/t        187    0   -1/t        187    0    1/g" \
            -e "s/q        181    0   -1/q        181    0    1/g" \
            -e "s/q        187    0   -1/q        187    0    1/g" "${usedate}" >> "${info_file}"
      else
        cat "${usedate}" >> "${info_file}"
      fi
    else
      echo "ERROR: No valid conventional info was found!"
      exit 1
    fi
    ;;
  oz)
    cd "${BUILD_GSINFO_DIR}/ozinfo" || exit 1
    usedate=$(get_usedate)
    if [[ ${usedate} != "" ]]; then
      # Header lines
      {
        echo '! For mls data, pressure and obs errors are pulled from bufr, so not listed here'
        echo '! sens/instr/sat lev  use pressure gross   obs    b_oz  pg_oz'
        echo '!                                  error  error variational qc'
      } >> "${info_file}"
      cat "${usedate}" >> "${info_file}"
    else
      echo "ERROR: No valid ozone info was found!"
      exit 1
    fi
    ;;
  sat)
    cd "${BUILD_GSINFO_DIR}/satinfo" || exit 1
    # Read in the satellite list, ignoring comment lines and the readme
    satellite_list=$(grep -Ev '^ *#|readme' satellites)
    if [[ -z "${satellite_list}" ]]; then
      echo "ERROR: No satellites found in the satellite file list!"
      exit 1
    fi

    # Header line
    echo '!sensor/instr/sat      chan iuse  error  error_cld  ermax   var_b    var_pg  icld_det icloud iaerosol' >> "${info_file}"
    while IFS= read -r sat
    do
      usedate=""
      # Check that the satellite directory exists
      if [[ ! -d "${sat}" ]]; then
        echo "ERROR: Directory ${sat} does not exist!"
        exit 1
      fi

      cd "${sat}" || exit 1

      usedate=$(get_usedate)

      cd "${BUILD_GSINFO_DIR}/satinfo" || exit 1

      if [[ ${usedate} != "" ]]; then
        cat "${sat}/${usedate}" >> "${info_file}"
      else
        echo "ERROR: No valid satellite info was found for satellite target '${sat}'!"
        exit 1
      fi
    done <<< "${satellite_list}"
    ;;
  *)
    echo "ERROR: Unknown info file type: '${type_in}'. Must be one of: conv, oz, sat"
    exit 2
    ;;
esac

# Return to starting directory
cd "${starting_dir}" || exit 1
