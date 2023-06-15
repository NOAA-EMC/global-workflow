#! /usr/bin/env bash
#
# define the array of the name of build program
#
declare -a Build_prg=("Build_ufs_model" \
                      "Build_ww3_prepost" \
                      "Build_gsi_enkf" \
                      "Build_gsi_utils" \
                      "Build_gsi_monitor" \
                      "Build_ww3_prepost" \
                      "Build_gdas" \
                      "Build_upp" \
                      "Build_ufs_utils" \
                      "Build_gfs_utils")

#
# function parse_cfg: read config file and retrieve the values
#
parse_cfg() {
  declare -i n
  declare -i num_args
  declare -i total_args
  declare -a all_prg
  total_args=$#
  num_args=$1
  (( num_args == 0 )) && return 0
  config=$2
  [[ ${config,,} == "--verbose" ]] && config=$3
  all_prg=()
  for (( n = num_args + 2; n <= total_args; n++ )); do
    all_prg+=( "${!n}" )
  done

  if [[ ${config^^} == ALL ]]; then
    #
    # set all values to true
    #
    for var in "${Build_prg[@]}"; do
      eval "${var}=true"
    done
  elif [[ ${config} == config=* ]]; then
    #
    # process config file
    #
    cfg_file=${config#config=}
    ${verbose} && echo "INFO: settings in config file: ${cfg_file}"
    while read -r cline; do
      #  remove leading white space
      clean_line="${cline#"${cline%%[![:space:]]*}"}"
       { [[ -z "${clean_line}" ]] || [[ "${clean_line:0:1}" == "#" ]]; } || {
        ${verbose} && echo "${clean_line}"
        first9=${clean_line:0:9}
        [[ ${first9,,} == "building " ]] && {
            # No shellcheck, this can't be replaced by a native bash substitute
            #   because it uses a regex
            # shellcheck disable=SC2001
            short_prg=$(sed -e 's/.*(\(.*\)).*/\1/' <<< "${clean_line}")
            # shellcheck disable=
            #  remove trailing white space
            clean_line="${cline%"${cline##*[![:space:]]}"}"
            build_action=true
            last5=${clean_line: -5}
            [[ ${last5,,} == ". yes" ]] && build_action=true
            last4=${clean_line: -4}
            [[ ${last4,,} == ". no" ]] && build_action=false
            found=false
            for prg in "${all_prg[@]}"; do
              [[ ${prg} == "Build_${short_prg}" ]] && {
                found=true
                eval "${prg}=${build_action}"
                break
              }
            done
          ${found} || {
            echo "*** Unrecognized line in config file \"${cfg_file}\":" 2>&1
            echo "${cline}" 2>&1
            exit 3
          }
        }
      }
    done < "${cfg_file}"
  elif [[ ${config} == select=* ]]; then
    #
    # set all values to (default) false
    #
    for var in "${Build_prg[@]}"; do
      eval "${var}=false"
    done
    #
    # read command line partial build setting
    #
    del=""
    sel_prg=${config#select=}
    for separator in " " "," ";" ":" "/" "|"; do
      [[ "${sel_prg/${separator}}" == "${sel_prg}" ]] || {
        del=${separator}
        sel_prg=${sel_prg//${del}/ }
      }
    done
    if [[ ${del} == "" ]]; then
      {
        short_prg=${sel_prg}
        found=false
        for prg in "${all_prg[@]}"; do
          [[ ${prg} == "Build_${short_prg}" ]] && {
            found=true
            eval "${prg}=true"
            break
          }
        done
        ${found} || {
          echo "*** Unrecognized program name \"${short_prg}\" in command line" 2>&1
          exit 4
        }
      } || {
        for short_prg in ${sel_prg}; do
          found=false
          for prg in "${all_prg[@]}"; do
            [[ ${prg} == "Build_${short_prg}" ]] && {
              found=true
              eval "${prg}=true"
              break
            }
          done
          ${found} || {
            echo "*** Unrecognized program name \"${short_prg}\" in command line" 2>&1
            exit 5
          }
        done
      }
    fi
  else
    echo "*** Unrecognized command line option \"${config}\"" 2>&1
    exit 6
  fi
}


usage() {
  cat << EOF 2>&1
Usage: ${BASH_SOURCE[0]} [-c config_file][-h][-v]
  -h:
    Print this help message and exit
  -v:
    Turn on verbose mode
  -c config_file:
    Override default config file to determine whether to build each program [default: gfs_build.cfg]
EOF
}


#
# read command line arguments; processing config file
#
declare -a parse_argv=()
verbose=false
config_file="gfs_build.cfg"
# Reset option counter for when this script is sourced
OPTIND=1
while getopts ":c:h:v" option; do
  case "${option}" in
    c) config_file="${OPTARG}";;
    h) usage;;
    v)
      verbose=true
      parse_argv+=( "--verbose" )
      ;;
    :)
      echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
      usage
      ;;
    *)
      echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
      usage
      ;;
  esac
done

shift $((OPTIND-1))

parse_argv+=( "config=${config_file}" )

#
# call arguments retriever/config parser
#
parse_cfg ${#parse_argv[@]} "${parse_argv[@]}" "${Build_prg[@]}"

#
# print values of build array
#
${verbose} && {
  echo "INFO: partial build settings:"
  for var in "${Build_prg[@]}"; do
    echo -n "  ${var}: "
    "${!var}" && echo True || echo False
  done
}

echo "=== end of partial build setting ===" > /dev/null

