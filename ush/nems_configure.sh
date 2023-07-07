#! /usr/bin/env bash

#####
## This script writes nems.configure file
## first, select a "*.IN" templates based on
## $confignamevarfornems and parse values based on
## $cpl** switches.
##
## This is a child script of modular
## forecast script. This script is definition only (Is it? There is nothing defined here being used outside this script.)
#####
writing_nems_configure()
{
echo "SUB ${FUNCNAME[0]}: parsing_nems_configure begins"
if [[ -e "${SCRIPTDIR}/nems.configure" ]]; then
  rm -f "${SCRIPTDIR}/nems.configure"
fi

# Setup nems.configure
local DumpFields=${NEMSDumpFields:-false}
local cap_dbug_flag=${cap_dbug_flag:-0}
# Determine "cmeps_run_type" based on the availability of the mediator restart file
# If it is a warm_start, we already copied the mediator restart to DATA, if it was present
# If the mediator restart was not present, despite being a "warm_start", we put out a WARNING
# in forecast_postdet.sh
if [[ -f "${DATA}/ufs.cpld.cpl.r.nc" ]]; then
  local cmeps_run_type='continue'
else
  local cmeps_run_type='startup'
fi

rm -f "${DATA}/nems.configure"

local esmf_logkind=${esmf_logkind:-"ESMF_LOGKIND_MULTI"} #options: ESMF_LOGKIND_MULTI_ON_ERROR, ESMF_LOGKIND_MULTI, ESMF_LOGKIND_NONE

# Copy the selected template into run directory
infile="${SCRIPTDIR}/nems.configure.${confignamevarfornems}.IN"
if [[ -s ${infile} ]]; then
  cp "${infile}" tmp1
else
  echo "FATAL ERROR: nem.configure template '${infile}' does not exist!"
  exit 1
fi

local atm_petlist_bounds="0 $(( ${ATMPETS}-1 ))"
local med_petlist_bounds="0 $(( ${MEDPETS}-1 ))"

sed -i -e "s;@\[atm_model\];fv3;g" tmp1
sed -i -e "s;@\[atm_petlist_bounds\];${atm_petlist_bounds};g" tmp1
sed -i -e "s;@\[atm_omp_num_threads\];${ATMTHREADS};g" tmp1
sed -i -e "s;@\[med_model\];cmeps;g" tmp1
sed -i -e "s;@\[med_petlist_bounds\];${med_petlist_bounds};g" tmp1
sed -i -e "s;@\[med_omp_num_threads\];${MEDTHREADS};g" tmp1
sed -i -e "s;@\[esmf_logkind\];${esmf_logkind};g" tmp1

if [[ "${cpl}" = ".true." ]]; then
  sed -i -e "s;@\[coupling_interval_slow_sec\];${CPL_SLOW};g" tmp1
fi

if [[ "${cplflx}" = ".true." ]]; then
  # TODO: Should this be raised up to config.ufs or config.ocn?
  case "${OCNRES}" in
    "500") local eps_imesh="4.0e-1";;
    "100") local eps_imesh="2.5e-1";;
    *) local eps_imesh="1.0e-1";;
  esac

  local use_coldstart=${use_coldstart:-".false."}
  local use_mommesh=${USE_MOMMESH:-"true"}
  local restile=$(echo "${CASE}" |cut -c2-)

  local start="${ATMPETS}"
  local end="$(( ${start}+${OCNPETS}-1 ))"
  local ocn_petlist_bounds="${start} ${end}"

  sed -i -e "s;@\[ocn_model\];mom6;g" tmp1
  sed -i -e "s;@\[ocn_petlist_bounds\];${ocn_petlist_bounds};g" tmp1
  sed -i -e "s;@\[ocn_omp_num_threads\];${OCNTHREADS};g" tmp1
  sed -i -e "s;@\[DumpFields\];${DumpFields};g" tmp1
  sed -i -e "s;@\[cap_dbug_flag\];${cap_dbug_flag};g" tmp1
  sed -i -e "s;@\[use_coldstart\];${use_coldstart};g" tmp1
  sed -i -e "s;@\[RUNTYPE\];${cmeps_run_type};g" tmp1
  sed -i -e "s;@\[CPLMODE\];${cplmode};g" tmp1
  sed -i -e "s;@\[coupling_interval_fast_sec\];${CPL_FAST};g" tmp1
  sed -i -e "s;@\[RESTART_N\];${restart_interval};g" tmp1
  sed -i -e "s;@\[use_mommesh\];${use_mommesh};g" tmp1
  sed -i -e "s;@\[eps_imesh\];${eps_imesh};g" tmp1
  sed -i -e "s;@\[ATMTILESIZE\];${restile};g" tmp1
fi

if [[ "${cplice}" = ".true." ]]; then

  local mesh_ocn_ice=${MESH_OCN_ICE:-"mesh.mx${ICERES}.nc"}

  local start="$(( ${ATMPETS}+${OCNPETS} ))"
  local end="$(( ${start}+${ICEPETS}-1 ))"
  local ice_petlist_bounds="${start} ${end}"

  sed -i -e "s;@\[ice_model\];cice6;g" tmp1
  sed -i -e "s;@\[ice_petlist_bounds\];${ice_petlist_bounds};g" tmp1
  sed -i -e "s;@\[ice_omp_num_threads\];${ICETHREADS};g" tmp1
  sed -i -e "s;@\[MESH_OCN_ICE\];${mesh_ocn_ice};g" tmp1
  sed -i -e "s;@\[FHMAX\];${FHMAX_GFS};g" tmp1
fi

if [[ "${cplwav}" = ".true." ]]; then

  local start="$(( ${ATMPETS}+${OCNPETS:-0}+${ICEPETS:-0} ))"
  local end="$(( ${start}+${WAVPETS}-1 ))"
  local wav_petlist_bounds="${start} ${end}"

  sed -i -e "s;@\[wav_model\];ww3;g" tmp1
  sed -i -e "s;@\[wav_petlist_bounds\];${wav_petlist_bounds};g" tmp1
  sed -i -e "s;@\[wav_omp_num_threads\];${WAVTHREADS};g" tmp1
  sed -i -e "s;@\[MESH_WAV\];${MESH_WAV};g" tmp1
  sed -i -e "s;@\[MULTIGRID\];${waveMULTIGRID};g" tmp1
fi

if [[ "${cplchm}" = ".true." ]]; then

  local chm_petlist_bounds="0 $(( ${CHMPETS}-1 ))"

  sed -i -e "s;@\[chm_model\];gocart;g" tmp1
  sed -i -e "s;@\[chm_petlist_bounds\];${chm_petlist_bounds};g" tmp1
  sed -i -e "s;@\[chm_omp_num_threads\];${CHMTHREADS};g" tmp1
  sed -i -e "s;@\[coupling_interval_fast_sec\];${CPL_FAST};g" tmp1
fi

mv tmp1 nems.configure

echo "$(cat nems.configure)"

if [[ "${cplflx}" = ".true." ]]; then

#Create other CMEPS mediator related files
cat > pio_in << EOF
&papi_inparm
  papi_ctr1_str = "PAPI_FP_OPS"
  papi_ctr2_str = "PAPI_NO_CTR"
  papi_ctr3_str = "PAPI_NO_CTR"
  papi_ctr4_str = "PAPI_NO_CTR"
/
&pio_default_inparm
  pio_async_interface = .false.
  pio_blocksize = -1
  pio_buffer_size_limit = -1
  pio_debug_level = 0
  pio_rearr_comm_enable_hs_comp2io = .true.
  pio_rearr_comm_enable_hs_io2comp = .false.
  pio_rearr_comm_enable_isend_comp2io = .false.
  pio_rearr_comm_enable_isend_io2comp = .true.
  pio_rearr_comm_fcd = "2denable"
  pio_rearr_comm_max_pend_req_comp2io = 0
  pio_rearr_comm_max_pend_req_io2comp = 64
  pio_rearr_comm_type = "p2p"
/
&prof_inparm
  profile_add_detail = .false.
  profile_barrier = .false.
  profile_depth_limit = 4
  profile_detail_limit = 2
  profile_disable = .false.
  profile_global_stats = .true.
  profile_outpe_num = 1
  profile_outpe_stride = 0
  profile_ovhd_measurement = .false.
  profile_papi_enable = .false.
  profile_single_file = .false.
  profile_timer = 4
/
EOF

echo "$(cat pio_in)"

cat > med_modelio.nml << EOF
&pio_inparm
  pio_netcdf_format = "64bit_offset"
  pio_numiotasks = -99
  pio_rearranger = 1
  pio_root = 1
  pio_stride = 36
  pio_typename = "netcdf"
/
EOF

echo "$(cat med_modelio.nml)"

fi

${NCP} "${HOMEgfs}/sorc/ufs_model.fd/tests/parm/fd_nems.yaml" fd_nems.yaml

echo "SUB ${FUNCNAME[0]}: Nems configured for ${confignamevarfornems}"

}
