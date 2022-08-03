#! /usr/bin/env bash

#####
## This script writes nems.configure file
## first, select a "*.IN" templates based on
## $confignamevarfornems and parse values based on
## $cpl** switches.
##
## This is a child script of modular
## forecast script. This script is definition only
#####
writing_nems_configure()
{
echo "SUB ${FUNCNAME[0]}: parsing_nems_configure begins"
if [ -e $SCRIPTDIR/nems.configure ]; then
  rm -f $SCRIPTDIR/nems.configure
fi

# Setup nems.configure
DumpFields=${NEMSDumpFields:-false}
cap_dbug_flag=${cap_dbug_flag:-0}
if [ $warm_start = ".true." ]; then
  cmeps_run_type='continue'
else
  cmeps_run_type='startup'
fi
restart_interval=${restart_interval:-3024000}    # Interval in seconds to write restarts

ATM_model=${ATM_model:-'fv3'}
OCN_model=${OCN_model:-'mom6'}
ICE_model=${ICE_model:-'cice'}
WAV_model=${WAV_model:-'ww3'}
CHM_model=${CHM_model:-'gocart'}

ATMPETS=${ATMPETS:-8}
MEDPETS=${MEDPETS:-8}
OCNPETS=${OCNPETS:-0}
ICEPETS=${ICEPETS:-0}
WAVPETS=${WAVPETS:-0}
CHMPETS=${CHMPETS:-${ATMPETS}}

USE_MOMMESH=${USE_MOMMESH:-"true"}
MESH_OCN_ICE=${MESH_OCN_ICE:-"mesh.mx${ICERES}.nc"}

if [[ $OCNRES = "100" ]];  then
  EPS_IMESH='2.5e-1'
else
  EPS_IMESH='1.0e-1'
fi

rm -f $DATA/nems.configure

med_petlist_bounds=${med_petlist_bounds:-"0 $(( $MEDPETS-1 ))"}
atm_petlist_bounds=${atm_petlist_bounds:-"0 $(( $ATMPETS-1 ))"}
ocn_petlist_bounds=${ocn_petlist_bounds:-"$ATMPETS $(( $ATMPETS+$OCNPETS-1 ))"}
ice_petlist_bounds=${ice_petlist_bounds:-"$(( $ATMPETS+$OCNPETS )) $(( $ATMPETS+$OCNPETS+$ICEPETS-1 ))"}
wav_petlist_bounds=${wav_petlist_bounds:-"$(( $ATMPETS+$OCNPETS+$ICEPETS )) $(( $ATMPETS+$OCNPETS+$ICEPETS+$WAVPETS-1 ))"}
chm_petlist_bounds=${chm_petlist_bounds:-"0 $(( $CHMPETS-1 ))"}

esmf_logkind=${esmf_logkind:-"ESMF_LOGKIND_MULTI"} #options: ESMF_LOGKIND_MULTI_ON_ERROR, ESMF_LOGKIND_MULTI, ESMF_LOGKIND_NONE

# Copy the selected template into run directory
infile="$SCRIPTDIR/nems.configure.$confignamevarfornems.IN"
if [ -s $infile ]; then
  cp $infile tmp1
else
  echo "FATAL ERROR: nem.configure template '$infile' does not exist!"
  exit 1
fi
sed -i -e "s;@\[med_model\];cmeps;g" tmp1
sed -i -e "s;@\[atm_model\];$ATM_model;g" tmp1
sed -i -e "s;@\[med_petlist_bounds\];$med_petlist_bounds;g" tmp1
sed -i -e "s;@\[atm_petlist_bounds\];$atm_petlist_bounds;g" tmp1
sed -i -e "s;@\[esmf_logkind\];$esmf_logkind;g" tmp1

if [ $cpl = ".true." ]; then
  sed -i -e "s;@\[coupling_interval_slow_sec\];$CPL_SLOW;g" tmp1
fi

if [ $cplflx = .true. ]; then
  if [ $restart_interval  -gt 0 ]; then
    restart_interval_nems=$restart_interval
  else
    restart_interval_nems=$FHMAX
  fi
  sed -i -e "s;@\[ocn_model\];$OCN_model;g" tmp1
  sed -i -e "s;@\[ocn_petlist_bounds\];$ocn_petlist_bounds;g" tmp1
  sed -i -e "s;@\[DumpFields\];$DumpFields;g" tmp1
  sed -i -e "s;@\[cap_dbug_flag\];$cap_dbug_flag;g" tmp1
  sed -i -e "s;@\[use_coldstart\];$use_coldstart;g" tmp1
  sed -i -e "s;@\[RUNTYPE\];$cmeps_run_type;g" tmp1
  sed -i -e "s;@\[CPLMODE\];$cplmode;g" tmp1
  sed -i -e "s;@\[restart_interval\];$restart_interval;g" tmp1
  sed -i -e "s;@\[coupling_interval_fast_sec\];$CPL_FAST;g" tmp1
  sed -i -e "s;@\[RESTART_N\];$restart_interval_nems;g" tmp1
  sed -i -e "s;@\[use_mommesh\];$USE_MOMMESH;g" tmp1
  sed -i -e "s;@\[eps_imesh\];$EPS_IMESH;g" tmp1
  sed -i -e "s;@\[ATMTILESIZE\];$RESTILE;g" tmp1
fi
if [ $cplwav = .true. ]; then
  sed -i -e "s;@\[wav_model\];ww3;g" tmp1
  sed -i -e "s;@\[wav_petlist_bounds\];$wav_petlist_bounds;g" tmp1
  sed -i -e "s;@\[MESH_WAV\];$MESH_WAV;g" tmp1
  sed -i -e "s;@\[MULTIGRID\];$waveMULTIGRID;g" tmp1
fi
if [ $cplice = .true. ]; then
  sed -i -e "s;@\[ice_model\];$ICE_model;g" tmp1
  sed -i -e "s;@\[ice_petlist_bounds\];$ice_petlist_bounds;g" tmp1
  sed -i -e "s;@\[MESH_OCN_ICE\];$MESH_OCN_ICE;g" tmp1
  sed -i -e "s;@\[FHMAX\];$FHMAX_GFS;g" tmp1
fi
if [ $cplchm = .true. ]; then
  sed -i -e "s;@\[chm_model\];$CHM_model;g" tmp1
  sed -i -e "s;@\[chm_petlist_bounds\];$chm_petlist_bounds;g" tmp1
  sed -i -e "s;@\[coupling_interval_fast_sec\];$CPL_FAST;g" tmp1
fi

mv tmp1 nems.configure

echo "$(cat nems.configure)"

if [ $cplflx = .true. ]; then

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

cp $HOMEgfs/sorc/ufs_model.fd/tests/parm/fd_nems.yaml fd_nems.yaml

echo "SUB ${FUNCNAME[0]}: Nems configured for $confignamevarfornems"

}
