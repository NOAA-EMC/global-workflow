#!/bin/sh

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
if [[ $inistep = "cold" ]]; then
  restart_interval=0
  coldstart=true     # this is the correct setting
else
  restart_interval=${restart_interval:-3024000}    # Interval in seconds to write restarts
  coldstart=false
fi

ATM_model=${ATM_model:-'fv3'}
OCN_model=${OCN_model:-'mom6'}
ICE_model=${ICE_model:-'cice'}
WAV_model=${WAV_model:-'ww3'}

ATMPETS=${ATMPETS:-8}
MEDPETS=${MEDPETS:-8} 
OCNPETS=${OCNPETS:-8}
ICEPETS=${ICEPETS:-8}
WAVPETS=${WAVPETS:-8}

rm -f $DATA/nems.configure

med_petlist_bounds=${med_petlist_bounds:-"0 $(( $MEDPETS-1 ))"}
atm_petlist_bounds=${atm_petlist_bounds:-"0 $(( $ATMPETS-1 ))"} 
ocn_petlist_bounds=${ocn_petlist_bounds:-"$ATMPETS $(( $ATMPETS+$OCNPETS-1 ))"}  
ice_petlist_bounds=${ice_petlist_bounds:-"$(( $ATMPETS+$OCNPETS )) $(( $ATMPETS+$OCNPETS+$ICEPETS-1 ))"} 
wav_petlist_bounds=${wav_petlist_bounds:-"$(( $ATMPETS+$OCNPETS+$ICEPETS )) $(( $ATMPETS+$OCNPETS+$ICEPETS+$WAVPETS-1 ))"} 

# Copy the selected template into run directory
cp $SCRIPTDIR/nems.configure.$confignamevarfornems.IN tmp1
sed -i -e "s;@\[med_model\];nems;g" tmp1
sed -i -e "s;@\[atm_model\];$ATM_model;g" tmp1
sed -i -e "s;@\[med_petlist_bounds\];$med_petlist_bounds;g" tmp1
sed -i -e "s;@\[atm_petlist_bounds\];$atm_petlist_bounds;g" tmp1

if [ $cplflx = .true. ]; then
    if [ $restart_interval  -gt 0 ]; then 
      restart_interval_nems=$restart_interval 
    else 
      restart_interval_nems=$FHMAX 
    fi
        sed -i -e "s;@\[ocn_model\];$OCN_model;g" tmp1
	sed -i -e "s;@\[ocn_petlist_bounds\];$ocn_petlist_bounds;g" tmp1
	sed -i -e "s;@\[DumpFields\];$DumpFields;g" tmp1
	sed -i -e "s;@\[coldstart\];$coldstart;g" tmp1
	sed -i -e "s;@\[restart_interval\];$restart_interval;g" tmp1
	sed -i -e "s;@\[CPL_SLOW\];$CPL_SLOW;g" tmp1
	sed -i -e "s;@\[CPL_FAST\];$CPL_FAST;g" tmp1
        sed -i -e "s;@\[restart_interval_hours\];$restart_interval_nems;g" tmp1
fi
if [ $cplwav = .true. ]; then
	sed -i -e "s;@\[wav_model\];ww3;g" tmp1
        sed -i -e "s;@\[wav_petlist_bounds\];$wav_petlist_bounds;g" tmp1
fi
if [ $cplice = .true. ]; then
	sed -i -e "s;@\[ice_model\];$ICE_model;g" tmp1
	sed -i -e "s;@\[ice_petlist_bounds\];$ice_petlist_bounds;g" tmp1
fi
if [ $cplchem = .true. ]; then
	sed -i -e "s;@\[chem_model\];gsd;g" tmp1
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

cp $HOMEgfs/sorc/fv3_coupled.fd/CMEPS/mediator/fd_nems.yaml fd_nems.yaml

fi 

echo "SUB ${FUNCNAME[0]}: Nems configured for $confignamevarfornems"

}
