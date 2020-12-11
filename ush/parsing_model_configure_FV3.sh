#! /bin/sh

##### 
## "parsing_model_configure_FV3.sh"
## This script writes model configure file
## for FV3 model
##
## This is the child script of ex-global forecast,
## writing model configure file for FV3
## This script is a direct execution.
#####

FV3_model_configure(){

rm -f model_configure
cat >> model_configure <<EOF
total_member:            $ENS_NUM
print_esmf:              ${print_esmf:-.true.}
PE_MEMBER01:             $NTASKS_TOT
start_year:              ${tPDY:0:4}
start_month:             ${tPDY:4:2}
start_day:               ${tPDY:6:2}
start_hour:              ${tcyc}
start_minute:            0
start_second:            0
fhrot:                   ${IAU_FHROT:-0}
nhours_fcst:             $FHMAX
RUN_CONTINUE:            ${RUN_CONTINUE:-".false."}
ENS_SPS:                 ${ENS_SPS:-".false."}

dt_atmos:                $DELTIM
output_1st_tstep_rst:    .false.
calendar:                ${calendar:-'julian'}
cpl:                     ${cpl:-".false."}
memuse_verbose:          ${memuse_verbose:-".false."}
atmos_nthreads:          $NTHREADS_FV3
use_hyper_thread:        ${hyperthread:-".false."}
ncores_per_node:         $cores_per_node
restart_interval:        $restart_interval

quilting:                $QUILTING
write_groups:            ${WRITE_GROUP:-1}
write_tasks_per_group:   ${WRTTASK_PER_GROUP:-24}
output_history:          ${OUTPUT_HISTORY:-".true."}
write_dopost:            ${WRITE_DOPOST:-".false."}
num_files:               ${NUM_FILES:-2}
filename_base:           'atm' 'sfc'
output_grid:             $OUTPUT_GRID
output_file:             $OUTPUT_FILE
ichunk2d:                ${ichunk2d:-0}
jchunk2d:                ${jchunk2d:-0}
ichunk3d:                ${ichunk3d:-0}
jchunk3d:                ${jchunk3d:-0}
kchunk3d:                ${kchunk3d:-0}
ideflate:                ${ideflate:-1}
nbits:                   ${nbits:-14}
write_nemsioflip:        $WRITE_NEMSIOFLIP
write_fsyncflag:         $WRITE_FSYNCFLAG
imo:                     $LONB_IMO
jmo:                     $LATB_JMO

nfhout:                  $FHOUT
nfhmax_hf:               $FHMAX_HF
nfhout_hf:               $FHOUT_HF
nsout:                   $NSOUT
iau_offset:              ${IAU_OFFSET:-0}
EOF

if [ $cpl = .true. ]; then
cat >> model_configure <<EOF
atm_coupling_interval_sec:      $DELTIM
output_history:          ${OUTPUT_HISTORY:-".true."}
EOF
elif [ $cpl = .false. ]; then
cat >> model_configure <<EOF
ideflate:                ${ideflate:-1}
nbits:                   ${nbits:-14}
iau_offset:              ${IAU_OFFSET:-0}
EOF
fi

echo "$(cat model_configure)"
}
