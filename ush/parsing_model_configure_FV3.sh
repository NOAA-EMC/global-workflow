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
start_year:              ${sPDY:0:4}
start_month:             ${sPDY:4:2}
start_day:               ${sPDY:6:2}
start_hour:              ${scyc}
start_minute:            0
start_second:            0
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
num_files:               ${NUM_FILES:-2}
filename_base:           'atm' 'sfc'
output_grid:             $OUTPUT_GRID
output_file:             $OUTPUT_FILE
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
write_dopost:            ${WRITE_DOPOST:-".false."}
EOF
fi

$NCP model_configure $DATA/model_configure

echo "$(cat model_configure)"
}
