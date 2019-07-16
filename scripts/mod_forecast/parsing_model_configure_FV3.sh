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

VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ] ; then
  echo $(date) EXECUTING $0 $* >&2
  set -x
fi

rm -f model_configure
cat > model_configure <<EOF
total_member:            $ENS_NUM
print_esmf:              ${print_esmf:-.true.}
PE_MEMBER01:             $NTASKS_FV3
start_year:              $SYEAR
start_month:             $SMONTH
start_day:               $SDAY
start_hour:              $SHOUR
start_minute:            0
start_second:            0
nhours_fcst:             $FHMAX
RUN_CONTINUE:            ${RUN_CONTINUE:-".false."}
ENS_SPS:                 ${ENS_SPS:-".false."}

dt_atmos:                $DELTIM
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
write_nemsioflip:        $WRITE_NEMSIOFLIP
write_fsyncflag:         $WRITE_FSYNCFLAG
imo:                     $LONB_IMO
jmo:                     $LATB_JMO

nfhout:                  $FHOUT
nfhmax_hf:               $FHMAX_HF
nfhout_hf:               $FHOUT_HF
nsout:                   $NSOUT
EOF
}
