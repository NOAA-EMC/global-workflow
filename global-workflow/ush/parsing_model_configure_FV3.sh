#! /usr/bin/env bash

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

local restile=$(echo "${CASE}" |cut -c2-)
local ichunk2d=$((4*restile))
local jchunk2d=$((2*restile))
local ichunk3d=$((4*restile))
local jchunk3d=$((2*restile))
local kchunk3d=1

rm -f model_configure
cat >> model_configure <<EOF
start_year:              ${tPDY:0:4}
start_month:             ${tPDY:4:2}
start_day:               ${tPDY:6:2}
start_hour:              ${tcyc}
start_minute:            0
start_second:            0
nhours_fcst:             ${FHMAX}
fhrot:                   ${IAU_FHROT:-0}

dt_atmos:                ${DELTIM}
calendar:                ${calendar:-'julian'}
restart_interval:        ${restart_interval} -1
output_1st_tstep_rst:    .false.

quilting:                ${QUILTING}
quilting_restart:        .true.
write_groups:            ${WRITE_GROUP:-1}
write_tasks_per_group:   ${WRTTASK_PER_GROUP:-24}
itasks:                  1
output_history:          ${OUTPUT_HISTORY:-".true."}
write_dopost:            ${WRITE_DOPOST:-".false."}
write_nsflip:            ${WRITE_NSFLIP:-".false."}
num_files:               ${NUM_FILES:-2}
filename_base:           'atm' 'sfc'
output_grid:             ${OUTPUT_GRID}
output_file:             '${OUTPUT_FILETYPE_ATM}' '${OUTPUT_FILETYPE_SFC}'
ichunk2d:                ${ichunk2d:-0}
jchunk2d:                ${jchunk2d:-0}
ichunk3d:                ${ichunk3d:-0}
jchunk3d:                ${jchunk3d:-0}
kchunk3d:                ${kchunk3d:-0}
ideflate:                ${ideflate:-1}
nbits:                   ${nbits:-14}
imo:                     ${LONB_IMO}
jmo:                     ${LATB_JMO}
output_fh:               ${FV3_OUTPUT_FH}
iau_offset:              ${IAU_OFFSET:-0}
EOF

echo "$(cat model_configure)"
}
