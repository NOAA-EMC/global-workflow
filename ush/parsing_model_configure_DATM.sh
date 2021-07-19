#! /bin/sh

##### 
## "parsing_model_configure_DATM.sh"
## This script writes model configure file
## for DATM model
##
## This is the child script of ex-global forecast,
## writing model configure file for DATM
## This script is a direct execution.
#####

DATM_model_configure(){

rm -f model_configure
cat > model_configure <<EOF
total_member:              ${ENS_NUM:-1}
print_esmf:                ${print_esmf:-.true.}
PE_MEMBER01:               $NTASKS_TOT
start_year:                $SYEAR
start_month:               $SMONTH
start_day:                 $SDAY
start_hour:                $SHOUR
start_minute:              0
start_second:              0
nhours_fcst:               $FHMAX
RUN_CONTINUE:              ${RUN_CONTINUE:-".false."}
ENS_SPS:                   ${ENS_SPS:-".false."}

dt_atmos:                  ${DT_ATMOS}
atm_coupling_interval_sec: ${coupling_interval_fast_sec}

iatm:                      ${IATM}
jatm:                      ${JATM}
cdate0:                    ${CDATE}
nfhout:                    ${NFHOUT} 
filename_base:             ${DATM_FILENAME_BASE}
EOF
echo "$(cat model_configure)"
}
