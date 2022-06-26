#! /bin/bash

#####
## "parsing_model_configure_DATM.sh"
## This script writes model configure file
## for DATM model
##
## This is the child script of ex-global forecast,
## writing model configure file for DATM
## This script is a direct execution.
#####
. $EXPDIR/config.defaults.datm
DATM_model_configure(){
echo "in datm_model_configure",$NFHOUT,$NFHOUT_HF

rm -f model_configure
cat > model_configure <<EOF
start_year:                $SYEAR
start_month:               $SMONTH
start_day:                 $SDAY
start_hour:                $SHOUR
start_minute:              0
start_second:              0
nhours_fcst:               $FHMAX
dt_atmos:                  ${ICETIM}
calendar:                  'julian'
nfrot:                     0
nfhout:                    ${NFHOUT}
nfhmax_hf:                 -1
nfhout_hf:                 ${NFHOUT_HF}
nsout:                     -1
EOF
echo "$(cat model_configure)"
}
