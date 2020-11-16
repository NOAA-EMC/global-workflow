#!/bin/ksh -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Loop over groups to Execute the JJOB
fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

for fhr in $fhrlst; do
    
    export FHMIN_EPOS=$fhr
    export FHMAX_EPOS=$fhr
    export FHOUT_EPOS=$fhr
    export job=epos${fhr}
    
	# Edited by liaofan on 2020.06.03
    #$HOMEgfs/jobs/JGDAS_ENKF_POST 
	$HOMEgfs/jobs/JGDAS_EFSOI_POST
	
    status=$?
    [[ $status -ne 0 ]] && exit $status

done

###############################################################
# Exit out cleanly
exit 0
