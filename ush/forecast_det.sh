#! /bin/sh

##### 
## "forecast_def.sh"
## This script sets value of all variables
##
## This is the child script of ex-global forecast,
## This script is a definition of functions.
#####


# For all non-evironment variables
# Cycling and forecast hour specific parameters

FV3_GFS_det(){
	#-------------------------------------------------------
	# warm start?
	warm_start=${warm_start:-".false."}
	read_increment=${read_increment:-".false."}
	restart_interval=${restart_interval:-0}
        res_latlon_dynamics="''"

        # Determine if this is a warm start or cold start
	if [ -f $gmemdir/RESTART/${PDY}.${cyc}0000.coupler.res ]; then
	  export warm_start=".true."
	fi

        # turn IAU off for cold start
        DOIAU_coldstart="NO"
        if [ $DOIAU = "YES" -a $warm_start = ".false." ]; then
          export DOIAU="NO"
          echo "turning off IAU since warm_start = $warm_start"
          DOIAU_coldstart="YES"
          sCDATE=$CDATE
          sPDY=$PDY
          scyc=$cyc
          #echo "ERROR: DOIAU = $DOIAU and warm_start = $warm_start are incompatible."
          #echo "Abort!"
          #exit 99
        fi

	#-------------------------------------------------------
	# determine if restart IC exists to continue from a previous forecast
	RERUN="NO"
	filecount=$(find $RSTDIR_TMP -type f | wc -l)
	if [ $CDUMP = "gfs" -a $restart_interval -gt 0 -a $FHMAX -gt $restart_interval -a $filecount -gt 10 ]; then
	    SDATE=$($NDATE +$FHMAX $CDATE)
	    EDATE=$($NDATE +$restart_interval $CDATE)
	    while [ $SDATE -gt $EDATE ]; do
	        PDYS=$(echo $SDATE | cut -c1-8)
	        cycs=$(echo $SDATE | cut -c9-10)
	        flag1=$RSTDIR_TMP/${PDYS}.${cycs}0000.coupler.res
	        flag2=$RSTDIR_TMP/coupler.res
	        if [ -s $flag1 ]; then
	            mv $flag1 ${flag1}.old
	            if [ -s $flag2 ]; then mv $flag2 ${flag2}.old ;fi
	            RERUN="YES"
	            CDATE_RST=$($NDATE -$restart_interval $SDATE)
	            break
	        fi
	        SDATE=$($NDATE -$restart_interval $SDATE)
	    done
	fi
	#-------------------------------------------------------
}

FV3_GEFS_det(){
	echo "SUB ${FUNCNAME[0]}: Defining variables for FV3GEFS"
}

WW3_det(){
	echo "SUB ${FUNCNAME[0]}: Run type determination for WW3"
}

CICE_det(){
	echo "SUB ${FUNCNAME[0]}: Run type determination for CICE"
	export histfreq_n=$FHOUT

	# Create ice_in file
	
	if [ $inistep = "restart" ] ; then
	  runtyp=continue
	  restim=.true.
	else
	  runtyp=initial
	  restim=.false.
	fi
}

MOM6_det(){
	echo "SUB ${FUNCNAME[0]}: Run type determination for MOM6"
	export histfreq_n=$FHOUT

	# Create ice_in file

	if [ $inistep = "restart" ] ; then
	  runtyp=continue
	  restim=.true.
	else
	  runtyp=initial
	  restim=.false.
	fi
}
