#/bin/sh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_vrfyrad.sh
# Script description:  Runs data extract/validation for global radiance diag data
#
# Author:        Ed Safford       Org: NP23         Date: 2012-01-18
#
# Abstract: This script runs the data extract/validation portion of the 
#           RadMon package.  
#
# Condition codes
#       0 - no problem encountered
#      >0 - some problem encountered
#
################################################################################
scr=exgdas_vrfyrad.sh
echo "${scr} HAS STARTED"

export VERBOSE=${VERBOSE:-"NO"} 
if [[ "$VERBOSE" = "YES" ]]
then
   set -x
fi


export RUN_ENVIR=${RUN_ENVIR:-nco}
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}
export envir=${envir:-prod}
export COMPONENT=${COMPONENT:-atmos}

#  Command line arguments
export PDY=${1:-${PDY:?}} 
export cyc=${2:-${cyc:?}}

#  Directories
export COM_IN=${COM_IN:-$(compath.py ${envir}/${NET}/${gfs_ver})}
export COMIN=${COMIN:-$COM_IN/${RUN}.${PDY}/${cyc}/$COMPONENT}


#  Filenames
export biascr=${biascr:-$COMIN/gdas.t${cyc}z.abias}
export radstat=${radstat:-$COMIN/gdas.t${cyc}z.radstat}
export satype_file=${satype_file:-${FIXgdas}/gdas_radmon_satype.txt}

#  Other variables
export RAD_AREA=${RAD_AREA:-glb}
export MAKE_CTL=${MAKE_CTL:-1}
export MAKE_DATA=${MAKE_DATA:-1}
export USE_ANL=${USE_ANL:-1}
export PDATE=${PDY}${cyc}
export DO_DIAG_RPT=${DO_DIAG_RPT:-1}
export DO_DATA_RPT=${DO_DATA_RPT:-1}
export USE_MAIL=${USE_MAIL:-0}
export MAIL_TO=${MAIL_TO:-" "}
export MAIL_CC=${MAIL_CC:-" "}
export NCP=${NCP:-/bin/cp}

###########################################################################
# ensure TANK dir exists, verify radstat and biascr are available
#
if [[ ! -d ${TANKverf_rad} ]]; then
   mkdir -p $TANKverf_rad
fi

if [[ "$VERBOSE" = "YES" ]]; then
   if [[ -s ${radstat} ]]; then
      echo ${radstat} is available
   fi
   if [[ -s ${biascr} ]]; then
      echo ${biascr} is available
   fi
fi
#####################################################################

data_available=0
if [[ -s ${radstat} && -s ${biascr} ]]; then
   data_available=1                                         

   #------------------------------------------------------------------
   #  Copy data files file to local data directory.  
   #  Untar radstat file.  
   #------------------------------------------------------------------

   $NCP $biascr  ./biascr.$PDATE
   $NCP $radstat ./radstat.$PDATE

   tar -xvf radstat.$PDATE
   rm radstat.$PDATE

   #------------------------------------------------------------------
   #  SATYPE is the list of expected satellite/instrument sources
   #  in the radstat file.  It should be stored in the $TANKverf 
   #  directory.  If it isn't there then use the $FIXgdas copy.  In all 
   #  cases write it back out to the radmon.$PDY directory.  Add any
   #  new sources to the list before writing back out.
   #------------------------------------------------------------------

   radstat_satype=`ls d*ges* | awk -F_ '{ print $2 "_" $3 }'`
   if [[ "$VERBOSE" = "YES" ]]; then
      echo $radstat_satype
   fi

   echo satype_file = $satype_file
  
   #------------------------------------------------------------------
   #  Get previous cycle's date, and look for the satype_file.  Using 
   #  the previous cycle will get us the previous day's directory if 
   #  the cycle being processed is 00z.
   #------------------------------------------------------------------
   if [[ $cyc = "00" ]]; then
      use_tankdir=${TANKverf_radM1}
   else
      use_tankdir=${TANKverf_rad}
   fi

   echo satype_file = $satype_file
   export SATYPE=`cat ${satype_file}`
   

   #-------------------------------------------------------------
   #  Update the SATYPE if any new sat/instrument was 
   #  found in $radstat_satype.  Write the SATYPE contents back 
   #  to $TANKverf/radmon.$PDY.
   #-------------------------------------------------------------
   satype_changes=0
   new_satype=$SATYPE
   for type in ${radstat_satype}; do
      test=`echo $SATYPE | grep $type | wc -l`

      if [[ $test -eq 0 ]]; then
         if [[ "$VERBOSE" = "YES" ]]; then
            echo "Found $type in radstat file but not in SATYPE list.  Adding it now."
         fi
         satype_changes=1
         new_satype="$new_satype $type"
      fi
   done

 
   #------------------------------------------------------------------
   # Rename the diag files and uncompress
   #------------------------------------------------------------------
   netcdf=0

   for type in ${SATYPE}; do

      if [[ netcdf -eq 0 && -e diag_${type}_ges.${PDATE}.nc4.${Z} ]]; then
         netcdf=1
      fi
     
      mv diag_${type}_ges.${PDATE}*.${Z} ${type}.${Z}
      ${UNCOMPRESS} ./${type}.${Z}
     
      if [[ $USE_ANL -eq 1 ]]; then
         mv diag_${type}_anl.${PDATE}*.${Z} ${type}_anl.${Z}
         ${UNCOMPRESS} ./${type}_anl.${Z}
      fi
   done

   export RADMON_NETCDF=$netcdf


   #------------------------------------------------------------------
   #   Run the child sccripts.
   #------------------------------------------------------------------
    ${USHradmon}/radmon_verf_angle.sh ${PDATE}
    rc_angle=$?

    ${USHradmon}/radmon_verf_bcoef.sh ${PDATE}
    rc_bcoef=$?

    ${USHradmon}/radmon_verf_bcor.sh ${PDATE}
    rc_bcor=$?

    ${USHradmon}/radmon_verf_time.sh ${PDATE}
    rc_time=$?

    #--------------------------------------
    #  optionally run clean_tankdir script
    #
    if [[ ${CLEAN_TANKVERF} -eq 1 ]]; then
       ${USHradmon}/clean_tankdir.sh glb 60
       rc_clean_tankdir=$?
       echo "rc_clean_tankdir = $rc_clean_tankdir"
    fi

fi



#####################################################################
# Postprocessing

err=0
if [[ ${data_available} -ne 1 ]]; then
   err=1
elif [[ $rc_angle -ne 0 ]]; then
   err=$rc_angle
elif [[ $rc_bcoef -ne 0 ]]; then
   err=$rc_bcoef
elif [[ $rc_bcor -ne 0 ]]; then
   err=$rc_bcor
elif [[ $rc_time -ne 0 ]]; then
   err=$rc_time
fi

#####################################################################
# Restrict select sensors and satellites
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
rlist="saphir"
for rtype in $rlist; do
    ${CHGRP_CMD} $TANKverf_rad/*${rtype}*
done


if [[ "$VERBOSE" = "YES" ]]; then
   echo "end exgdas_vrfyrad.sh, exit value = ${err}"
fi

echo "${scr} HAS ENDED"


set +x
exit ${err}

