#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         radmon_verf_angle.sh
# Script description:  Extract angle dependent data from radiance
#                      diagnostic files.
#
# Author:        Ed  Safford       Org: NP23         Date: 2012-02-02
#
# Abstract:  This script extracts angle dependent data from radiance
#            diagnostic files (which are an output from GSI runs),
#            storing the extracted data in small binary files.
#
#            This script is a child script of exgdas_vrfyrad.sh.sms.  The parent
#            script opens and uncompresses the radiance diagnostic file and copies
#            other supporting files into a temporary working directory.
#
#
# Usage:  radmon_verf_angle.sh PDATE
#
#   Input script positional parameters:
#     PDATE             processing date
#                       yyyymmddcc format; required
#
#   Imported Shell Variables:
#     RADMON_SUFFIX     data source suffix
#                       defauls to opr
#     EXECradmon        executable directory
#                       defaults to current directory
#     RAD_AREA          global or regional flag
#                       defaults to global
#     TANKverf_rad      data repository
#                       defaults to current directory
#     SATYPE            list of satellite/instrument sources
#                       defaults to none
#     VERBOSE           Verbose flag (YES or NO)
#                       defaults to NO
#     LITTLE_ENDIAN     flag to indicate LE machine
#                       defaults to 0 (big endian)
#     USE_ANL           use analysis files as inputs in addition to 
#                         the ges files.  Default is 0 (ges only)
#
#   Modules and files referenced:
#     scripts    : 
#
#     programs   : $NCP
#                  $angle_exec
#
#     fixed data : $scaninfo
#
#     input data : $data_file
#
#     output data: $angle_file
#                  $angle_ctl
#                  $pgmout
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
####################################################################

# Command line arguments.
RAD_AREA=${RAD_AREA:-glb}
REGIONAL_RR=${REGIONAL_RR:-0}	# rapid refresh model flag
rgnHH=${rgnHH:-}
rgnTM=${rgnTM:-}

export PDATE=${1:-${PDATE:?}}

echo " REGIONAL_RR, rgnHH, rgnTM = $REGIONAL_RR, $rgnHH, $rgnTM"
netcdf_boolean=".false."
if [[ $RADMON_NETCDF -eq 1 ]]; then
   netcdf_boolean=".true."
fi  
echo " RADMON_NETCDF, netcdf_boolean = ${RADMON_NETCDF}, $netcdf_boolean"

which prep_step
which startmsg

if [[ "$VERBOSE" = "YES" ]]; then
   set -ax
fi

# Directories
FIXgdas=${FIXgdas:-$(pwd)}
EXECradmon=${EXECradmon:-$(pwd)}
TANKverf_rad=${TANKverf_rad:-$(pwd)}

# File names
export pgmout=${pgmout:-${jlogfile}}
touch $pgmout

# Other variables
SATYPE=${SATYPE:-}
VERBOSE=${VERBOSE:-NO}
LITTLE_ENDIAN=${LITTLE_ENDIAN:-0}
USE_ANL=${USE_ANL:-0}


if [[ $USE_ANL -eq 1 ]]; then
   gesanl="ges anl"
else
   gesanl="ges"
fi

err=0
angle_exec=radmon_angle.x
shared_scaninfo=${shared_scaninfo:-$FIXgdas/gdas_radmon_scaninfo.txt}
scaninfo=scaninfo.txt

#--------------------------------------------------------------------
#   Copy extraction program and supporting files to working directory

$NCP ${EXECradmon}/${angle_exec}  ./
$NCP $shared_scaninfo  ./${scaninfo}

if [[ ! -s ./${angle_exec} || ! -s ./${scaninfo} ]]; then
   err=2
else
#--------------------------------------------------------------------
#   Run program for given time

   export pgm=${angle_exec}

   iyy=`echo $PDATE | cut -c1-4`
   imm=`echo $PDATE | cut -c5-6`
   idd=`echo $PDATE | cut -c7-8`
   ihh=`echo $PDATE | cut -c9-10`

   ctr=0
   fail=0
   touch "./errfile"

   for type in ${SATYPE}; do

      if [[ ! -s ${type} ]]; then
         echo "ZERO SIZED:  ${type}"
         continue
      fi

      for dtype in ${gesanl}; do

         echo "pgm    = $pgm"
         echo "pgmout = $pgmout"
         prep_step

         ctr=`expr $ctr + 1`

         if [[ $dtype == "anl" ]]; then
            data_file=${type}_anl.${PDATE}.ieee_d
            ctl_file=${type}_anl.ctl
            angl_ctl=angle.${ctl_file}
         else
            data_file=${type}.${PDATE}.ieee_d
            ctl_file=${type}.ctl
            angl_ctl=angle.${ctl_file}
         fi

         if [[ $REGIONAL_RR -eq 1 ]]; then
            angl_file=${rgnHH}.${data_file}.${rgnTM}
         fi


         rm input

         nchanl=-999
cat << EOF > input
 &INPUT
  satname='${type}',
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-720,
  incr=${CYCLE_INTERVAL},
  nchanl=${nchanl},
  suffix='${RADMON_SUFFIX}',
  gesanl='${dtype}',
  little_endian=${LITTLE_ENDIAN},
  rad_area='${RAD_AREA}',
  netcdf=${netcdf_boolean},
 /
EOF

	 startmsg
         ./${angle_exec} < input >>   ${pgmout} 2>>errfile
         export err=$?; err_chk
         if [[ $err -ne 0 ]]; then
             fail=`expr $fail + 1`
         fi

#-------------------------------------------------------------------
#  move data, control, and stdout files to $TANKverf_rad and compress

         #-----------------------------------------------------------
         #  For cfp use, instead of executing these file manipulation 
	 #  comands directly, journel them instead to a flat file.
	 #  Then execute cfp (as appropriate for each machine) to
	 #  perform the file manipulations.  Those manipulation 
         #  commands should probably go in a parm file so I can not
	 #  rely on a machine dependent decision here.
	 #
	 #  Note that in my esafford_RadMon_45526 branch I've already
	 #  modified the order of operation to compress before copy.
	 #  That change should be delivered to trunk soon.
         #-----------------------------------------------------------


         if [[ -s ${angl_file} ]]; then
            ${COMPRESS} -f ${angl_file}
         fi

         if [[ -s ${angl_ctl} ]]; then
            ${COMPRESS} -f ${angl_ctl}
         fi 


      done    # for dtype in ${gesanl} loop

   done    # for type in ${SATYPE} loop


   ${USHradmon}/rstprod.sh

   tar_file=radmon_angle.tar 
   tar -cf $tar_file angle*.ieee_d* angle*.ctl*
   ${COMPRESS} ${tar_file}
   mv $tar_file.${Z} ${TANKverf_rad}/.

   if [[ $RAD_AREA = "rgn" ]]; then
      cwd=`pwd`
      cd ${TANKverf_rad}
      tar -xf ${tar_file}.${Z}
      rm ${tar_file}.${Z}
      cd ${cwd}
   fi   

   if [[ $fail -eq $ctr || $fail -gt $ctr ]]; then
      err=3
   fi
fi

################################################################################
#  Post processing

if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXITING $0 error code ${err} >&2
fi


echo "<-- radmon_verf_angle.sh"
exit ${err}
