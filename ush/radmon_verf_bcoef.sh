#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         radmon_verf_bcoef.sh
# Script description:  Extract bias correction coefficients data from radiance 
#                      diagnostic files.
#
# Author:        Ed  Safford       Org: NP23         Date: 2012-02-02
#
# Abstract:  This script extracts bias correction coefficient related data from 
#            radiance diagnostic files (which are an output from GSI runs), 
#            storing the extracted data in small binary files.
#
#            This script is a child script of exgdas_vrfyrad.sh.sms.  The parent
#            script opens and uncompresses the radiance diagnostic file and copies
#            other supporting files into a temporary working directory.
#
#
# Usage:  radmon_verf_bcoef.sh PDATE
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
#     FIXradmon         fixed data directory
#                       defaults to current directory
#     RAD_AREA          global or regional flag
#                       defaults to global
#     TANKverf_rad      data repository
#                       defaults to current directory
#     SATYPE            list of satellite/instrument sources
#                       defaults to none
#     VERBOSE           Verbose flag (YES or NO)
#                       defaults to NO
#     LITTLE_ENDIAN     flag for LE machine
#                       defaults to 0 (big endian)
#     USE_ANL		use analysis files as inputs in addition to 
#                         the ges files.  Default is 0 (ges only)
#
#   Modules and files referenced:
#     scripts    : 
#
#     programs   : $NCP
#                  $bcoef_exec
#
#     fixed data : $biascr
#
#     input data : $data_file
#
#     output data: $bcoef_file
#                  $bcoef_ctl
#                  $pgmout
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
####################################################################
#  Command line arguments.
export PDATE=${1:-${PDATE:?}}

netcdf_boolean=".false."
if [[ $RADMON_NETCDF -eq 1 ]]; then
   netcdf_boolean=".true."
fi
echo " RADMON_NETCDF, netcdf_boolean = ${RADMON_NETCDF}, $netcdf_boolean"

if [[ "$VERBOSE" = "YES" ]]; then
   set -ax
fi


# Directories
FIXgdas=${FIXgdas:-$(pwd)}
EXECradmon=${EXECradmon:-$(pwd)}
TANKverf_rad=${TANKverf_rad:-$(pwd)}

# File names
pgmout=${pgmout:-${jlogfile}}
touch $pgmout

# Other variables
RAD_AREA=${RAD_AREA:-glb}
REGIONAL_RR=${REGIONAL_RR:-0}
rgnHH=${rgnHH:-}
rgnTM=${rgnTM:-}
SATYPE=${SATYPE:-}
VERBOSE=${VERBOSE:-NO}
LITTLE_ENDIAN=${LITTLE_ENDIAN:-0}
USE_ANL=${USE_ANL:-0}


err=0
bcoef_exec=radmon_bcoef.x

if [[ $USE_ANL -eq 1 ]]; then
   gesanl="ges anl"
else
   gesanl="ges"
fi

#--------------------------------------------------------------------
#   Copy extraction program and supporting files to working directory

$NCP $EXECradmon/${bcoef_exec}              ./${bcoef_exec}
$NCP ${biascr}                              ./biascr.txt

if [[ ! -s ./${bcoef_exec} || ! -s ./biascr.txt ]]; then
   err=4
else


#--------------------------------------------------------------------
#   Run program for given time

   export pgm=${bcoef_exec}

   iyy=`echo $PDATE | cut -c1-4`
   imm=`echo $PDATE | cut -c5-6`
   idd=`echo $PDATE | cut -c7-8`
   ihh=`echo $PDATE | cut -c9-10`

   ctr=0
   fail=0

   nchanl=-999
   npredr=5

   for type in ${SATYPE}; do

      if [[ ! -s ${type} ]]; then
         echo "ZERO SIZED:  ${type}"
         continue
      fi

      for dtype in ${gesanl}; do

         prep_step

         ctr=`expr $ctr + 1`

         if [[ $dtype == "anl" ]]; then
            data_file=${type}_anl.${PDATE}.ieee_d
            ctl_file=${type}_anl.ctl
            bcoef_ctl=bcoef.${ctl_file}
         else
            data_file=${type}.${PDATE}.ieee_d
            ctl_file=${type}.ctl
            bcoef_ctl=bcoef.${ctl_file}
         fi 

         if [[ $REGIONAL_RR -eq 1 ]]; then
            bcoef_file=${rgnHH}.bcoef.${data_file}.${rgnTM}
         else
            bcoef_file=bcoef.${data_file}
         fi
 

         rm input


cat << EOF > input
 &INPUT
  satname='${type}',
  npredr=${npredr},
  nchanl=${nchanl},
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-720,
  incr=${CYCLE_INTERVAL},
  suffix='${RADMON_SUFFIX}',
  gesanl='${dtype}',
  little_endian=${LITTLE_ENDIAN},
  netcdf=${netcdf_boolean},
 /
EOF
         startmsg
         ./${bcoef_exec} < input >>${pgmout} 2>>errfile
         export err=$?; err_chk
         if [[ $err -ne 0 ]]; then
            fail=`expr $fail + 1`
         fi


#-------------------------------------------------------------------
#  move data, control, and stdout files to $TANKverf_rad and compress
#

         if [[ -s ${bcoef_file} ]]; then
            ${COMPRESS} ${bcoef_file}
         fi

         if [[ -s ${bcoef_ctl} ]]; then
            ${COMPRESS} ${bcoef_ctl}
         fi


      done  # dtype in $gesanl loop
   done     # type in $SATYPE loop


   ${USHradmon}/rstprod.sh

   tar_file=radmon_bcoef.tar
   tar -cf $tar_file bcoef*.ieee_d* bcoef*.ctl*
   ${COMPRESS} ${tar_file}
   mv $tar_file.${Z} ${TANKverf_rad}

   if [[ $RAD_AREA = "rgn" ]]; then
      cwd=`pwd`
      cd ${TANKverf_rad}
      tar -xf ${tar_file}.${Z}
      rm ${tar_file}.${Z}
      cd ${cwd}
   fi

   if [[ $fail -eq $ctr || $fail -gt $ctr ]]; then
      err=5
   fi
fi


################################################################################
#  Post processing
if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXITING $0 with error code ${err} >&2
fi


exit ${err}
