#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         radmon_verf_bcor.sh
# Script description:  Extract bias correction data from radiance diagnostic 
#                      files.
#
# Author:        Ed  Safford       Org: NP23         Date: 2012-02-02
#
# Abstract:  This script extracts bias correction related data from radiance 
#            diagnostic files (which are an output from GSI runs), storing the 
#            extracted data in small binary files. 
#
#            This script is a child script of exgdas_vrfyrad.sh.sms.  The parent
#            script opens and uncompresses the radiance diagnostic file and copies
#            other supporting files into a temporary working directory.
#
#
# Usage:  radmon_verf_bcor.sh PDATE
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
#     LITTLE_ENDIAN     flag for little endian machine
#                       defaults to 0 (big endian)
#     USE_ANL           use analysis files as inputs in addition to 
#                         the ges files.  Default is 0 (ges only)
#
#   Modules and files referenced:
#     scripts    : 
#
#     programs   : $NCP
#                  $bcor_exec
#
#     fixed data : none
#
#     input data : $data_file
#                  
#     output data: $bcor_file
#                  $bcor_ctl
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


if [[ "$VERBOSE" = "YES" ]]; then
   set -ax
fi

# Directories
EXECradmon=${EXECradmon:-$(pwd)}
TANKverf_rad=${TANKverf_rad:-$(pwd)}

# File names
pgmout=${pgmout:-${jlogfile}}
touch $pgmout

# Other variables
RAD_AREA=${RAD_AREA:-glb}
SATYPE=${SATYPE:-}
VERBOSE=${VERBOSE:-NO}
LITTLE_ENDIAN=${LITTLE_ENDIAN:-0}
USE_ANL=${USE_ANL:-0}

bcor_exec=radmon_bcor.x
err=0

netcdf_boolean=".false."
if [[ $RADMON_NETCDF -eq 1 ]]; then
   netcdf_boolean=".true."
fi

if [[ $USE_ANL -eq 1 ]]; then
   gesanl="ges anl"
else
   gesanl="ges"
fi


#--------------------------------------------------------------------
#   Copy extraction program to working directory

$NCP ${EXECradmon}/${bcor_exec}  ./${bcor_exec}

if [[ ! -s ./${bcor_exec} ]]; then
   err=6
else


#--------------------------------------------------------------------
#   Run program for given time

   export pgm=${bcor_exec}

   iyy=`echo $PDATE | cut -c1-4`
   imm=`echo $PDATE | cut -c5-6`
   idd=`echo $PDATE | cut -c7-8`
   ihh=`echo $PDATE | cut -c9-10`

   ctr=0
   fail=0
   touch "./errfile"

   for type in ${SATYPE}; do

      for dtype in ${gesanl}; do

         prep_step

         ctr=`expr $ctr + 1`

         if [[ $dtype == "anl" ]]; then
            data_file=${type}_anl.${PDATE}.ieee_d
            bcor_file=bcor.${data_file}
            ctl_file=${type}_anl.ctl
            bcor_ctl=bcor.${ctl_file}
            stdout_file=stdout.${type}_anl
            bcor_stdout=bcor.${stdout_file}
            input_file=${type}_anl
         else
            data_file=${type}.${PDATE}.ieee_d
            bcor_file=bcor.${data_file}
            ctl_file=${type}.ctl
            bcor_ctl=bcor.${ctl_file}
            stdout_file=stdout.${type}
            bcor_stdout=bcor.${stdout_file}
            input_file=${type}
         fi

         rm input

      # Check for 0 length input file here and avoid running 
      # the executable if $input_file doesn't exist or is 0 bytes
      #
         if [[ -s $input_file ]]; then
            nchanl=-999

cat << EOF > input
 &INPUT
  satname='${type}',
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-720,
  incr=6,
  nchanl=${nchanl},
  suffix='${RADMON_SUFFIX}',
  gesanl='${dtype}',
  little_endian=${LITTLE_ENDIAN},
  rad_area='${RAD_AREA}',
  netcdf=${netcdf_boolean},
 /
EOF
   
            startmsg
            ./${bcor_exec} < input >> ${pgmout} 2>>errfile
            export err=$?; err_chk
            if [[ $? -ne 0 ]]; then
               fail=`expr $fail + 1`
            fi
 

#-------------------------------------------------------------------
#  move data, control, and stdout files to $TANKverf_rad and compress
#

            if [[ -s ${bcor_file} ]]; then
               ${COMPRESS} ${bcor_file}
            fi

            if [[ -s ${bcor_ctl} ]]; then
               ${COMPRESS} ${bcor_ctl}
            fi

         fi
      done  # dtype in $gesanl loop
   done     # type in $SATYPE loop


   ${USHradmon}/rstprod.sh
   tar_file=radmon_bcor.tar

   tar -cf $tar_file bcor*.ieee_d* bcor*.ctl*
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
      err=7
   fi
fi

################################################################################
#  Post processing

if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXITING $0 error code ${err} >&2
fi

exit ${err}

