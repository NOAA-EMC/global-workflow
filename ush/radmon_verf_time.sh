#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         radmon_verf_time.sh
# Script description:  Extract time data from radiance diagnostic files,
#                      perform data integrity checks.
#
# Author:        Ed  Safford       Org: NP23         Date: 2012-02-02
#
# Abstract:  This script extracts time related data from radiance diagnostic
#            files (which are an output from GSI runs), storing the extracted
#            data in small binary files.  Data integrity checks are performed
#            on the data and mail messages are sent if potential errors are
#            detected.
#
#            This script is a child script of exgdas_vrfyrad.sh.sms.  The parent
#            script opens and uncompresses the radiance diagnostic file and copies
#            other supporting files into a temporary working directory. 
#
#
# Usage:  radmon_verf_time.sh PDATE
#
#   Input script positional parameters:
#     PDATE		processing date
#  			yyyymmddcc format; required 
#
#   Imported Shell Variables:
#     DO_DIAG_RPT	switch to build the diagnostic report
#			defaults to 1 (on)
#     DO_DATA_RPT	switch to build the data report
#                       defaults to 1 (on)
#     RADMON_SUFFIX	data source suffix
#                       defauls to opr
#     MAKE_CTL          switch to construct the meta-data control file
#                       defaults to 1 (on)
#     MAKE_DATA         switch to construct the binary data file
#                       defaults to 1 (on)
#     EXECradmon        executable directory
#                       defaults to current directory 
#     FIXgdas           fixed data directory
#                       defaults to current directory
#     RAD_AREA          global or regional flag
#                       defaults to global
#     TANKverf_rad	data repository
#                       defaults to current directory 
#     SATYPE		list of satellite/instrument sources
#        		defaults to none
#     MAIL_TO		email recipients
#			defaults to none
#     MAIL_CC		email cc recipients
#			defaults to none
#     VERBOSE           Verbose flag (YES or NO)
#                       defaults to NO
#     LITTLE_ENDIAN     flag for little endian machine
#                       defaults to 0 (big endian)
#     USE_ANL		use analysis files as inputs in addition to 
#                         the ges files.  Default is 0 (ges only)
#
#   Modules and files referenced:
#     scripts    : 
#
#     programs   : $NCP
#                  $time_exec
#
#     fixed data : gdas_radmon_base.tar
#
#     input data : $data_file
#                  
#     output data: $time_file
#                  $time_ctl
#                  $pgmout
#                  $bad_pen
#                  $bad_chan
#                  $report
#                  $diag_report
#                   
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

scr=radmon_verf_time.sh
msg="${scr} HAS STARTED"
postmsg "$jlogfile" "$msg"

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

radmon_err_rpt=${radmon_err_rpt:-${USHradmon}/radmon_err_rpt.sh}
base_file=${base_file:-$FIXgdas/gdas_radmon_base.tar}
report=report.txt
disclaimer=disclaimer.txt
region=region.txt
diag_report=diag_report.txt
diag_hdr=diag_hdr.txt
diag=diag.txt
obs_err=obs_err.txt
obs_hdr=obs_hdr.txt
pen_err=pen_err.txt
pen_hdr=pen_hdr.txt
chan_err=chan_err.txt
chan_hdr=chan_hdr.txt

# Other variables
DO_DIAG_RPT=${DO_DIAG_RPT:-1}
DO_DATA_RPT=${DO_DATA_RPT:-1}
RADMON_SUFFIX=${RADMON_SUFFIX:-opr}
MAKE_CTL=${MAKE_CTL:-1}
MAKE_DATA=${MAKE_DATA:-1}
RAD_AREA=${RAD_AREA:-glb}
REGIONAL_RR=${REGIONAL_RR:-0}
rgnHH=${rgnHH:-}
rgnTM=${rgnTM:-}
SATYPE=${SATYPE:-}
MAIL_TO=${MAIL_TO:-}
MAIL_CC=${MAIL_CC:-}
VERBOSE=${VERBOSE:-NO}
LITTLE_ENDIAN=${LITTLE_ENDIAN:-0}
USE_MAIL=${USE_MAIL:-0}

time_exec=radmon_time
USE_ANL=${USE_ANL:-0}
err=0 

if [[ $USE_ANL -eq 1 ]]; then
   gesanl="ges anl"
else
   gesanl="ges"
fi


#--------------------------------------------------------------------
#   Copy extraction program and base files to working directory
#-------------------------------------------------------------------
$NCP ${EXECradmon}/${time_exec}  ./
if [[ ! -s ./${time_exec} ]]; then
   err=8
fi

iyy=`echo $PDATE | cut -c1-4`
imm=`echo $PDATE | cut -c5-6`
idd=`echo $PDATE | cut -c7-8`
ihh=`echo $PDATE | cut -c9-10`
cyc=$ihh
CYCLE=$cyc

local_base="local_base"
if [[ $DO_DATA_RPT -eq 1 ]]; then

   if [[ -e ${base_file}.${Z} ]]; then
      $NCP ${base_file}.${Z}  ./${local_base}.{Z}
      ${UNCOMPRESS} ${local_base}.${Z}
   else
      $NCP ${base_file}  ./${local_base}
   fi

   if [[ ! -s ./${local_base} ]]; then
      echo "RED LIGHT: local_base file not found"
   else
      echo "Confirming local_base file is good = ${local_base}"
      tar -xf ./${local_base}
      echo "local_base is untarred"
   fi
fi

if [[ $err -eq 0 ]]; then
   ctr=0
   fail=0

   export pgm=${time_exec}
#--------------------------------------------------------------------
#   Loop over each entry in SATYPE
#--------------------------------------------------------------------
   for type in ${SATYPE}; do

      if [[ ! -s ${type} ]]; then
         echo "ZERO SIZED:  ${type}"
         continue
      fi

      ctr=`expr $ctr + 1`

      for dtype in ${gesanl}; do

         prep_step

         rm input

         if [[ $dtype == "anl" ]]; then
            data_file=${type}_anl.${PDATE}.ieee_d
            ctl_file=${type}_anl.ctl
            time_ctl=time.${ctl_file}
         else
            data_file=${type}.${PDATE}.ieee_d
            ctl_file=${type}.ctl
            time_ctl=time.${ctl_file}
         fi

         if [[ $REGIONAL_RR -eq 1 ]]; then
            time_file=${rgnHH}.time.${data_file}.${rgnTM}
         else
            time_file=time.${data_file}
         fi

#--------------------------------------------------------------------
#   Run program for given satellite/instrument
#--------------------------------------------------------------------
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
  imkctl=${MAKE_CTL},
  imkdata=${MAKE_DATA},
  gesanl='${dtype}',
  little_endian=${LITTLE_ENDIAN},
  rad_area='${RAD_AREA}',
 /
EOF
	startmsg

        ./${time_exec} < input >>   stdout.${type} 2>>errfile
        export err=$?; err_chk

        #
        #  stdout.${type} is needed by radmon_ck_stdout.sh 
        #  NCO requirement is executable output goes to jlogfile, so 
        #  cat it there now:
        cat stdout.${type} >> ${pgmout}

        if [[ $err -ne 0 ]]; then
            fail=`expr $fail + 1`
        fi

#-------------------------------------------------------------------
#  move data, control, and stdout files to $TANKverf_rad and compress
#-------------------------------------------------------------------

         if [[ -s ${data_file} ]]; then
            mv ${data_file} ${time_file}
            ${COMPRESS} -f  ${time_file}
            mv ${time_file}* $TANKverf_rad/.
         fi

         if [[ -s ${ctl_file} ]]; then
            $NCP ${ctl_file} ${time_ctl}
            ${COMPRESS} -f   ${time_ctl}
            $NCP ${time_ctl}*  ${TANKverf_rad}/.
         fi

      done
   done
   if [[ $fail -eq $ctr || $fail -gt $ctr  ]]; then
      echo "fail, ctr = $fail, $ctr"
      err=10
   fi

fi



####################################################################
#-------------------------------------------------------------------
#  Begin error analysis and reporting
#-------------------------------------------------------------------
####################################################################

if [[ $DO_DIAG_RPT -eq 1 ]]; then

#  build the disclaimer and region files 

   cat << EOF > ${disclaimer}


*********************** WARNING ***************************
THIS IS AN AUTOMATED EMAIL.  REPLIES TO SENDER WILL NOT BE
RECEIVED.  PLEASE DIRECT REPLIES TO edward.safford@noaa.gov
*********************** WARNING ***************************
EOF

   cat << EOF > ${region}
  Region Definitions:

    1  Global              (90S-90N, 0-360E)
EOF


#-------------------------------------------------------------------
#  Check stdout file for any reported problem(s) reading the 
#  diagnostic file by calling ck_stdout.sh
#
   ${USHradmon}/radmon_ck_stdout.sh  ${diag}

   if [[ -s ${diag} ]]; then
      cat << EOF > ${diag_hdr}

Problem Reading Diagnostic File
   
     $PDATE

  Problems were encountered reading the diagnostic file for
  the following sources:

EOF

      cat ${diag_hdr} >> ${diag_report}
      cat ${diag} >> ${diag_report}
      if [[ $USE_MAIL -eq 1 ]]; then
         cat ${disclaimer} >> ${diag_report}
      else
         echo End Problem Reading Diagnostic File >> ${diag_report}
         echo >> ${diag_report}
      fi
      rm ${diag} ${diag_hdr}
   fi 

#-------------------------------------------------------------------
#  mail error notifications or dump to log file

   if [[ -s ${diag_report} ]]; then
      lines=`wc -l <${diag_report}`
      if [[ $lines -gt 1 ]]; then

         if [[ $USE_MAIL -eq 1 ]]; then
            if [[ $MAIL_CC == "" ]]; then
               /bin/mail -v -s diagnostic_error_report ${MAIL_TO}< ${diag_report}
            else
               /bin/mail -v -s diagnostic_error_report -c "${MAIL_CC}" ${MAIL_TO}< ${diag_report}
            fi
         else
            
            cat ${diag_report}
         fi
      fi
   fi

fi


#-------------------------------------------------------------------
#  Assemble the bad penalty/channel report

if [[ $DO_DATA_RPT -eq 1 ]]; then

   #----------------------------------------------------------------
   #  Identify bad_pen and bad_chan files for this cycle and 
   #   previous cycle

   bad_pen=bad_pen.${PDATE}
   bad_chan=bad_chan.${PDATE}

   qdate=`$NDATE -${CYCLE_INTERVAL} $PDATE`
   pday=`echo $qdate | cut -c1-8`
   
   prev_bad_pen=bad_pen.${qdate}
   prev_bad_chan=bad_chan.${qdate}

   if [[ $CYCLE == "00" ]]; then
      prev_bad_pen=${TANKverf_radM1}/${prev_bad_pen}
      prev_bad_chan=${TANKverf_radM1}/${prev_bad_chan}
   else
      prev_bad_pen=${TANKverf_rad}/${prev_bad_pen}
      prev_bad_chan=${TANKverf_rad}/${prev_bad_chan}
   fi

   do_pen=0
   do_chan=0
   if [[ -s $bad_pen && -s $prev_bad_pen ]]; then
      do_pen=1
   fi
   if [[ -s $bad_chan && -s $prev_bad_chan ]]; then
      do_chan=1
   fi

#--------------------------------------------------------------------
#  Remove extra spaces in new bad_pen file
#
   gawk '{$1=$1}1' $bad_pen > tmp.bad_pen
   mv -f tmp.bad_pen $bad_pen


   if [[ $do_pen -eq 1 || $do_chan -eq 1 ]]; then

      if [[ $do_pen -eq 1 ]]; then   

         $NCP ${TANKverf_radM1}/${prev_bad_pen} ./
         ${radmon_err_rpt} ${prev_bad_pen} ${bad_pen} pen ${qdate} \
		${PDATE} ${diag_report} ${pen_err}
      fi

      if [[ $do_chan -eq 1 ]]; then   

         $NCP ${TANKverf_radM1}/${prev_bad_chan} ./
         ${radmon_err_rpt} ${prev_bad_chan} ${bad_chan} chan ${qdate} \
		${PDATE} ${diag_report} ${chan_err}
      fi

#-------------------------------------------------------------------
#  put together the unified error report with any obs, chan, and
#  penalty problems and mail it

   if [[ -s ${obs_err} || -s ${pen_err} || -s ${chan_err} ]]; then

      echo DOING ERROR REPORTING

      echo "Begin Cycle Data Integrity Report" > $report

      cat << EOF >> $report
Cycle Data Integrity Report 
  $PDATE

EOF

      cat ${region} >> $report

      if [[ -s ${chan_err} ]]; then

         echo OUTPUTING CHAN_ERR

         cat << EOF > ${chan_hdr}
         
  The following channels report 0 observational counts over the past two cycles:
   
  Satellite/Instrument    Channel
  ====================    =======

EOF

         cat ${chan_hdr} >> $report
         cat ${chan_err} >> $report
 
      fi

      if [[ -s ${pen_err} ]]; then

         cat << EOF > ${pen_hdr}


  Penalty values outside of the established normal range were found
  for these sensor/channel/regions in the past two cycles: 

  Questionable Penalty Values 
  ============ ======= ======      Cycle                 Penalty          Bound
                                   -----                 -------          -----
EOF
         cat ${pen_hdr} >> $report
         cat ${pen_err} >> $report
         rm -f ${pen_hdr} 
         rm -f ${pen_err}
      fi 

      if [[ $USE_MAIL -eq 1 ]]; then
         cat ${disclaimer} >> $report
      else
         echo End Cycle Data Integrity Report  >> $report
         echo  >> $report
      fi
   fi

#-------------------------------------------------------------------
#  mail error notifications or dump to log file
#
   if [[ -s ${report} ]]; then
      lines=`wc -l <${report}`
      if [[ $lines -gt 2 ]]; then
         if [[ $USE_MAIL -eq 1 ]]; then
            if [[ $MAIL_CC == "" ]]; then
               /bin/mail -v -s cycle_report ${MAIL_TO}< ${report}
            else
               /bin/mail -v -s cycle_report -c "${MAIL_CC}" ${MAIL_TO}< ${report}
            fi 
         else
            cat ${report}
         fi
      fi
  fi

  fi

#-------------------------------------------------------------------
#  copy new bad_pen and bad_chan files to $TANKverf_rad
   
   if [[ -s ${bad_chan} ]]; then
      $NCP ${bad_chan} ${TANKverf_rad}/.
   fi

   if [[ -s ${bad_pen} ]]; then
      $NCP ${bad_pen} ${TANKverf_rad}/.
   fi

fi

for type in ${SATYPE}; do
   rm -f stdout.${type}
done

################################################################################
#-------------------------------------------------------------------
#  end error reporting section
#-------------------------------------------------------------------
################################################################################

################################################################################
#  Post processing
if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXITING $0 error code ${err} >&2
fi

msg="${scr} HAS ENDED"
postmsg "$jlogfile" "$msg"

exit ${err}
