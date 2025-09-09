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
#     DO_DATA_RPT	switch to build the data report
#                       defaults to 1 (on)
#     RADMON_SUFFIX	data source suffix
#                       defauls to opr
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


if [[ "$VERBOSE" = "YES" ]]; then
   set -ax
fi

# Directories
FIXgdas=${FIXgdas:-$(pwd)}
EXECradmon=${EXECradmon:-$(pwd)}
TANKverf_rad=${TANKverf_rad:-$(pwd)}

# File names
#pgmout=${pgmout:-${jlogfile}}
#touch $pgmout

radmon_err_rpt=${radmon_err_rpt:-${USHradmon}/radmon_err_rpt.sh}
base_file=${base_file:-$FIXgdas/gdas_radmon_base.tar}
report=report.txt
disclaimer=disclaimer.txt

diag_report=diag_report.txt
diag_hdr=diag_hdr.txt
diag=diag.txt

obs_err=obs_err.txt
obs_hdr=obs_hdr.txt
pen_err=pen_err.txt
pen_hdr=pen_hdr.txt

chan_err=chan_err.txt
chan_hdr=chan_hdr.txt
count_hdr=count_hdr.txt
count_err=count_err.txt

netcdf_boolean=".false."
if [[ $RADMON_NETCDF -eq 1 ]]; then
   netcdf_boolean=".true."
fi

DO_DATA_RPT=${DO_DATA_RPT:-1}
RADMON_SUFFIX=${RADMON_SUFFIX:-opr}
RAD_AREA=${RAD_AREA:-glb}
REGIONAL_RR=${REGIONAL_RR:-0}
rgnHH=${rgnHH:-}
rgnTM=${rgnTM:-}
SATYPE=${SATYPE:-}
MAIL_TO=${MAIL_TO:-}
MAIL_CC=${MAIL_CC:-}
VERBOSE=${VERBOSE:-NO}
LITTLE_ENDIAN=${LITTLE_ENDIAN:-0}
USE_MAIL=${USE_MAIL:-1}

time_exec=radmon_time.x
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
  gesanl='${dtype}',
  little_endian=${LITTLE_ENDIAN},
  rad_area='${RAD_AREA}',
  netcdf=${netcdf_boolean},
 /
EOF

         ./${time_exec} < input >>   stdout.${type} 2>>errfile
         
         if [[ $err -ne 0 ]]; then
            fail=`expr $fail + 1`
         fi

#-------------------------------------------------------------------
#  move data, control, and stdout files to $TANKverf_rad and compress
#-------------------------------------------------------------------
         cat stdout.${type} >> stdout.time

         if [[ -s ${time_file} ]]; then
            ${COMPRESS} ${time_file}
         fi

         if [[ -s ${time_ctl} ]]; then
            ${COMPRESS} ${time_ctl}
         fi
         
      done
   done


   ${USHradmon}/rstprod.sh

   tar_file=radmon_time.tar
   tar -cf $tar_file time*.ieee_d* time*.ctl*
   ${COMPRESS} ${tar_file}
   mv $tar_file.${Z} ${TANKverf_rad}/.

   if [[ $RAD_AREA = "rgn" ]]; then
      cwd=`pwd`
      cd ${TANKverf_rad}
      tar -xf ${tar_file}.${Z}
      rm ${tar_file}.${Z}
      cd ${cwd}
   fi

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

if [[ $DO_DATA_RPT -eq 1 ]]; then

#---------------------------
#  build report disclaimer 
#
   cat << EOF > ${disclaimer}


*********************** WARNING ***************************
THIS IS AN AUTOMATED EMAIL.  REPLIES TO SENDER WILL NOT BE
RECEIVED.  PLEASE DIRECT REPLIES TO edward.safford@noaa.gov
*********************** WARNING ***************************
EOF


#-------------------------------------------------------------------
#  Check for missing diag files 
#
   tmp_satype="./tmp_satype.txt"
   echo ${SATYPE} > ${tmp_satype}
   ${USHradmon}/radmon_diag_ck.sh  --rad ${radstat} --sat ${tmp_satype} --out ${diag}

   if [[ -s ${diag} ]]; then
      cat << EOF > ${diag_hdr}

  Problem Reading Diagnostic File
   

  Problems were encountered reading the diagnostic file for
  the following sources:

EOF

      cat ${diag_hdr} >> ${diag_report}
      cat ${diag} >> ${diag_report}

      echo >> ${diag_report}

      rm ${diag_hdr}
   fi 

#-------------------------------------------------------------------
#  move warning notification to TANKverf
#
   if [[ -s ${diag} ]]; then
      lines=`wc -l <${diag}`
      echo "lines in diag = $lines"   
   
      if [[ $lines -gt 0 ]]; then
         cat ${diag_report}
         cp ${diag}  ${TANKverf_rad}/bad_diag.${PDATE}
      else
         rm ${diag_report}
      fi
   fi



   #----------------------------------------------------------------
   #  Identify bad_pen and bad_chan files for this cycle and 
   #   previous cycle

   bad_pen=bad_pen.${PDATE}
   bad_chan=bad_chan.${PDATE}
   low_count=low_count.${PDATE}

   qdate=`$NDATE -${CYCLE_INTERVAL} $PDATE`
   pday=`echo $qdate | cut -c1-8`
   
   prev_bad_pen=bad_pen.${qdate}
   prev_bad_chan=bad_chan.${qdate}
   prev_low_count=low_count.${qdate}

   prev_bad_pen=${TANKverf_radM1}/${prev_bad_pen}
   prev_bad_chan=${TANKverf_radM1}/${prev_bad_chan}
   prev_low_count=${TANKverf_radM1}/${prev_low_count}

   if [[ -s $bad_pen ]]; then
      echo "pad_pen        = $bad_pen"
   fi
   if [[ -s $prev_bad_pen ]]; then
      echo "prev_pad_pen   = $prev_bad_pen"
   fi

   if [[ -s $bad_chan ]]; then
      echo "bad_chan       = $bad_chan"
   fi
   if [[ -s $prev_bad_chan ]]; then
      echo "prev_bad_chan  = $prev_bad_chan"
   fi
   if [[ -s $low_count ]]; then
      echo "low_count = $low_count"
   fi 
   if [[ -s $prev_low_count ]]; then
      echo "prev_low_count = $prev_low_count"
   fi 

   do_pen=0
   do_chan=0
   do_cnt=0

   if [[ -s $bad_pen && -s $prev_bad_pen ]]; then
      do_pen=1
   fi

   if [[ -s $low_count && -s $prev_low_count ]]; then
      do_cnt=1
   fi

   #--------------------------------------------------------------------  
   # avoid doing the bad_chan report for REGIONAL_RR sources -- because
   # they run hourly they often have 0 count channels for off-hour runs.
   #
   if [[ -s $bad_chan && -s $prev_bad_chan && REGIONAL_RR -eq 0 ]]; then
      do_chan=1
   fi

   #--------------------------------------------------------------------
   #  Remove extra spaces in new bad_pen & low_count files
   #
   gawk '{$1=$1}1' $bad_pen > tmp.bad_pen
   mv -f tmp.bad_pen $bad_pen

   gawk '{$1=$1}1' $low_count > tmp.low_count
   mv -f tmp.low_count $low_count

   echo " do_pen, do_chan, do_cnt = $do_pen, $do_chan, $do_cnt"
   echo " diag_report = $diag_report "
   if [[ $do_pen -eq 1 || $do_chan -eq 1 || $do_cnt -eq 1 || -s ${diag_report} ]]; then

      if [[ $do_pen -eq 1 ]]; then   

         echo "calling radmon_err_rpt for pen"
         ${radmon_err_rpt} ${prev_bad_pen} ${bad_pen} pen ${qdate} \
		${PDATE} ${diag_report} ${pen_err}
      fi

      if [[ $do_chan -eq 1 ]]; then   

         echo "calling radmon_err_rpt for chan"
         ${radmon_err_rpt} ${prev_bad_chan} ${bad_chan} chan ${qdate} \
		${PDATE} ${diag_report} ${chan_err}
      fi

      if [[ $do_cnt -eq 1 ]]; then   

         echo "calling radmon_err_rpt for cnt"
         ${radmon_err_rpt} ${prev_low_count} ${low_count} cnt ${qdate} \
		${PDATE} ${diag_report} ${count_err}
      fi

      #-------------------------------------------------------------------
      #  put together the unified error report with any obs, chan, and
      #  penalty problems and mail it

      if [[ -s ${obs_err} || -s ${pen_err} || -s ${chan_err} || -s ${count_err} || -s ${diag_report} ]]; then

         echo DOING ERROR REPORTING


         cat << EOF > $report
Radiance Monitor warning report
 
  Net:   ${RADMON_SUFFIX}
  Run:   ${RUN}
  Cycle: $PDATE

EOF

         if [[ -s ${diag_report} ]]; then
            echo OUTPUTING DIAG_REPORT
            cat ${diag_report} >> $report
         fi

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

         if [[ -s ${count_err} ]]; then

            cat << EOF > ${count_hdr}


         
  The following channels report abnormally low observational counts in the latest 2 cycles:
   
Satellite/Instrument              Obs Count          Avg Count
====================              =========          =========

EOF
              
            cat ${count_hdr} >> $report
            cat ${count_err} >> $report
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

         echo  >> $report
         cat ${disclaimer} >> $report
         echo  >> $report
      fi

      #-------------------------------------------------------------------
      #  dump report to log file
      #
      if [[ -s ${report} ]]; then
         lines=`wc -l <${report}`
         if [[ $lines -gt 2 ]]; then
            cat ${report}

            $NCP ${report} ${TANKverf_rad}/warning.${PDATE}
         fi
      fi


   fi

   #-------------------------------------------------------------------
   #  copy new bad_pen, bad_chan, and low_count files to $TANKverf_rad
   #   
   if [[ -s ${bad_chan} ]]; then
      mv ${bad_chan} ${TANKverf_rad}/.
   fi

   if [[ -s ${bad_pen} ]]; then
      mv ${bad_pen} ${TANKverf_rad}/.
   fi

   if [[ -s ${low_count} ]]; then
      mv ${low_count} ${TANKverf_rad}/.
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

exit ${err}
