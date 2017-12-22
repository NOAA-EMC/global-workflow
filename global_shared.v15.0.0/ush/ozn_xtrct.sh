#!/bin/ksh
#------------------------------------------------------------------
#  ozn_xtrct.sh
#
#  This script performs the data extraction from the cnvstat's
#  diagnostic files.  The resulting data (*.ieee_d) files, GrADS
#  control files and stdout files will be moved to the 
#  $TANKverf_ozn.  
#
#  Calling scripts must define: 
#	$TANKverf_ozn
#	$HOMEoznmon
#	$PDATE
#
#  Return values are 
#	0 = normal 
#	2 = unable to generate SATYPE list; may indicate no diag
#		files found in oznstat file
#------------------------------------------------------------------

set -ax
echo "start ozn_xtrct.sh"

msg="ozn_xtrct.sh HAS STARTED"
postmsg "$jlogfile" "$msg"

iret=0
export NCP=${NCP:-/bin/cp}
nregion=${nregion:-6}

#------------------------------------------------------------------
# ptype here is the processing type which is intended to be "ges" 
# or "anl".  Default is "ges".  
#
# If this needs to change to include __both__ then the extraction 
# executables will need to be modified to handle the ges && anl
# inputs in the diag file names.
#
ozn_ptype=${ozn_ptype:-"ges"}


#---------------------------------------------------------------------------
#  Build SATYPE list from files found in oznstat file.
#
#  NOTE:  at some point we'll want to make this a fixed list so we can
#  detect missing instruments, allowing new sources to be added like the
#  radmon does.
#
#  An empty SATYPE list means there are no diag files to process.  That's
#  a problem, reported by an iret value of 2
#
SATYPE=`ls -l d*ges* | sed -e 's/_/ /g;s/\./ /' | gawk '{ print $11 "_" $12 }'`
echo $SATYPE

len_satype=`echo -n "$SATYPE" | wc -c`

if [[ $len_satype -lt 1 ]]; then
   iret=2 

else

   #---------------------------------------------------------------------------
   #  NOTE:  If ges && anl are to be processed then add an outer for loop on 
   #  $ozn_ptype
   #
   if [[ -e ${type}.gz ]]; then
      rm -f ${type}.gz
   fi
   if [[ -e ${type} ]]; then 
      rm -f ${type}
   fi
   for type in ${SATYPE}; do
      mv diag_${type}_${ozn_ptype}.${PDATE}.gz ${type}.gz
      gunzip ./${type}.gz
   done


   #--------------------------------------------------------------------
   #   Copy extraction programs to working directory
   #
   $NCP ${HOMEoznmon}/exec/oznmon_time.x   ./oznmon_time.x
   if [[ ! -e oznmon_time.x ]]; then
      iret=2
      exit $iret
   fi
   $NCP ${HOMEoznmon}/exec/oznmon_horiz.x  ./oznmon_horiz.x
   if [[ ! -e oznmon_horiz.x ]]; then
      iret=3
      exit $iret
   fi


   #--------------------------------------------------------------------
   #   Run programs for given time

   iyy=`echo $PDATE | cut -c1-4`
   imm=`echo $PDATE | cut -c5-6`
   idd=`echo $PDATE | cut -c7-8`
   ihh=`echo $PDATE | cut -c9-10`

   for type in ${SATYPE}; do
      rm -f input

cat << EOF > input
         &INPUT
         satname='${type}',
         iyy=${iyy},
         imm=${imm},
         idd=${idd},
         ihh=${ihh},
         idhh=-720,
         incr=6,
         nregion=${nregion},
         region(1)='global',    rlonmin(1)=-180.0,rlonmax(1)=180.0,rlatmin(1)=-90.0,rlatmax(1)= 90.0,
         region(2)='70N-90N',   rlonmin(2)=-180.0,rlonmax(2)=180.0,rlatmin(2)= 70.0,rlatmax(2)= 90.0,
         region(3)='20N-70N',   rlonmin(3)=-180.0,rlonmax(3)=180.0,rlatmin(3)= 20.0,rlatmax(3)= 70.0,
         region(4)='20S-20N',   rlonmin(4)=-180.0,rlonmax(4)=180.0,rlatmin(4)=-20.0,rlatmax(4)= 20.0,
         region(5)='20S-70S',   rlonmin(5)=-180.0,rlonmax(5)=180.0,rlatmin(5)=-70.0,rlatmax(5)=-20.0,
         region(6)='70S-90S',   rlonmin(6)=-180.0,rlonmax(6)=180.0,rlatmin(6)=-90.0,rlatmax(6)=-70.0,
      /
EOF


      msg="oznmon_time.x HAS STARTED $type"
      postmsg "$jlogfile" "$msg"

      ./oznmon_time.x < input >   stdout.time.$type

      msg="oznmon_time.x HAS ENDED $type"
      postmsg "$jlogfile" "$msg"

      if [[ ! -d ${TANKverf_ozn}/time ]]; then
         mkdir -p ${TANKverf_ozn}/time
      fi
      $NCP ${type}.ctl              ${TANKverf_ozn}/time/
      $NCP ${type}.${PDATE}.ieee_d  ${TANKverf_ozn}/time/
      $NCP stdout.time.${type}      ${TANKverf_ozn}/time/

      rm -f input

cat << EOF > input
         &INPUT
         satname='${type}',
         iyy=${iyy},
         imm=${imm},
         idd=${idd},
         ihh=${ihh},
         idhh=-18,
         incr=6,
      /
EOF

      msg="oznmon_horiz.x HAS STARTED $type"
      postmsg "$jlogfile" "$msg"

      ./oznmon_horiz.x < input >   stdout.horiz.$type

      msg="oznmon_horiz.x HAS ENDED $type"
      postmsg "$jlogfile" "$msg"

      if [[ ! -d ${TANKverf_ozn}/horiz ]]; then
         mkdir -p ${TANKverf_ozn}/horiz
      fi
      $NCP ${type}.ctl              ${TANKverf_ozn}/horiz/
      $NCP ${type}.${PDATE}.ieee_d  ${TANKverf_ozn}/horiz/
      $NCP stdout.horiz.${type}     ${TANKverf_ozn}/horiz/

   done
fi

msg="ozn_xtrct.sh HAS ENDED, iret = $iret"
postmsg "$jlogfile" "$msg"

exit $iret
