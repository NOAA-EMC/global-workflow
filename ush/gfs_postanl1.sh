#!/bin/ksh
#####################################################################
echo "------------------------------------------------"
echo "GFS Analysis postprocessing"
echo "------------------------------------------------"
echo "History: JAN 1997  - First implementation of this new script."
echo "         Apr 2003  - Remove Unneeded FAX processing."
echo "         Apr 2004  - Rename AVN to GFS              "
echo "         Sept 2005 - Remove SND2FORN logic              "
echo "         Sept 2005 - Remove generation of the 250MB plot and analysis 6-bit format"
echo "                     Replace with GEMPAK verison             "
echo "                     Converted fax graphics to T4 format and stopped writing to the"
echo "                     stat file."
#####################################################################

cd $DATA

##########################################
# 1) Execute UPAPREP 
# 2) Execute PLOTVPAP
# 3) Execute TRPANL 
# 7) Execute WNDALFTF - This utility consists of 5 steps.
# 8) Execute WNDALFTV - generate 4 AFOS charts of winds aloft.
# 9) Execute GETFLDS to retrieve selected fields from the analysis. 
########################################

########################################
set -x
msg="HAS BEGUN!"
postmsg "$jlogfile" "$msg"
########################################
set +x
echo " "
echo "###################################################"
echo " Execute the UPAPREP for mercator tropical analysis"
echo "###################################################"
echo " "
set -x

cp ${COMIN}/gfs.$cycle.prepbufr gfs.$cycle.prepbufr
err=$?
if test "$err" -ne 0
then
  msg="File not yet available in com"
  postmsg "$jlogfile" "$msg"
  err_exit
fi

export pgm=upaprep
. prep_step

echo ${PDY}${cyc} > upaprep.input

export FORT9=${UTILgfs}/parm/graph_upaprep.ft9.mrcontrl
export FORT10=gfs.$cycle.prepbufr
export FORT22=${UTILgfs}/parm/graph_upaprep.ft22.aircft
export FORT23=${UTILgfs}/parm/graph_upaprep.ft23.satwnd
export FORT24=${UTILgfs}/parm/graph_upaprep.ft24.tiros
export FORT55=gfs.${PDY}${cyc}.upaprep
export FORT56=gfs.${PDY}${cyc}.aircft
export FORT57=gfs.${PDY}${cyc}.satwnd
export FORT58=gfs.${PDY}${cyc}.satell
export FORT75=LOGMSG

startmsg
# $UPAPREP < upaprep.input >> $pgmout 2> errfile
${UTILgfs}/exec/upaprep < upaprep.input >> $pgmout 2> errfile
export err=$?;err_chk
  
set +x
echo " "
echo "###################################################"
echo " Execute PLOTVPAP "
echo "###################################################"
echo " "
set -x

export pgm=plotvpap
. prep_step

# cp $PARMshared/graph_plotvpap.gfs.anl .
cp ${UTILgfs}/parm/graph_plotvpap.gfs.anl .
# cp $PARMshared/graph_plotvpap.plotmlty.ft26 .
cp ${UTILgfs}/parm/graph_plotvpap.plotmlty.ft26 .

export FORT15="graph_plotvpap.gfs.anl"
export FORT26="graph_plotvpap.plotmlty.ft26"
export FORT41="gfs.${PDY}${cyc}.upaprep"
export FORT42="gfs.${PDY}${cyc}.aircft"
export FORT43="gfs.${PDY}${cyc}.satwnd"
export FORT44="gfs.${PDY}${cyc}.satell"
export FORT55="OLDlabel55"
export FORT60="label60"
export FORT61="label61"
export FORT62="label62"
export FORT63="label63"
export FORT75="LOGMSG"
  
startmsg
# $PLOTVPAP
${UTILgfs}/exec/plotvpap
export err=$?;err_chk

set +x
echo " "
echo "###################################################"
echo " Execute TRPANL "
echo "###################################################"
echo " "
set -x

FAXOUT=trpismer.${cyc}

cp ${COMIN}/gfs.$cycle.pgrb2.1p00.anl .
err1=$?
$CNVGRIB -g21 gfs.$cycle.pgrb2.1p00.anl gfs.$cycle.pgrbanl 
err2=$?
$GRBINDEX gfs.$cycle.pgrbanl gfs.$cycle.pgrbianl
err3=$?

tot=`expr $err1 + $err2`
if test "$tot" -ne 0
then
  msg="File not yet available in com"
  postmsg "$jlogfile" "$msg"
  err_exit
fi

# cp ${FIXshared}/graph_gphbg/mr4002.pur mr4002.pur
cp ${UTILgfs}/fix/graph_gphbg/mr4002.pur mr4002.pur
# cp ${PARMshared}/graph_trpanl.ft8.gfs_${cycle}.anl .
cp ${UTILgfs}/parm/graph_trpanl.ft8.gfs_${cycle}.anl .
# cp ${FIXshared}/raph_awpseed .
cp ${UTILgfs}/fix/graph_awpseed .

export pgm=trpanl
. prep_step
export FORT8="graph_trpanl.ft8.gfs_${cycle}.anl"
export FORT12="gfs.$cycle.pgrbanl"
export FORT22="gfs.$cycle.pgrbianl"
export FORT48="graph_awpseed"
export FORT52="x6b"
export FORT54="OLDlabel55"
export FORT55="f55"
export FORT60="f60"
export FORT61="f61"
export FORT62="f62"
export FORT63="f63"
export FORT71="ras"
export FORT72="rs2"
export FORT81="trpismer.${cyc}"

startmsg
# $TRPANL  >> $pgmout < ${PARMshared}/graph_trpanl.ft5.gfs_${cycle}.anl 2>errfile
${UTILgfs}/exec/trpanl >> $pgmout < ${UTILgfs}/parm/graph_trpanl.ft5.gfs_${cycle}.anl 2>errfile
#export err=$?; err_chk 

if [ ${cyc} -eq 00 -o ${cyc} -eq 12 ];
then
 
 # JY- this is used for part of the COMOUTwmo file name in mk_graphics.sh
 jobn=`echo $job|sed 's/[jpt]gfs/gfs/'`

 for KEYW in TRP850_g TRP850V TRP700A TRP700_g TRP500A TRP500_g TRP250A TRP250_g
 do

# grep $KEYW ${FIXshared}/identifyfax.tbl | read Keyword sub00 sub06 sub12 sub18 gif toc prt lprt name
 grep $KEYW ${UTILgfs}/fix/identifyfax.tbl | read Keyword sub00 sub06 sub12 sub18 gif toc prt lprt name

 if [ ${cyc} = '00' ]; then submn=$sub00; fi
 if [ ${cyc} = '12' ]; then submn=$sub12; fi

 export FAXOUT submn name Keyword gif toc prt jobn lprt
# mk_graphics.sh
 ${UTILgfs}/ush/mk_graphics.sh

 done
fi

if [ ${cyc} -eq 06 -o ${cyc} -eq 18 ];
then

 # JY jobn=$job
 jobn=`echo $job|sed 's/[jpt]gfs/gfs/'`

 for KEYW in  TRP250A TRP250_g
 do

#  grep $KEYW ${FIXshared}/identifyfax.tbl | read Keyword sub00 sub06 sub12 sub18 gif toc prt lprt name
 grep $KEYW ${UTILgfs}/fix/identifyfax.tbl | read Keyword sub00 sub06 sub12 sub18 gif toc prt lprt name

 if [ ${cyc} = '06' ]; then submn=$sub06; fi
 if [ ${cyc} = '18' ]; then submn=$sub18; fi

 export FAXOUT submn name Keyword gif toc prt jobn lprt
#  mk_graphics.sh
 ${UTILgfs}/ush/mk_graphics.sh

 done
fi
 
if test ${cycle} = 't00z' -o ${cycle} = 't12z'
then
  set +x
  echo " "
  echo "###################################################"
  echo " Execute WNDALFTF                                  "
  echo "###################################################"
  echo " "
  set -x

#  mkwindalftf.sh
  ${UTILgfs}/ush/mkwindalftf.sh

  set +x
  echo " "
  echo "###################################################"
  echo " Execute WNDALFTV (first get prepbufr bufr data)     "
  echo "###################################################"
  echo " "
  echo "###################################################"
  echo "#  Extract significant height observations         "
  echo "###################################################"
  set -x

  export pgm=rsondplt
  . prep_step
  
  export FORT10="gfs.${cycle}.prepbufr"
  export FORT51="f51Z"
  
  startmsg
#  ${RSONDPLT} < ${PARMshared}/graph_rsondplt.ft05.Z >> $pgmout 2>errfile
  ${UTILgfs}/exec/rsondplt < ${UTILgfs}/parm/graph_rsondplt.ft05.Z >> $pgmout 2>errfile
  export err=$?;err_chk

  set +x
  echo " "
  echo "###################################################"
  echo "#  Extract significant pressure observations       "
  echo "###################################################"
  set -x
  
  export pgm=rsondplt
  . prep_step

  export FORT10="gfs.${cycle}.prepbufr"
  export FORT51="f51P"

  startmsg
#  ${RSONDPLT} < ${PARMshared}/graph_rsondplt.ft05.P >>$pgmout 2>errfile
  ${UTILgfs}/exec/rsondplt < ${UTILgfs}/parm/graph_rsondplt.ft05.P >>$pgmout 2>errfile
  export err=$?;err_chk

  set +x
  echo " "
  echo "###################################################"
  echo "# cat the observation files together               "
  echo "###################################################"
  set -x

  cat f51Z f51P > formatted.soundings


fi

exit
