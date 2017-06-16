#!/bin/ksh
################################################################
# Script Name:          gfs_sndp.sh
# Script Description:   Format GFS BUFR sounding files for AWIPS
# Script History Log:
#   1) 2004-09-10       Steve Gilbert       First Implementation
################################################################

set -x


  #  Create "collectives" consisting of groupings of the soundings
  #  into files designated by geographical region.   Each input
  #  file gfs_collective*.list (1-9) contains the list of stations to
  #  put in a particular collective output file. 

#  cp $FIXGLOBAL/gfs_collective*.list $DATA/. 
  cp $FIXbufr/fix_snd/gfs_collective*.list $DATA/. 
  CCCC=KWBC
  let "m=1"
  while [ $m -lt 10 ]
  do
    file_list=gfs_collective$m.list

    if [ $m -le 2 ]
    then 
      WMOHEAD=JUSA4$m
    elif [ $m -le 6 ]
    then 
      WMOHEAD=JUSB4$m
    else 
      WMOHEAD=JUSX4$m
    fi

    for stn in `cat $file_list`
    do
       cp ${COMIN}/bufr.${cycle}/bufr3.$stn.$PDY$cyc $DATA/bufrin
       export pgm=tocsbufr
       #. prep_step
       export FORT11=$DATA/bufrin
       export FORT51=$DATA/bufrout
       # JY - Turn off the startmsg to reduce the update on jlogfile in this loop
       # startmsg
#       $utilexec/tocsbufr << EOF
#       $HOMEutil/sorc/tocsbufr.fd/tocsbufr << EOF
#       /nwprod/util/exec/tocsbufr << EOF
        $TOCSBUFR << EOF
 &INPUT
  BULHED="$WMOHEAD",KWBX="$CCCC",
  NCEP2STD=.TRUE.,
  SEPARATE=.TRUE.,
  MAXFILESIZE=200000
 /
EOF
       # JY export err=$?;err_chk
       export err=$?
       if [ $err -ne 0 ]
       then
          echo "ERROR in $pgm"
          err_chk
       fi

       cat $DATA/bufrout >> gfs_collective$m.fil
       rm $DATA/bufrin
       rm $DATA/bufrout
    done

    if test $SENDCOM = 'YES'
    then 
      cp gfs_collective$m.fil ${COMOUT}/bufr.${cycle}/.
      if [ $SENDDBN = 'YES' ] ; then
         cp gfs_collective$m.fil $pcom/gfs_collective$m.$job
         $DBNROOT/bin/dbn_alert NTC_LOW BUFR $job $pcom/gfs_collective$m.$job
      fi
    fi

    let "m=m+1"

  done

exit
