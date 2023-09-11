#! /usr/bin/env bash

##############################################################
# Add the NCDC GIF processing to the end of the gempak_gif job
# There is no timing issue with the NCDC GIF, so it is
# okay to just add it here. If timing becomes a problem
# in the future, we should move it above somewhere else.
##############################################################

source "$HOMEgfs/ush/preamble.sh"

cd $DATA

export NTS=$USHgempak/restore

if [ $MODEL = GDAS -o $MODEL = GFS ]
then
    case $MODEL in
      GDAS) fcsthrs="00";;
      GFS)  fcsthrs="00 12 24 36 48";;
    esac

    export fhr
    for fhr in $fcsthrs
    do
        icnt=1
        maxtries=180
        export GRIBFILE=${COMIN}/${RUN}_${PDY}${cyc}f0${fhr}
        while [ $icnt -lt 1000 ]
        do
          if [ -r ${COMIN}/${RUN}_${PDY}${cyc}f0${fhr} ] ; then
            sleep 5
            break
          else
            echo "The process is waiting ... ${GRIBFILE} file to proceed."
            sleep 20
            let "icnt=icnt+1"
          fi
          if [ $icnt -ge $maxtries ]
          then
            echo "ABORTING: after 1 hour of waiting for ${GRIBFILE} file at F$fhr to end."
            export err=7 ; err_chk
            exit $err
          fi
        done

       cp ${COMIN}/${RUN}_${PDY}${cyc}f0${fhr} gem_grids${fhr}.gem

#       if [ $cyc -eq 00 -o $cyc -eq 12 ]
       #then
          $USHgempak/gempak_${RUN}_f${fhr}_gif.sh
       #fi

    done
fi

####################################################################################
# echo "-----------------------------------------------------------------------------"
# echo "GFS MAG postprocessing script exmag_sigman_skew_k_gfs_gif_ncdc_skew_t.sh "
# echo "-----------------------------------------------------------------------------"
# echo "History: Mar 2012 added to processing for enhanced MAG skew_t"
# echo "2012-03-11 Mabe -- reworked script to add significant level "
# echo "  data to existing mandatory level data in a new file"
# echo "2013-04-24 Mabe -- Reworked to remove unneeded output with "
# echo "  conversion to WCOSS"
# Add ms to filename to make it different since it has both mandatory
# and significant level data      $COMOUT/${RUN}.${cycle}.msupperair
#                             $COMOUT/${RUN}.${cycle}.msupperairtble
#####################################################################################

cd $DATA

export RSHPDY=$(echo $PDY | cut -c5-)$(echo $PDY | cut -c3-4)

cp $HOMEgfs/gempak/dictionaries/sonde.land.tbl .
cp $HOMEgfs/gempak/dictionaries/metar.tbl .
sort -k 2n,2 metar.tbl > metar_stnm.tbl
cp $COMINobsproc/${model}.$cycle.adpupa.tm00.bufr_d fort.40
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " File ${model}.$cycle.adpupa.tm00.bufr_d does not exist."
   exit $err
fi
# $RDBFMSUA  >> $pgmout 2> errfile
${UTILgfs}/exec/rdbfmsua >> $pgmout 2> errfile

err=$?;export err ;err_chk

export filesize=$( ls -l rdbfmsua.out | awk '{print $5}' )

################################################################
#   only run script if rdbfmsua.out contained upper air data.
################################################################

if [ $filesize -gt 40 ]
then

if [ $SENDCOM = "YES" ]; then
       cp rdbfmsua.out $COMOUT/${RUN}.${cycle}.msupperair
       cp sonde.idsms.tbl $COMOUT/${RUN}.${cycle}.msupperairtble
       if [ $SENDDBN = "YES" ]; then
          $DBNROOT/bin/dbn_alert DATA MSUPPER_AIR $job $COMOUT/${RUN}.${cycle}.msupperair
          $DBNROOT/bin/dbn_alert DATA MSUPPER_AIRTBL $job $COMOUT/${RUN}.${cycle}.msupperairtble
       fi
fi

fi

############################################################

if [ -e "$pgmout" ] ; then
   cat $pgmout
fi


exit
