#!/bin/sh
##############################################################
# Add the NCDC GIF processing to the end of the gempak_gif job
# There is no timing issue with the NCDC GIF, so it is
# okay to just add it here. If timing becomes a problem
# in the future, we should move it above somewhere else.
##############################################################
export PS4='exgempakgif_ncdc:$SECONDS + '
set -xa

cd $DATA
msg="The NCDC GIF processing has begun"
postmsg "$jlogfile" "$msg"

export NTS=$USHgempak/restore

if [ $MODEL = GDAS ]
then
    case $MODEL in
      GDAS) fcsthrs="000";;
    esac

    export fhr
    for fhr in $fcsthrs
    do
        icnt=1
        maxtries=180
        while [ $icnt -lt 1000 ]
        do
          if [ -r ${COMIN}/${RUN}_${PDY}${cyc}f${fhr} ] ; then
            break
          else
            sleep 20
            let "icnt=icnt+1"
          fi
          if [ $icnt -ge $maxtries ]
          then
            msg="ABORTING after 1 hour of waiting for F$fhr to end."
            err_exit $msg
          fi
        done

       cp ${COMIN}/${RUN}_${PDY}${cyc}f${fhr} gem_grids${fhr}.gem
       export err=$?
       if [[ $err -ne 0 ]] ; then
          echo " File: ${COMIN}/${RUN}_${PDY}${cyc}f${fhr} does not exist."
          exit $err
       fi
     
       if [ $cyc -eq 00 -o $cyc -eq 12 ]
       then
          $USHgempak/gempak_${RUN}_f${fhr}_gif.sh
          if [ ! -f $USHgempak/gempak_${RUN}_f${fhr}_gif.sh ] ; then
             echo "WARNING: $USHgempak/gempak_${RUN}_f${fhr}_gif.sh FILE is missing"
             msg=" $USHgempak/gempak_${RUN}_f${fhr}_gif.sh file is missing "
             postmsg "jlogfile" "$msg"
          fi
       fi

    done
fi

exit
