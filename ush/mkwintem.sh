
#  UTILITY SCRIPT NAME :  mkwintem.sh
#               AUTHOR :  Hua-Lu Pan
#         DATE WRITTEN :  02/03/97
#
#  Abstract:  This utility script produces the wind temp
#             bulletins, from the GFS forecast fields.
#
#     Input:  none
#
echo "History: February 1997 - First implementation of this utility script"
echo "History: July     2014 - Modify to use GFS 1.0 deg GRIB2"
#

set -xa

job_name=`echo $job|sed 's/[jpt]gfs/gfs/'`

export pgm=wintemv
. prep_step


for fhr in 06 12 18 24
do

  cp $COMIN/gfs.${cycle}.pgrb2.1p00.f0${fhr}   .
  $CNVGRIB -g21 gfs.${cycle}.pgrb2.1p00.f0${fhr} pgrbf${fhr}
  $GRBINDEX  pgrbf${fhr}   pgrbif${fhr}

done

cp $PARMwmo/bulls_wkeysb wkeysb
export FORT11="pgrbf06"
export FORT12="pgrbf12"
export FORT13="pgrbf18"
export FORT14="pgrbf24"
export FORT31="pgrbif06"
export FORT32="pgrbif12"
export FORT33="pgrbif18"
export FORT34="pgrbif24"
export FORT51="wintemv.bul"
$EXECgfs/wintemv < wkeysb >> $pgmout 2>errfile
export err=$?; err_chk

####################################################################

  if test $SENDCOM = 'YES'
  then
    cp wintemv.bul $COMOUTwmo/wintemv.tran.$job_name
    export err=$?
    if [[ $err -ne 0 ]] ; then
       echo " File wintemv.bul does not exist."
       exit $err
    fi

# Add an entry to the OSO status file.

    if test $SENDDBN = 'YES'
    then
       ${UTILgfs}/ush/make_ntc_bull.pl WMONV NONE KWNO NONE   \
               wintemv.bul   $COMOUTwmo/wintemv.tran.$job_name    
    fi
  fi

###################################################################
