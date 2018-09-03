set +x
#################################################
#----------------------------------------------------------------
# Name    : mk_graphics.sh
# Author  : Steve Lilly
# Purpose : This script first extracts the pure raster graphich by using
#           fxcompoz, and then uses ImageMagick to converts the raster format
#           to either GIF format or NTC (T4 format with a WMO HEADR).
# History :
#           OCT 2005  - first implementation
#----------------------------------------------------------------

  set -x    
if [ "$gif" = 'YES' ]
then 
  export INFILE=$FAXOUT
  export subnn=$submn
  export PDYHH=$PDY${cyc}
  export outname=${name}.${jobn}.gif
  export KEYW=$Keyword

#  convert6bit.sh
   $UTILgfs/ush/convert6bit.sh

  if test "$SENDCOM" = 'YES'
  then
    cp ${outname} ${COMOUT}
      if test "$SENDDBN" = 'YES'
      then
        $DBNROOT/bin/dbn_alert MODEL NCDCGIF ${job} ${COMOUT}/$outname
      fi
  fi
fi

if [ "$prt" = 'YES' -a "$PRINT_FAX_CHARTS" = 'YES' ]
then
  export INFILE=$FAXOUT
  export subnn=$submn
  export PDYHH=$PDY${cyc}
  export outname=pcl.${submn}_${name}.${jobn}
  export KEYW=$Keyword

  convert6bit.sh

  if test "$SENDCOM" = 'YES'
  then
    cp ${outname} ${COMOUTwmo}
      if test "$SENDDBN" = 'YES'
      then
        lpr -P ${lprt} ${COMOUTwmo}/${outname}
      fi
  fi
fi

if [ "$toc" = 'YES' ]
then
  export INFILE=$FAXOUT
  export subnn=$submn
  export PDYHH=$PDY${cyc}
  export OUTPATH=${name}.${jobn}.ntc
  export outname=${name}.${jobn}.fax
  export KEYW=$Keyword

#  convert6bit.sh
  $UTILgfs/ush/convert6bit.sh

  if test "$SENDCOM" = 'YES'
  then
    cp ${OUTPATH} ${COMOUTwmo}
      if test "$SENDDBN" = 'YES'
      then
        $DBNROOT/bin/dbn_alert GRIB_LOW HRLY FAX ${COMOUTwmo}/$OUTPATH
      fi
  fi
fi

if [ "$toc" = 'WAF' ]
then
  export INFILE=$FAXOUT
  export subnn=$submn
  export PDYHH=$PDY${cyc}
  export OUTPATH=${name}.${jobn}.ntc
  export outname=${name}.${jobn}.fax
  export KEYW=$Keyword

#  convert6bit.sh
  $UTILgfs/ush/convert6bit.sh

  if test "$SENDCOM" = 'YES'
  then
    cp ${OUTPATH} ${COMOUTwmo}
      if test "$SENDDBN" = 'YES'
      then
        $DBNROOT/bin/dbn_alert GRIB_LOW HRLY FAX ${COMOUTwmo}/$OUTPATH
      fi
  fi

fi
