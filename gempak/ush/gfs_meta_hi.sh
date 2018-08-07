#!/bin/sh
#
# Metafile Script : gfs_meta_hi.sh
#
# Log :
# D.W.Plummer/NCEP   2/97   Add log header
# D.W.Plummer/NCEP   4/97   Changed SKIP for grid2
# B. Gordon          4/00   Converted for production on the IBM-SP
#                           and changed gdplot_nc -> gdplot2_nc
# D. Michaud         4/16   Added logic to display different title
#                           for parallel runs
# B. Gordon          7/02   Converted to run off the GFS due to demise
#                           of the MRF.
# J. Carr           11/04   Added a ? to all title/TITLE lines. Changed contur parameter to a 2.
#                           Changed the GDATTIM line to end at F240 every 6 hrs instead of out to
#                           F384 every 12 hrs. This is to account for 06 and 18 UTC runs.
# M. Klein           4/07   Fix bug in PW display.
#
set -xa
mkdir -p -m 775 $DATA/mrfhi
cd $DATA/mrfhi
cp $FIXgempak/datatype.tbl datatype.tbl

device="nc | mrfhi.meta"

PDY2=`echo $PDY | cut -c3-`

if [ "$envir" = "prod" ] ; then
   export m_title="GFS"
else
   export m_title="GFSP"
fi

export pgm=gdplot2_nc;. prep_step
startmsg

$GEMEXE/gdplot2_nc << EOF
GDFILE	= F-GFS | ${PDY2}/${cyc}00
GDATTIM	= F000-F192-06; F214-F240-12
DEVICE	= $device
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP	= 1
CLEAR	= yes
CLRBAR  = 1

restore ${USHgempak}/restore/garea_hi.nts

restore ${USHgempak}/restore/pmsl_thkn.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title MSL PRESSURE, 1000-500 MB THICKNESS|~MSLP, 1000-500 THKN!0
l
ru


restore ${USHgempak}/restore/850mb_hght_tmpc.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HGHTS, TEMPERATURE AND WIND (KTS)|~@ HGHT, TMP, WIND!0!0!0
l
ru


restore ${USHgempak}/restore/700mb_hght_relh_omeg.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HGHTS, REL HUMIDITY AND OMEGA|~@ HGHT, RH AND OMEGA!0
l
ru


restore ${USHgempak}/restore/500mb_hght_absv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HEIGHTS AND VORTICITY|~@ HGHT AND VORTICITY!0
l
ru


restore ${USHgempak}/restore/200mb_hght_wnd.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HEIGHTS, ISOTACHS AND WIND (KTS)|~@ HGHT AND WIND!0
l
ru


restore ${USHgempak}/restore/250mb_hght_wnd.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HEIGHTS, ISOTACHS AND WIND (KTS)|~@ HGHT AND WIND!0
l
ru


restore ${USHgempak}/restore/300mb_hght_wnd.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HEIGHTS, ISOTACHS AND WIND (KTS)|~@ HGHT AND WIND!0
l
ru


GLEVEL  = 4400:10000
GVCORD  = sgma
GDPFUN  = sm5s(relh)
CINT    = 10
LINE    = 21/1/2
TITLE	= 5/-2/~ ? $m_title MEAN RELATIVE HUMIDITY|~1000-440 MB MEAN RH!0
SCALE   = 0
FINT    = 10;70;90
FLINE   = 20;0;22;23
TYPE    = f/c
l
ru

GLEVEL  = 0
GVCORD  = none
GDPFUN  = sm5s(quo(pwtr,25.4))     !sm5s(quo(pwtr,25.4))
CINT    = 0.25/0.25/0.5            !0.25/0.75/6.0
LINE    = 22///2                   !32//2/2
TITLE	= 5/-2/~ ? $m_title PRECIPITABLE WATER (in)|~PRECIPITABLE WATER!0
SCALE   = 0
SKIP    = 0
FINT    = 0.5;1.0;1.5;2.0
FLINE   = 0;23;22;21;2
TYPE    = c      !c/f
CLRBAR  = 1/V/LL
r


GLEVEL  = 1000
GVCORD  = pres
GDPFUN  = sm5s(tmpc)   !kntv(wnd)
CINT    = 5
LINE    = 2/1/3
TITLE	= 5/-2/~ ? $m_title @ TEMPERATURE|~1000 MB TEMP!0
SCALE   = 0
SKIP    = 0
FINT    = 0;30
FLINE   = 26;0;21
TYPE    = c/f   ! b
FILTER  = Y
CONTUR  = 3
CLRBAR  = 1
r


restore ${USHgempak}/restore/precip.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
GDATTIM = F12-F192-06; F214-F384-12
GDPFUN  = (quo(p12m,25.4))
TITLE   = 5/-2/~ ? $m_title 12-HOUR TOTAL PRECIPITATION (IN)|~12-HOURLY TOTAL PCPN
hilo    = 31;0/x#2/.01-20//50;50/y!17/H#;L#/1020-1070;900-1012
hlsym   = 1.5!1;1//22;22/2;2/hw
l
ru

GDATTIM = F24-F192-06; F214-F384-12
GDPFUN  = (quo(p24m,25.4))
TITLE   = 5/-2/~ ? $m_title 24-HOUR TOTAL PRECIPITATION (IN)|~24-HOURLY TOTAL PCPN
l
ru

GDATTIM = F180
GDPFUN  = (quo(p180m,25.4))
TITLE   = 5/-2/~ ? $m_title 180-HOUR TOTAL PRECIPITATION (IN)|~180-HOURLY TOTAL PCPN
l
ru

exit
EOF
export err=$?; err_chk

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l mrfhi.meta
export err=$?;export pgm="GEMPAK CHECK FILE"; err_chk

if [ $SENDCOM = "YES" ] ; then
  mv mrfhi.meta ${COMOUT}/gfs_${PDY}_${cyc}_hi
  if [ $SENDDBN = "YES" ] ; then
    $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
     $COMOUT/gfs_${PDY}_${cyc}_hi
      if [ $DBN_ALERT_TYPE = "GFS_METAFILE_LAST" ] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
        ${COMOUT}/gfs_${PDY}_${cyc}_hi
      fi
  fi
fi

#
