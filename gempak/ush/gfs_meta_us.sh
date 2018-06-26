#!/bin/sh
#
# Metafile Script : gfs_meta_us.sh
#
# Log :
# D.W.Plummer/NCEP   2/97   Add log header
# D.W.Plummer/NCEP   3/97   Added ecmwf comparison.
# D.W.Plummer/NCEP   3/97   Added $MAPFIL specification for lower resolution
# D.W.Plummer/NCEP   4/97   Removed run from 3-HOURLY PRECIP
# J. Carr/HPC        2/99   Changed skip to 0
# B. Gordon/NCO      5/00   Modified for production on IBM-SP
#                           Changed gdplot_nc -> gdplot2_nc
# D. Michaud/NCO     4/01   Modified to Reflect Different Title for
#                           Parallel runs
# J. Carr/PMB       11/04   Added a ? to all title lines
#                           Changed contur from a 1 to a 2.
#
cd $DATA

set -xa

device="nc | gfs.meta"
PDY2=`echo $PDY | cut -c3-`

export fend=F216

if [ "$envir" = "para" ] ; then
   export m_title="GFSP"
else
   export m_title="GFS"
fi

export pgm=gdplot2_nc;. prep_step
startmsg
$GEMEXE/gdplot2_nc << EOF
GDFILE	= F-GFS | ${PDY2}/${cyc}00
GDATTIM	= F00-$fend-6
DEVICE	= ${device}
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP     = 1
CLEAR	= yes
CLRBAR  = 1
SKIP    = 0

GAREA	= 17.529;-129.296;53.771;-22.374
PROJ	= str/90;-105;0
LATLON	= 0

restore $USHgempak/restore/pmsl_thkn.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title PMSL, 1000-500 MB THICKNESS|~MSLP, 1000-500 THKN!0
l
run


restore $USHgempak/restore/850mb_hght_tmpc.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HGT, TEMPERATURE AND WIND (KTS)|~@ HGT, TMP, WIND!0
l
run


restore $USHgempak/restore/700mb_hght_relh_omeg.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HGT, REL HUMIDITY AND OMEGA|~@ HGT, RH AND OMEGA!0
l
run


restore $USHgempak/restore/500mb_hght_absv.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HGT AND VORTICITY|~@ HGT AND VORTICITY!0
l
run


restore $USHgempak/restore/250mb_hght_wnd.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HGT, ISOTACHS AND WIND (KTS)|~@ HGT AND WIND!0
l
run

 
restore $USHgempak/restore/p06m_pmsl.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
GDATTIM	= F06-$fend-6
TITLE	= 5/-2/~ ? $m_title 6-HR TOTAL PCPN, MSLP|~6-HR TOTAL PCPN, MSLP!0
l
run

HILO    = 31;0/x#2////y
HLSYM   = 1.5

GDATTIM	= F12-$fend-06
GDPFUN   = p12i                                                
TITLE	= 5/-2/~ ? $m_title 12-HR TOTAL PCPN (IN)|~12-HR TOTAL PCPN
l
run


GDATTIM	= F24-$fend-06
GDPFUN   = p24i                                                
TITLE	= 5/-2/~ ? $m_title 24-HR TOTAL PCPN (IN)|~24-HR TOTAL PCPN
l
run


GDATTIM	= F72;f78;f84
GDPFUN   = p72i                                                
TITLE	= 5/-2/~ ? $m_title 72-HR TOTAL PCPN(IN)|~72-HR TOTAL PCPN
l
run


GAREA   = 26.52;-119.70;50.21;-90.42
PROJ    = str/90;-105;0/3;3;0;1
MAP     = 1//2
GDATTIM	= F24-$fend-06
GDPFUN  = p24i                                                
TITLE	= 5/-2/~ ? $m_title 24-HR TOTAL PCPN (IN)|~WEST: 24-HR PCPN
l


GAREA   = 24.57;-100.55;47.20;-65.42
PROJ    = str/90;-90;0/3;3;0;1
MAP     = 1//2
GDATTIM	= F24-$fend-06
GDPFUN  = p24i                                                
TITLE	= 5/-2/~ ? $m_title 24-HR TOTAL PCPN (IN)|~EAST: 24-HR PCPN
l

exit
EOF
export err=$?;err_chk

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l gfs.meta
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk


if [ $SENDCOM = "YES" ] ; then
  mv gfs.meta ${COMOUT}/gfs_${PDY}_${cyc}_us
  if [ $SENDDBN = "YES" ] ; then
    $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
     $COMOUT/gfs_${PDY}_${cyc}_us
    if [ $DBN_ALERT_TYPE = "GFS_METAFILE_LAST" ] ; then
      DBN_ALERT_TYPE=GFS_METAFILE
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
       ${COMOUT}/gfs_${PDY}_${cyc}_us
    fi
    if [ $fhr -eq 216 ] ; then
     ${DBNROOT}/bin/dbn_alert MODEL GFS_METAFILE_LAST $job \
       ${COMOUT}/gfs_${PDY}_${cyc}_us
    fi
  fi
fi

