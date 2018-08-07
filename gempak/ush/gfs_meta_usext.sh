#!/bin/sh
#
# Metafile Script : gfs_meta_usext.sh
#
# Log :
# D.W.Plummer/NCEP   2/97   Add log header
# D.W.Plummer/NCEP   3/97   Add ecmwf comparison.
# D.W.Plummer/NCEP   3/97   Added $MAPFIL specification for lower resolution
# D.W.Plummer/NCEP   4/97   Changed SKIP for grid2
# D.W.Plummer/NCEP   4/97   Changed gdplot to gdplot2 and related restore files
# D.W.Plummer/NCEP   4/97   Changed NAOP to NAOP w/ .us suffix
# D.W.Plummer/NCEP   1/98   Added 12hr 2m min and max temps out to day 6 and 7
# J.L.Partain/MPC    8/98   Added latlon lines
# J. Carr/HPC        2/99   Changed skip to a 0
# J. Carr/HPC        4/2000 Changed the Alaska 5-day pcpn to a 3-5 day pcpn
#                           Added other pcpn products for the medium range fcstrs.
# B. Gordon/NCO      4/00   Converted to run as production on the IBM-SP
# D. Michaud/NCO     4/01   Added logic to display different title for parallel
#                           runs.
# B. Gordon          7/02   Converted to run off the GFS due to demise
#                           of the MRF.
# J. Carr/PMB       11/04   Added a ? to title lines.
#                           Changed contur from a 1 to a 2.
#                           Changed increment in gdattim to every 6 hrs instead of 12.
#                           Added 3 new products for HPC medium range. (2 48-hr qpf and 1 5 day qpf)
# M. Klein/HPC      01/10   Add boundary layer winds/isotachs to the metafile for CPC.
#
set -xa
mkdir -p -m 775 $DATA/mrfus
cd $DATA/mrfus
cp $FIXgempak/datatype.tbl datatype.tbl

device="nc | mrf.meta"

#XXW cp $FIXgempak/model/gfs/ak_sfstns.tbl alaska.tbl
cp $FIXgempak/ak_sfstns.tbl alaska.tbl

month=`echo $PDY | cut -c5,6`
if [ $month -ge 5 -a $month -le 9 ] ; then
#  fint="40;45;50;55;60;65;70;75;80;85;90;95;100"
#  fline="26;25;24;23;22;21;20;19;18;17;16;15;14;31"
  fint="60;65;70;75;80;85;90;95;100;105;110;115;120"
  fline="26;25;24;23;22;21;20;19;18;17;16;15;14;31"
else
  fint="-5;0;5;10;15;20;25;30;35;40;45;50;55;60;65;70;75;80"
  fline="4;30;29;28;27;26;25;24;23;22;21;20;19;18;17;16;15;14;31"
fi

PDY2=`echo $PDY | cut -c3-`

if [ "$envir" = "para" ] ; then
   export m_title="GFSP"
else
   export m_title="GFS"
fi

export pgm=gdplot2_nc; prep_step; startmsg

$GEMEXE/gdplot2_nc << EOF
GDFILE	= F-GFS | ${PDY2}/${cyc}00
GDATTIM	= F000-F384-06
DEVICE	= ${device}
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP     = 1
CLEAR	= yes
CLRBAR  = 1

GAREA	= 17.529;-129.296;53.771;-22.374
PROJ	= str/90;-105;0
LATLON	= 18/2

restore ${USHgempak}/restore/pmsl_thkn.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? $m_title PMSL, 1000-500 MB THICKNESS|~MSLP, 1000-500 THKN!0
l
run

restore ${USHgempak}/restore/850mb_hght_tmpc.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? $m_title @ HGT, TEMPERATURE AND WIND (KTS)|~@ HGT, TMP, WIND!0
l
run

restore ${USHgempak}/restore/700mb_hght_relh_omeg.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? $m_title @ HGT, REL HUMIDITY AND OMEGA|~@ HGT, RH AND OMEGA!0
l
run

restore ${USHgempak}/restore/500mb_hght_absv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? $m_title @ HGT AND VORTICITY|~@ HGT AND VORTICITY!0
l
run

restore ${USHgempak}/restore/500mb_hght_gabsv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? $m_title @ HGT AND GEO ABS VORT|~@ HGT, GEO ABS VORT!0
l
run

restore ${USHgempak}/restore/250mb_hght_wnd.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? $m_title @ HGT, ISOTACHS AND WIND (KTS)|~@ HGT AND WIND!0
l
run

GLEVEL  = 9950
GVCORD  = SGMA
PANEL   = 0
SKIP    = 0
SCALE   = 0
GDPFUN  = mag(kntv(wnd)) !kntv(wnd)
TYPE    = c/f            !b
CONTUR  = 2
CINT    = 20;30;40
LINE    = 27/4/2/1
FINT    = 20;30;40
FLINE   = 0;25;24;29
HILO    =
HLSYM   =
CLRBAR  = 1/V/LL!0
WIND    = bk18/0.8/1/112
REFVEC  =
TITLE   = 1/-2/~ ? $m_title BOUNDARY LAYER WINDS (KTS) AND ISOTACHS|~BL WIND, ISOTACHS !0
TEXT    = 1/21//hw
CLEAR   = YES
l
run

restore ${USHgempak}/restore/precip.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
HILO    = 31;0/x#2/.25-10///y
HLSYM   = 1.5
GDATTIM	= F12-F384-6
GDPFUN  = p12i
TITLE	= 1/-2/~ ? $m_title 12-HR TOTAL PCPN (IN)|~12-HR TOTAL PCPN
l
run

GDATTIM	= F24-F384-6
GDPFUN  = p24i
TITLE	= 5/-2/~ ? $m_title 24-HR TOTAL PCPN (IN)|~24-HR TOTAL PCPN
l
run

GDATTIM	= F84
wind    = bk0
gvcord  = none
type    = f
cint    = 
line    = 
clrbar  = 1/V/LL
fint    = .01;.1;.25;.5;.75;1;1.5;2;2.5;3;4;5;6;7;8;9;10
fline   = 0;21-30;14-20;5
glevel  = 0
scale   = 0
gdpfun  = p72i
refvec  = 
title   = 1/-2/~ ? $m_title 3-day (F12-F84) PCPN|~DAY 1-3 (F12-F84) PCPN
l
run

GDATTIM = F108
gdpfun  = p96i
title   = 1/-2/~ ? $m_title 4-day (F12-F120) PCPN|~DAY 1-4 (F12-F108) PCPN
l
run

GDATTIM = F132
gdpfun  = p120i 
title   = 1/-2/~ ? $m_title 5-day (F12-F132) PCPN|~DAY 1-5 (F12-F132) PCPN
l
run

GDATTIM = F132
gdpfun  = p48i
title   = 1/-2/~ ? $m_title 2-day (F84-F132) PCPN|~DAY 4-5 (F84-F132) PCPN
l
run

! NEW

GDATTIM = F126
gdpfun  = p120i
title   = 1/-2/~ ? $m_title 5-day (F06-F126) PCPN|~DAY 1-5 (F06-F126) PCPN
l
run

! NEW

GDATTIM = F126
gdpfun  = p48i
title   = 1/-2/~ ? $m_title 2-day (F78-F126) PCPN|~DAY 4-5 (F78-F126) PCPN
l
run

! NEW

GDATTIM = F138
gdpfun  = p48i
title   = 1/-2/~ ? $m_title 2-day (F90-F138) PCPN|~DAY 4-5 (F90-F138) PCPN
l
run

GDATTIM = F156
gdpfun  = p72i
title   = 1/-2/~ ? $m_title 3-day (F84-F156) PCPN|~DAY 4-6 (F84-F156) PCPN
l
run

\$MAPFIL=hipowo.cia
GAREA   = 49;173;72;-122
PROJ    = mer//3;3;0;1
STNPLT  = 5|5/1//2|alaska.tbl
gdattim = f144
gdpfun  = p72i
title   = 1/-2/~ ? $m_title 3-day (F72-F144) PCPN|~AK 3-DAY(F72-F144) PCPN
l
run

STNPLT  =

exit
EOF
export err=$?; err_chk

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l mrf.meta
export err=$?;export pgm="GEMPAK CHECK FILE"; err_chk

if [ $SENDCOM = "YES" ] ; then
  mv mrf.meta ${COMOUT}/gfs_${PDY}_${cyc}_usext
  if [ $SENDDBN = "YES" ] ; then
    $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
     $COMOUT/gfs_${PDY}_${cyc}_usext
      if [ $DBN_ALERT_TYPE = "GFS_METAFILE_LAST" ] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
        ${COMOUT}/gfs_${PDY}_${cyc}_usext
      fi
  fi
fi

#

