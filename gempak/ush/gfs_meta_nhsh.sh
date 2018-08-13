#!/bin/sh

#
# Metafile Script : mrf_meta_nhsh
#
# Log :
# D.W.Plummer/NCEP   2/97   Add log header
# D.W.Plummer/NCEP   2/97   Added $MAPFIL=mepowo.gsf
# D.W.Plummer/NCEP   4/97   Changed SKIP for grid2
# B. Gordon          4/00   Converted for production on IBM-SP
#                           and changed gdplot_nc -> gdplot2_nc
# D. Michaud         4/16   Added logic to display different titles
#                           for parallel runs
# B. Gordon          7/02   Converted to run off the GFS due to demise
#                           of the MRF.
# J. Carr           11/04   Changed contur from 1 to a 2.
#                           Added a ? to all title/TITLE lines.
#
set -xa
mkdir -p -m 775 $DATA/mrfnhsh
cd $DATA/mrfnhsh
cp $FIXgempak/datatype.tbl datatype.tbl

PDY2=`echo $PDY | cut -c3-`

if [ "$envir" = "para" ] ; then
   export m_title="GFSP"
else
   export m_title="GFS"
fi

export pgm=gdplot2_nc; prep_step; startmsg

$GEMEXE/gdplot2_nc << EOF
\$MAPFIL=mepowo.gsf
GDFILE	= F-GFS | ${PDY2}/${cyc}00
GDATTIM	= F000-F384-12
DEVICE	= nc | Nmeta_nh
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP	= 1
CLEAR	= yes
CLRBAR  = 1

restore ${USHgempak}/restore/garea_nh.nts

restore ${USHgempak}/restore/500mb_hght_absv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw 
SKIP	= 0                  !0                  !1
SCALE	= 5                  !5                  !-1
GFUNC	= (avor(wnd))//v     !mul(v,-1)          !hght
CTYPE	= c/f                !c/f                !c
CINT	= 2/10/99            !2/10/99            !6
LINE	= 7/5/1/2            ! 7/5/1/2           ! 20/1/2/1
FINT	= 16;20;24;28;32;36;40;44
HILO	= 2;6/X;N/10-99;10-99!2;6/X;N/10-99;10-99!
TITLE	= 5//~ ? @ HEIGHTS AND VORTICITY|~ @ HGHT AND VORTICITY!
TEXT	= 1/21//hw
CLEAR	= yes
 
TITLE	= 5//~ ? $m_title @ HEIGHTS AND VORTICITY|~ @ HGHT AND VORTICITY!0
l
ru


restore ${USHgempak}/restore/garea_sh.nts

DEVICE	= nc | Nmeta_sh
TITLE	= 5//~ ? $m_title @ HEIGHTS AND VORTICITY|~ @ HGHT AND VORTICITY!0
l
ru


restore ${USHgempak}/restore/garea_nh.nts
DEVICE	= nc | Nmeta_nh

restore ${USHgempak}/restore/250mb_hght_wnd.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
GDPFUN  = knts((mag(wnd)))            !sm9s(hght)
TITLE	= 5/-2/~ ? $m_title @ HEIGHTS, ISOTACHS AND WIND (KTS)|~ @ HGHT AND WIND!0
l
ru


restore ${USHgempak}/restore/garea_sh.nts
DEVICE	= nc | Nmeta_sh
ru

restore ${USHgempak}/restore/precip.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
GDATTIM = F12-F240-12
GDPFUN   = (quo(mul(pr12,43200),25.4))
GDPFUN   = (quo(p12m,25.4))
TITLE   = 5//~ ? $m_title 12-HOUR TOTAL PRECIPITATION (IN)|~ 12-HOURLY TOTAL PCPN
l
r

restore ${USHgempak}/restore/garea_sh.nts
DEVICE	= nc | Nmeta_sh
ru

exit
EOF
export err=$?; err_chk

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l Nmeta_nh
export err=$?;export pgm="GEMPAK CHECK FILE"; err_chk
ls -l Nmeta_sh
export err=$?;export pgm="GEMPAK CHECK FILE"; err_chk

if [ $SENDCOM = "YES" ] ; then
  mv Nmeta_nh ${COMOUT}/gfs_${PDY}_${cyc}_nh
  mv Nmeta_sh ${COMOUT}/gfs_${PDY}_${cyc}_sh
  if [ $SENDDBN = "YES" ] ; then
    $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
     $COMOUT/gfs_${PDY}_${cyc}_nh
    $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
     $COMOUT/gfs_${PDY}_${cyc}_sh
      if [ $DBN_ALERT_TYPE = "GFS_METAFILE_LAST" ] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
         $COMOUT/gfs_${PDY}_${cyc}_nh
        $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
         $COMOUT/gfs_${PDY}_${cyc}_sh
      fi
  fi
fi

#
